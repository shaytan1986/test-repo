#!/usr/bin/env python3
"""
doc_crawler.py — Crawl technical documentation and emit RAG-ready chunks.

Crawls all pages reachable from a starting URL (same host + path prefix),
extracts the main text content, splits it into overlapping chunks, and
writes one JSON object per chunk to a JSONL file.

Output schema (one JSON object per line):
    {
      "id":          "<url-hash>-<chunk-index>",
      "url":         "https://...",
      "title":       "Page title",
      "content":     "Chunk text ...",
      "chunk_index": 0,
      "metadata":    {"total_chunks": 3}
    }

Usage:
    pip install requests beautifulsoup4 lxml
    python doc_crawler.py https://docs.example.com/
    python doc_crawler.py https://docs.example.com/ --output chunks.jsonl --max-pages 500
"""

import argparse
import hashlib
import json
import re
import sys
import time
from collections import deque
from dataclasses import asdict, dataclass, field
from urllib.parse import urljoin, urlparse
from urllib.robotparser import RobotFileParser

import requests
from bs4 import BeautifulSoup, Tag


# ---------------------------------------------------------------------------
# Data model
# ---------------------------------------------------------------------------

@dataclass
class Document:
    id: str
    url: str
    title: str
    content: str
    chunk_index: int
    metadata: dict = field(default_factory=dict)


# ---------------------------------------------------------------------------
# HTML → clean text
# ---------------------------------------------------------------------------

# Tags that are purely chrome / boilerplate
_NOISE_TAGS = {"nav", "header", "footer", "aside", "script", "style", "noscript"}

# Class-name fragments that usually indicate navigation / sidebar noise
_NOISE_CLASSES = {
    "sidebar", "nav", "toc", "menu", "breadcrumb",
    "footer", "header", "banner", "cookie", "announcement",
}


def _is_noisy(tag: Tag) -> bool:
    if tag.name in _NOISE_TAGS:
        return True
    classes = " ".join(tag.get("class", [])).lower()
    return any(c in classes for c in _NOISE_CLASSES)


def extract_text(soup: BeautifulSoup) -> tuple[str, str]:
    """Return (title, cleaned_body_text) from a parsed page."""
    title_tag = soup.find("title")
    h1 = soup.find("h1")
    title = (
        (h1.get_text(strip=True) if h1 else None)
        or (title_tag.get_text(strip=True) if title_tag else "")
    )

    # Prefer a semantic main-content container over the full <body>
    main = (
        soup.find("main")
        or soup.find("article")
        or soup.find(attrs={"role": "main"})
        or soup.find(id=re.compile(r"content|main|docs?", re.I))
        or soup.find("div", class_=re.compile(r"content|main|docs?|body", re.I))
        or soup.body
    )
    if main is None:
        return title, ""

    # Work on a copy so we can strip noise without mutating the original
    main = BeautifulSoup(str(main), "lxml")
    for tag in main.find_all(True):
        if _is_noisy(tag):
            tag.decompose()

    text = main.get_text(separator="\n")
    text = re.sub(r"\n{3,}", "\n\n", text)   # collapse blank lines
    text = re.sub(r"[ \t]+", " ", text)       # collapse horizontal whitespace
    return title, text.strip()


# ---------------------------------------------------------------------------
# Chunking
# ---------------------------------------------------------------------------

def chunk_text(text: str, chunk_size: int = 1000, overlap: int = 200) -> list[str]:
    """
    Split *text* into chunks of at most *chunk_size* characters.

    Splits prefer paragraph boundaries; consecutive chunks share *overlap*
    characters so that no sentence is cut off cold at a boundary.
    """
    paragraphs = [p.strip() for p in re.split(r"\n\n+", text) if p.strip()]
    chunks: list[str] = []
    current: list[str] = []
    current_len = 0

    for para in paragraphs:
        para_len = len(para)
        if current_len + para_len > chunk_size and current:
            chunks.append("\n\n".join(current))
            # Seed the next chunk with trailing text for overlap
            tail = "\n\n".join(current)[-overlap:]
            current = [tail] if tail else []
            current_len = len(tail)
        current.append(para)
        current_len += para_len + 2  # +2 for the "\n\n" separator

    if current:
        chunks.append("\n\n".join(current))

    return chunks or [text]  # never return an empty list


# ---------------------------------------------------------------------------
# Link helpers
# ---------------------------------------------------------------------------

def same_scope(base_url: str, candidate: str) -> bool:
    """True if *candidate* lives under the same host + path prefix as *base_url*."""
    base = urlparse(base_url)
    target = urlparse(candidate)
    return (
        target.scheme in ("http", "https")
        and target.netloc == base.netloc
        and target.path.startswith(base.path.rstrip("/"))
    )


def extract_links(soup: BeautifulSoup, page_url: str) -> list[str]:
    links = []
    for a in soup.find_all("a", href=True):
        href = a["href"].split("#")[0]   # drop fragment
        if not href:
            continue
        full = urljoin(page_url, href)
        parsed = urlparse(full)
        # Strip query strings — docs sites rarely need them for page identity
        clean = parsed._replace(query="", fragment="").geturl()
        links.append(clean)
    return links


# ---------------------------------------------------------------------------
# Robots.txt
# ---------------------------------------------------------------------------

def build_robot_parser(start_url: str, session: requests.Session) -> RobotFileParser:
    parsed = urlparse(start_url)
    robots_url = f"{parsed.scheme}://{parsed.netloc}/robots.txt"
    rp = RobotFileParser()
    rp.set_url(robots_url)
    try:
        resp = session.get(robots_url, timeout=10)
        rp.parse(resp.text.splitlines())
    except Exception:
        pass  # unreachable robots.txt → allow everything
    return rp


# ---------------------------------------------------------------------------
# Crawler
# ---------------------------------------------------------------------------

def crawl(
    start_url: str,
    *,
    max_pages: int = 200,
    delay: float = 0.5,
    chunk_size: int = 1000,
    chunk_overlap: int = 200,
) -> list[Document]:
    """
    BFS crawl from *start_url*, staying within the same host + path prefix.

    Returns a list of :class:`Document` objects, one per text chunk.
    """
    session = requests.Session()
    session.headers["User-Agent"] = (
        "DocCrawler/1.0 (RAG pipeline; respects robots.txt)"
    )

    robot_parser = build_robot_parser(start_url, session)
    visited: set[str] = set()
    queue: deque[str] = deque([start_url])
    documents: list[Document] = []

    print(f"Crawling {start_url}  (max {max_pages} pages)", file=sys.stderr)

    while queue and len(visited) < max_pages:
        url = queue.popleft()
        if url in visited:
            continue
        visited.add(url)

        if not robot_parser.can_fetch("*", url):
            print(f"  [robots] skip  {url}", file=sys.stderr)
            continue

        # ── Fetch ──────────────────────────────────────────────────────────
        try:
            resp = session.get(url, timeout=15)
            resp.raise_for_status()
        except requests.RequestException as exc:
            print(f"  [error]        {url}: {exc}", file=sys.stderr)
            continue

        if "html" not in resp.headers.get("content-type", ""):
            continue  # skip PDFs, ZIPs, etc.

        # ── Parse + chunk ──────────────────────────────────────────────────
        soup = BeautifulSoup(resp.text, "lxml")
        title, text = extract_text(soup)

        if text:
            chunks = chunk_text(text, chunk_size=chunk_size, overlap=chunk_overlap)
            url_hash = hashlib.md5(url.encode()).hexdigest()[:8]
            for i, chunk in enumerate(chunks):
                documents.append(Document(
                    id=f"{url_hash}-{i}",
                    url=url,
                    title=title,
                    content=chunk,
                    chunk_index=i,
                    metadata={"total_chunks": len(chunks)},
                ))
            print(f"  [ok]    {len(chunks):>3} chunk(s)  {url}", file=sys.stderr)
        else:
            print(f"  [empty]        {url}", file=sys.stderr)

        # ── Enqueue in-scope links ─────────────────────────────────────────
        for link in extract_links(soup, url):
            if link not in visited and same_scope(start_url, link):
                queue.append(link)

        time.sleep(delay)

    print(
        f"\nDone — pages visited: {len(visited)}, chunks produced: {len(documents)}",
        file=sys.stderr,
    )
    return documents


# ---------------------------------------------------------------------------
# CLI
# ---------------------------------------------------------------------------

def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("url", help="Starting URL, e.g. https://docs.example.com/guide/")
    parser.add_argument(
        "--output", default="docs.jsonl",
        help="Output JSONL file (default: docs.jsonl)",
    )
    parser.add_argument(
        "--max-pages", type=int, default=200,
        help="Maximum number of pages to crawl (default: 200)",
    )
    parser.add_argument(
        "--chunk-size", type=int, default=1000,
        help="Target characters per chunk (default: 1000)",
    )
    parser.add_argument(
        "--chunk-overlap", type=int, default=200,
        help="Overlap characters between adjacent chunks (default: 200)",
    )
    parser.add_argument(
        "--delay", type=float, default=0.5,
        help="Seconds to wait between requests (default: 0.5)",
    )
    args = parser.parse_args()

    docs = crawl(
        args.url,
        max_pages=args.max_pages,
        delay=args.delay,
        chunk_size=args.chunk_size,
        chunk_overlap=args.chunk_overlap,
    )

    with open(args.output, "w", encoding="utf-8") as fh:
        for doc in docs:
            fh.write(json.dumps(asdict(doc), ensure_ascii=False) + "\n")

    print(f"Wrote {len(docs)} chunks → {args.output}")


if __name__ == "__main__":
    main()
