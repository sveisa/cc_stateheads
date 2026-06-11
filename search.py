"""
search.py — Query the FAISS index and print top results.

Usage:
    python search.py "how do markets make emotions"
    python search.py "race and inequality" --top 10
"""

import argparse
import pickle
import textwrap

import faiss
import numpy as np
from sentence_transformers import SentenceTransformer

INDEX_PATH = "index.faiss"
PAPERS_PATH = "papers.pkl"
MODEL_NAME = "all-MiniLM-L6-v2"
SNIPPET_LEN = 300


def load_artifacts():
    index = faiss.read_index(INDEX_PATH)
    with open(PAPERS_PATH, "rb") as f:
        metadata = pickle.load(f)
    return index, metadata


def search(query: str, top_k: int = 20) -> list[dict]:
    index, metadata = load_artifacts()
    model = SentenceTransformer(MODEL_NAME)

    vec = model.encode([query], convert_to_numpy=True).astype("float32")
    faiss.normalize_L2(vec)

    scores, indices = index.search(vec, top_k)

    results = []
    for score, idx in zip(scores[0], indices[0]):
        if idx < 0:
            continue
        row = metadata[idx].copy()
        row["score"] = float(score)
        results.append(row)
    return results


def print_results(results: list[dict]):
    for rank, r in enumerate(results, 1):
        score = r.get("score", 0)
        title = r.get("title", "Untitled")
        authors = r.get("authors", "")
        year = r.get("year", "")
        journal = r.get("journal", "")
        doi = r.get("doi", "")
        abstract = r.get("abstract", "")

        snippet = textwrap.shorten(abstract, width=SNIPPET_LEN, placeholder="...")

        print(f"\n{'─'*70}")
        print(f"#{rank:>2}  Score: {score:.4f}")
        print(f"     {title}")
        if authors or year:
            print(f"     {authors}  ({year})")
        if journal:
            print(f"     {journal}")
        if doi:
            print(f"     {doi}")
        print(f"     {snippet}")
    print(f"\n{'─'*70}")


def main():
    parser = argparse.ArgumentParser(description="Semantic search over sociology papers")
    parser.add_argument("query", help="Research question or keyword string")
    parser.add_argument("--top", type=int, default=20, metavar="N",
                        help="Number of results to return (default: 20)")
    args = parser.parse_args()

    print(f"Query: \"{args.query}\"  (top {args.top})\n")
    results = search(args.query, top_k=args.top)
    print_results(results)


if __name__ == "__main__":
    main()
