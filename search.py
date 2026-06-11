"""
search.py — Query the FAISS index and print top results.

Usage:
    python search.py "how do markets make emotions"
    python search.py "renewable energy policy" --top 10
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
        speech = r.get("speech_id", r.get("docname", "?"))
        year = r.get("year", "?")
        country = r.get("country", "?")
        topic = r.get("topic", "")
        is_crisis = r.get("is_crisis", "")
        summary = r.get("paragraph_summary", "")
        text = r.get("paragraph_text", "")

        snippet = textwrap.shorten(text, width=SNIPPET_LEN, placeholder="...")

        print(f"\n{'─'*70}")
        print(f"#{rank:>2}  Score: {score:.4f}  |  {speech}  [{country}, {year}]")
        if topic:
            print(f"     Topic: {topic}  |  Crisis: {is_crisis}")
        if summary:
            print(f"     Summary: {summary}")
        print(f"     Text: {snippet}")
    print(f"\n{'─'*70}")


def main():
    parser = argparse.ArgumentParser(description="Semantic search over speech paragraphs")
    parser.add_argument("query", help="Search query string")
    parser.add_argument("--top", type=int, default=20, metavar="N",
                        help="Number of results to return (default: 20)")
    args = parser.parse_args()

    print(f"Query: "{args.query}"  (top {args.top})\n")
    results = search(args.query, top_k=args.top)
    print_results(results)


if __name__ == "__main__":
    main()
