"""
embed.py — Load CSV, embed paragraph_text, save FAISS index and metadata.
"""

import pickle
import sys

import faiss
import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer

CSV_PATH = "overview_cleaned2.csv"
INDEX_PATH = "index.faiss"
PAPERS_PATH = "papers.pkl"
MODEL_NAME = "all-MiniLM-L6-v2"
BATCH_SIZE = 256


def main():
    print(f"Loading CSV: {CSV_PATH}")
    df = pd.read_csv(CSV_PATH)
    print(f"Rows: {len(df)}")

    # Drop rows with missing paragraph_text
    df = df.dropna(subset=["paragraph_text"]).reset_index(drop=True)
    print(f"Rows after dropping empty paragraph_text: {len(df)}")

    texts = df["paragraph_text"].tolist()

    print(f"Loading model: {MODEL_NAME}")
    model = SentenceTransformer(MODEL_NAME)

    embeddings = []
    for i in range(0, len(texts), BATCH_SIZE):
        batch = texts[i : i + BATCH_SIZE]
        vecs = model.encode(batch, show_progress_bar=False, convert_to_numpy=True)
        embeddings.append(vecs)
        done = min(i + BATCH_SIZE, len(texts))
        if done % 1000 < BATCH_SIZE or done == len(texts):
            print(f"  Embedded {done}/{len(texts)} rows...")

    matrix = np.vstack(embeddings).astype("float32")
    faiss.normalize_L2(matrix)

    dim = matrix.shape[1]
    index = faiss.IndexFlatIP(dim)  # inner product on normalised vecs = cosine sim
    index.add(matrix)

    faiss.write_index(index, INDEX_PATH)
    print(f"Saved FAISS index → {INDEX_PATH}  ({index.ntotal} vectors, dim={dim})")

    meta_cols = ["graf_id", "speech_id", "docname", "year", "country",
                 "paragraph_text", "paragraph_summary", "topic", "is_crisis"]
    available = [c for c in meta_cols if c in df.columns]
    metadata = df[available].to_dict(orient="records")

    with open(PAPERS_PATH, "wb") as f:
        pickle.dump(metadata, f)
    print(f"Saved metadata → {PAPERS_PATH}")


if __name__ == "__main__":
    main()
