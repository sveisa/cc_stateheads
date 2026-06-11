"""
embed.py — Load CSV, embed title + abstract, save FAISS index and metadata.
"""

import pickle

import faiss
import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer

CSV_PATH = "sociology_papers.csv"
INDEX_PATH = "index.faiss"
PAPERS_PATH = "papers.pkl"
MODEL_NAME = "all-MiniLM-L6-v2"
BATCH_SIZE = 256


def main():
    print(f"Loading CSV: {CSV_PATH}")
    df = pd.read_csv(CSV_PATH)
    print(f"Rows: {len(df)}")

    df = df.dropna(subset=["abstract"]).reset_index(drop=True)
    print(f"Rows after dropping empty abstracts: {len(df)}")

    # Combine title + abstract for richer embeddings
    texts = (df["title"].fillna("") + " " + df["abstract"]).tolist()

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
    index = faiss.IndexFlatIP(dim)  # inner product on L2-normalised vecs = cosine sim
    index.add(matrix)

    faiss.write_index(index, INDEX_PATH)
    print(f"Saved FAISS index → {INDEX_PATH}  ({index.ntotal} vectors, dim={dim})")

    meta_cols = ["title", "authors", "year", "journal", "doi", "abstract"]
    available = [c for c in meta_cols if c in df.columns]
    metadata = df[available].to_dict(orient="records")

    with open(PAPERS_PATH, "wb") as f:
        pickle.dump(metadata, f)
    print(f"Saved metadata → {PAPERS_PATH}")


if __name__ == "__main__":
    main()
