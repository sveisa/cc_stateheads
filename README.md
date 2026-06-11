---
title: Sociology Search
emoji: 📚
colorFrom: blue
colorTo: indigo
sdk: gradio
sdk_version: "5.0.0"
python_version: "3.10"
app_file: app.py
pinned: false
---

# Semantic Search — Sociology Papers

Semantic search over ~24,000 sociology paper titles and abstracts, using sentence embeddings and FAISS.

## Install

```bash
pip install sentence-transformers faiss-cpu pandas numpy
```

## Build the index (one-time)

Place `sociology_papers.csv` (tab-separated) in the same directory, then run:

```bash
python embed.py
```

This produces two files:
- `index.faiss` — FAISS vector index (cosine similarity, 384 dimensions)
- `papers.pkl` — metadata for each paper (title, authors, year, journal, doi, abstract)

Takes a few minutes for ~24,000 papers.

## Search

```bash
python search.py "how do markets make emotions"
python search.py "race and inequality in education" --top 10
```

Each result shows rank, similarity score, title, authors, year, journal, DOI, and an abstract snippet.

## Data

Source CSV: `sociology_papers.csv` (tab-separated)

| Column | Role |
|---|---|
| `title` | Embedded (combined with abstract) |
| `abstract` | Embedded; shown as snippet in results |
| `authors` | Shown in results |
| `year` | Shown in results |
| `journal` | Shown in results |
| `doi` | Shown in results |

## Model

`all-MiniLM-L6-v2` via `sentence-transformers` — fast, 384-dim embeddings, strong semantic similarity performance.
