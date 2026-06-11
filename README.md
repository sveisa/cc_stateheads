# Semantic Search — Climate Speech Paragraphs

Semantic search over ~2,700 political speech paragraphs about climate change, using sentence embeddings and FAISS.

## Install

```bash
pip install sentence-transformers faiss-cpu pandas numpy
```

## Build the index (one-time)

```bash
python embed.py
```

Produces two files:
- `index.faiss` — FAISS vector index (cosine similarity)
- `papers.pkl` — metadata for each paragraph

## Search

```bash
python search.py "renewable energy transition"
python search.py "climate crisis and migration" --top 10
```

Each result shows rank, similarity score, speech ID, country, year, topic, summary, and a text snippet.

## Data

Source CSV: `overview_cleaned2.csv`  
Key columns used:
| Column | Role |
|---|---|
| `paragraph_text` | Embedded text |
| `paragraph_summary` | Shown in results |
| `speech_id` / `docname` | Identifier |
| `year`, `country` | Shown in results |
| `topic`, `is_crisis` | Shown in results |

## Model

`all-MiniLM-L6-v2` via `sentence-transformers` — fast, 384-dim embeddings, good for semantic similarity.
