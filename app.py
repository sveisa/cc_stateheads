import pickle
import textwrap

import faiss
import gradio as gr
import numpy as np
from sentence_transformers import SentenceTransformer

INDEX_PATH = "index.faiss"
PAPERS_PATH = "papers.pkl"
MODEL_NAME = "all-MiniLM-L6-v2"

# Load once at startup
index = faiss.read_index(INDEX_PATH)
with open(PAPERS_PATH, "rb") as f:
    metadata = pickle.load(f)
model = SentenceTransformer(MODEL_NAME)


def search(query: str, top_k: int = 20) -> str:
    if not query.strip():
        return "Enter a research question above."

    vec = model.encode([query], convert_to_numpy=True).astype("float32")
    faiss.normalize_L2(vec)
    scores, indices = index.search(vec, int(top_k))

    lines = []
    for rank, (score, idx) in enumerate(zip(scores[0], indices[0]), 1):
        if idx < 0:
            continue
        r = metadata[idx]
        title = r.get("title", "Untitled")
        authors = r.get("authors", "")
        year = r.get("year", "")
        journal = r.get("journal", "")
        doi = r.get("doi", "")
        abstract = r.get("abstract", "")
        snippet = textwrap.shorten(abstract, width=400, placeholder="…")

        doi_md = f"[{doi}]({doi})" if doi else ""
        lines.append(
            f"### {rank}. {title}\n"
            f"**Score:** {score:.4f} &nbsp;|&nbsp; "
            f"**{authors}** &nbsp;({year}) &nbsp;|&nbsp; *{journal}*\n\n"
            f"{doi_md}\n\n"
            f"{snippet}\n\n"
            f"---"
        )

    return "\n\n".join(lines) if lines else "No results found."


with gr.Blocks(title="Sociology Paper Search") as demo:
    gr.Markdown("# Sociology Paper Search\nSemantic search over ~15,000 sociology papers using sentence embeddings.")

    with gr.Row():
        query_box = gr.Textbox(
            label="Research question",
            placeholder="e.g. how do markets make emotions",
            scale=4,
        )
        top_k = gr.Slider(minimum=5, maximum=50, value=20, step=5, label="Results", scale=1)

    search_btn = gr.Button("Search", variant="primary")
    output = gr.Markdown()

    search_btn.click(fn=search, inputs=[query_box, top_k], outputs=output)
    query_box.submit(fn=search, inputs=[query_box, top_k], outputs=output)

demo.launch()
