import requests
import csv
import time
import sys

MAILTO = "isak.ladegaard@gmail.com"
BASE_URL = "https://api.openalex.org"
OUTPUT_FILE = "/home/user/cc_stateheads/sociology_papers.csv"

JOURNALS = [
    "American Sociological Review",
    "American Journal of Sociology",
    "Social Problems",
    "Social Forces",
    "Socio-Economic Review",
    "British Journal of Sociology",
    "Theory & Society",
]

def get(url, params=None, retries=5):
    if params is None:
        params = {}
    params["mailto"] = MAILTO
    for attempt in range(retries):
        try:
            r = requests.get(url, params=params, timeout=30)
            if r.status_code == 429:
                print("  Rate limited, sleeping 10s...")
                time.sleep(10)
                continue
            r.raise_for_status()
            return r.json()
        except Exception as e:
            print(f"  Error (attempt {attempt+1}): {e}")
            time.sleep(2 ** attempt)
    raise RuntimeError(f"Failed after {retries} attempts: {url}")

def find_source_id(journal_name):
    data = get(f"{BASE_URL}/sources", {"search": journal_name, "per_page": 5})
    results = data.get("results", [])
    if not results:
        print(f"  WARNING: No source found for '{journal_name}'")
        return None
    # Pick best match (first result)
    for r in results:
        print(f"  Candidate: {r['display_name']} | id={r['id']} | works_count={r.get('works_count',0)}")
    chosen = results[0]
    print(f"  -> Using: {chosen['display_name']} ({chosen['id']})")
    return chosen["id"].replace("https://openalex.org/", "")

def decode_abstract(inverted_index):
    if not inverted_index:
        return ""
    positions = []
    for word, pos_list in inverted_index.items():
        for p in pos_list:
            positions.append((p, word))
    positions.sort()
    return " ".join(w for _, w in positions)

def fetch_works(source_id, journal_name):
    filter_str = f"primary_location.source.id:{source_id},from_publication_date:1990-01-01,type:article"
    params = {
        "filter": filter_str,
        "per_page": 200,
        "cursor": "*",
        "select": "title,authorships,publication_year,doi,abstract_inverted_index,primary_location",
    }
    papers = []
    page = 0
    while True:
        data = get(f"{BASE_URL}/works", params)
        results = data.get("results", [])
        meta = data.get("meta", {})
        page += 1
        if page == 1:
            print(f"  Total works reported: {meta.get('count', '?')}")

        for work in results:
            title = work.get("title") or ""
            # Skip book reviews by title
            if "book review" in title.lower():
                continue

            # Authors
            authors = "; ".join(
                a.get("author", {}).get("display_name", "") or ""
                for a in (work.get("authorships") or [])
            )

            year = work.get("publication_year", "")
            doi = work.get("doi") or ""
            abstract = decode_abstract(work.get("abstract_inverted_index"))

            papers.append({
                "title": title,
                "authors": authors,
                "year": year,
                "journal": journal_name,
                "doi": doi,
                "abstract": abstract,
            })

        next_cursor = meta.get("next_cursor")
        print(f"  Page {page}: fetched {len(results)} works, cumulative={len(papers)}", flush=True)

        if not next_cursor or not results:
            break

        params["cursor"] = next_cursor
        time.sleep(0.12)  # ~8 req/sec, polite pool

    return papers

def main():
    all_papers = []

    # Step 1: Find source IDs
    print("=== Finding journal source IDs ===")
    journal_sources = {}
    for jname in JOURNALS:
        print(f"\nSearching: {jname}")
        sid = find_source_id(jname)
        if sid:
            journal_sources[jname] = sid
        time.sleep(0.5)

    print(f"\nFound {len(journal_sources)}/{len(JOURNALS)} journals")

    # Step 2: Fetch papers
    print("\n=== Fetching papers ===")
    for jname, sid in journal_sources.items():
        print(f"\n--- {jname} (source: {sid}) ---")
        papers = fetch_works(sid, jname)
        print(f"  Collected {len(papers)} papers from {jname}")
        all_papers.extend(papers)

    # Step 3: Write CSV
    print(f"\n=== Writing {len(all_papers)} papers to CSV ===")
    fieldnames = ["title", "authors", "year", "journal", "doi", "abstract"]
    with open(OUTPUT_FILE, "w", newline="", encoding="utf-8") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(all_papers)

    print(f"Done! Total rows written: {len(all_papers)}")
    print(f"Output: {OUTPUT_FILE}")

if __name__ == "__main__":
    main()
