#!/usr/bin/env python3
"""
Download Statens pensjonsfond white papers (stortingsmeldinger) from regjeringen.no.
Run this script on your LOCAL machine — regjeringen.no blocks datacenter IPs.

Requirements: pip install requests beautifulsoup4
"""
import os
import re
import time
import requests
from bs4 import BeautifulSoup

DOCS = [
    ("2006-2007", "St.meld. nr. 24", "stmeld-nr-24-2006-2007",  "https://www.regjeringen.no/no/dokumenter/stmeld-nr-24-2006-2007-/id462723/"),
    ("2007-2008", "St.meld. nr. 16", "stmeld-nr-16-2007-2008",  "https://www.regjeringen.no/no/dokumenter/stmeld-nr-16-2007-2008-/id505748/"),
    ("2008-2009", "St.meld. nr. 20", "stmeld-nr-20-2008-2009",  "https://www.regjeringen.no/no/dokumenter/stmeld-nr-20-2008-2009-/id553201/"),
    ("2009-2010", "Meld. St. 10",    "meld-st-10-2009-2010",    "https://www.regjeringen.no/no/dokumenter/Meld-St-10-2009-2010/id599137/"),
    ("2010-2011", "Meld. St. 15",    "meld-st-15-2010-2011",    "https://www.regjeringen.no/no/dokumenter/meld-st-15-20102011/id639721/"),
    ("2011-2012", "Meld. St. 17",    "meld-st-17-2011-2012",    "https://www.regjeringen.no/no/dokumenter/meld-st-17-20112012/id676409/"),
    ("2012-2013", "Meld. St. 27",    "meld-st-27-2012-2013",    "https://www.regjeringen.no/no/dokumenter/meld-st-27-20122013/id721780/"),
    ("2013-2014", "Meld. St. 19",    "meld-st-19-2013-2014",    "https://www.regjeringen.no/no/dokumenter/Meld-St-19-201320141/id754743/"),
    ("2014-2015", "Meld. St. 21",    "meld-st-21-2014-2015",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-21-2014-2015/id2405360/"),
    ("2015-2016", "Meld. St. 23",    "meld-st-23-2015-2016",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-23-20152016/id2481800/"),
    ("2016-2017", "Meld. St. 26",    "meld-st-26-2016-2017",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-26-20162017/id2545354/"),
    ("2017-2018", "Meld. St. 13",    "meld-st-13-2017-2018",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-13-20172018/id2596754/"),
    ("2018-2019a","Meld. St. 14",    "meld-st-14-2018-2019",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-14-20182019/id2631726/"),
    ("2018-2019b","Meld. St. 20",    "meld-st-20-2018-2019",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-20-20182019/id2639311/"),
    ("2019-2020", "Meld. St. 32",    "meld-st-32-2019-2020",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-32-20192020/id2741487/"),
    ("2020-2021", "Meld. St. 24",    "meld-st-24-2020-2021",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-24-20202021/id2843255/"),
    ("2021-2022", "Meld. St. 9",     "meld-st-9-2021-2022",     "https://www.regjeringen.no/no/dokumenter/meld.-st.-9-20212022/id2906344/"),
    ("2022-2023", "Meld. St. 17",    "meld-st-17-2022-2023",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-17-20222023/id2969663/"),
    ("2023-2024", "Meld. St. 22",    "meld-st-22-2023-2024",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-22-20232024/id3033198/"),
    ("2024-2025", "Meld. St. 22",    "meld-st-22-2024-2025",    "https://www.regjeringen.no/no/dokumenter/meld.-st.-22-20242025/"),
    ("2025-2026", "Meld. St. 7",     "meld-st-7-2025-2026",     "https://www.regjeringen.no/no/dokumenter/meld.-st.-7-2025-2026/id3155109/"),
]

OUT_DIR = "meldinger"
os.makedirs(OUT_DIR, exist_ok=True)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/125.0.0.0 Safari/537.36"
    ),
    "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    "Accept-Language": "nb-NO,nb;q=0.9,no;q=0.8,en;q=0.7",
    "Accept-Encoding": "gzip, deflate, br",
    "DNT": "1",
    "Connection": "keep-alive",
    "Upgrade-Insecure-Requests": "1",
}

sess = requests.Session()
sess.headers.update(HEADERS)

def find_pdf_link(html: str, base_url: str) -> str | None:
    """Parse the page HTML and return the first PDF download link, if any."""
    soup = BeautifulSoup(html, "html.parser")
    for a in soup.find_all("a", href=True):
        href = a["href"]
        if href.lower().endswith(".pdf"):
            if href.startswith("http"):
                return href
            return "https://www.regjeringen.no" + href
    # Also look for links with 'pdf' in the URL path
    for a in soup.find_all("a", href=True):
        href = a["href"]
        if "/pdfs/" in href.lower() or "pdf" in href.lower():
            if href.startswith("http"):
                return href
            return "https://www.regjeringen.no" + href
    return None

results = {"ok_pdf": [], "ok_html": [], "failed": []}

for year, title, slug, url in DOCS:
    html_path = f"{OUT_DIR}/{year}_{slug}.html"
    pdf_path  = f"{OUT_DIR}/{year}_{slug}.pdf"

    if os.path.exists(pdf_path):
        print(f"[skip] {pdf_path}")
        results["ok_pdf"].append(year)
        continue
    if os.path.exists(html_path):
        print(f"[skip] {html_path}")
        results["ok_html"].append(year)
        continue

    print(f"Fetching page {year} {title} ...", end=" ", flush=True)
    try:
        r = sess.get(url, timeout=30)
        r.raise_for_status()
    except Exception as e:
        print(f"FAILED: {e}")
        results["failed"].append((year, title, str(e)))
        time.sleep(1)
        continue

    # Try to find and download a PDF
    pdf_url = find_pdf_link(r.text, url)
    if pdf_url:
        print(f"found PDF -> {pdf_url} ...", end=" ", flush=True)
        try:
            pr = sess.get(pdf_url, timeout=60, stream=True)
            pr.raise_for_status()
            with open(pdf_path, "wb") as f:
                for chunk in pr.iter_content(chunk_size=65536):
                    f.write(chunk)
            size_kb = os.path.getsize(pdf_path) // 1024
            print(f"OK ({size_kb} KB)")
            results["ok_pdf"].append(year)
        except Exception as e:
            print(f"PDF download failed ({e}), saving HTML instead")
            with open(html_path, "w", encoding="utf-8") as f:
                f.write(r.text)
            results["ok_html"].append(year)
    else:
        # Save the HTML
        with open(html_path, "w", encoding="utf-8") as f:
            f.write(r.text)
        size_kb = len(r.text) // 1024
        print(f"OK HTML ({size_kb} KB, no PDF link found)")
        results["ok_html"].append(year)

    time.sleep(1.5)

print("\n--- Summary ---")
print(f"PDFs:  {len(results['ok_pdf'])}")
print(f"HTML:  {len(results['ok_html'])}")
print(f"Failed: {len(results['failed'])}")
if results["failed"]:
    print("Failed downloads:")
    for y, t, e in results["failed"]:
        print(f"  {y} {t}: {e}")
