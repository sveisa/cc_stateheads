#!/usr/bin/env python3
"""
Check the current state on Telegram of channels TGNAV has dropped.

Reads a CSV with a `username` column (removed.csv works as-is) and fetches each
handle's public t.me page, recording whether it still resolves, its current name,
description and subscriber count, and — with --activity — the date of its most
recent post.

    pip install requests
    python3 05_check_telegram_status.py removed.csv -o removed_enriched.csv --activity

Resumes if interrupted: re-run the same command and rows already written are skipped.
Nothing here needs a Telegram account; t.me/<handle> is a public preview page.
"""
import argparse, csv, os, random, re, sys, time
import requests

UA = ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
      "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")
OG = lambda p, h: (re.search(r'<meta property="og:%s" content="([^"]*)"' % p, h) or [None, ""])[1]
EXTRA = re.compile(r'tgme_page_extra">([^<]*)<')
TIME = re.compile(r'<time datetime="([^"]+)"')
FIELDS = ["username", "name_tgnav", "verdict", "name_now", "subscribers_now",
          "kind", "last_post", "description_now", "http_status"]


def unescape(s):
    import html
    return html.unescape(s or "").strip()


def classify(handle, html_text, status):
    if status == 404:
        return "gone", "", "", "", ""
    title, desc = unescape(OG("title", html_text)), unescape(OG("description", html_text))
    extra = unescape((EXTRA.search(html_text) or [None, ""])[1])

    # t.me serves a generic contact card for handles that resolve to nothing
    if title.lower().startswith("telegram: contact") and not extra:
        return "gone or renamed", "", "", "", desc

    kind = ("channel" if "subscriber" in extra.lower() else
            "group" if "member" in extra.lower() else
            "bot" if handle.lower().endswith("bot") else "")
    # "68 039 subscribers" — Telegram uses a space as the thousands separator
    m = re.match(r"([\d\s\u202f\u00a0,.]+?)\s*(subscriber|member|photo|video|file|link)", extra, re.I)
    subs = re.sub(r"[^\d]", "", m.group(1)) if m else ""
    verdict = "live" if extra else "resolves, no counter (private or restricted)"
    return verdict, title, subs, kind, desc


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("infile")
    ap.add_argument("-o", "--out", default="removed_enriched.csv")
    ap.add_argument("--sleep", type=float, default=1.5, help="base seconds between requests")
    ap.add_argument("--activity", action="store_true",
                    help="also fetch t.me/s/<handle> for the latest post date (2x requests)")
    a = ap.parse_args()

    rows = list(csv.DictReader(open(a.infile, encoding="utf-8-sig")))
    done = set()
    if os.path.exists(a.out):
        done = {r["username"] for r in csv.DictReader(open(a.out, encoding="utf-8-sig"))}
        print(f"resuming — {len(done)} already checked", file=sys.stderr)

    new = not os.path.exists(a.out)
    with open(a.out, "a", newline="", encoding="utf-8-sig") as fh:
        w = csv.DictWriter(fh, fieldnames=FIELDS)
        if new:
            w.writeheader()
        s = requests.Session()
        s.headers["User-Agent"] = UA
        todo = [r for r in rows if r["username"] not in done]
        for i, r in enumerate(todo, 1):
            h = r["username"]
            try:
                resp = s.get(f"https://t.me/{h}", timeout=25)
                verdict, name, subs, kind, desc = classify(h, resp.text, resp.status_code)
                code = resp.status_code
            except Exception as e:
                verdict, name, subs, kind, desc, code = f"fetch error: {e}", "", "", "", "", ""

            last = ""
            if a.activity and verdict == "live":
                try:
                    time.sleep(a.sleep * random.uniform(0.6, 1.4))
                    t = s.get(f"https://t.me/s/{h}", timeout=25).text
                    stamps = TIME.findall(t)
                    last = stamps[-1][:10] if stamps else ""
                except Exception:
                    pass

            w.writerow({"username": h, "name_tgnav": r.get("name", ""), "verdict": verdict,
                        "name_now": name, "subscribers_now": subs, "kind": kind,
                        "last_post": last, "description_now": desc, "http_status": code})
            fh.flush()
            print(f"[{i}/{len(todo)}] @{h}: {verdict}"
                  + (f" · {subs} {kind}" if subs else "")
                  + (f" · last post {last}" if last else ""), file=sys.stderr)
            time.sleep(a.sleep * random.uniform(0.6, 1.4))


if __name__ == "__main__":
    main()
