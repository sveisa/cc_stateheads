import subprocess, re, json, sys
import sys, os
REPO = os.environ.get("TGNAV_REPO", sys.argv[1] if len(sys.argv) > 1 else "tgnav.github.io")
OUT  = os.environ.get("TGNAV_OUT", sys.argv[2] if len(sys.argv) > 2 else ".")
os.makedirs(OUT, exist_ok=True)
from urllib.parse import unquote

def git(*a):
    r = subprocess.run(["git","-C",REPO,*a], capture_output=True)
    return None if r.returncode else r.stdout.decode("utf-8","replace")

log = git("log","--first-parent","--reverse","--format=%H\t%ad\t%s","--date=format:%Y-%m-%d %H:%M").strip().split("\n")
commits = [l.split("\t",2) for l in log]

CARD   = re.compile(r'data-url=(?:"([^"]*)"|\'([^\']*)\'|([^\s>"\']+))')
STRONG = re.compile(r'<strong>(.*?)</strong>', re.S)
USER   = re.compile(r'^[A-Za-z0-9_]{3,64}$')

def norm(v):
    """Return telegram username, or None if this card is an ad / external site."""
    v = unquote(v.strip()).strip()
    if not v: return None
    m = re.match(r'^/?go/?\?url=(.*)$', v)
    if m: v = m.group(1)
    elif re.match(r'^/?link/?\?url=', v): return None       # ad / external link
    elif v.startswith(("http://","https://","//","#","/")): return None
    v = v.split("?")[0].split("#")[0].strip("/")
    if not v or "." in v or "/" in v: return None           # bare domain = ad
    return v if USER.match(v) else None

def from_index(html):
    out = {}
    for chunk in html.split("url-card")[1:]:
        m = CARD.search(chunk)
        if not m: continue
        u = norm(next(g for g in m.groups() if g is not None))
        if not u: continue
        s = STRONG.search(chunk)
        name = re.sub(r'<[^>]+>','',s.group(1)).strip() if s else ""
        out.setdefault(u.lower(), (u, name))
    return out

snaps = []
for sha, date, subj in commits:
    t = git("ls-tree","-d","--name-only",f"{sha}:detail")
    if t is not None:
        chans = {n.lower(): (n, "") for n in t.strip().split("\n") if n}
        era = "detail"
    else:
        html = git("show", f"{sha}:index.html")
        if html is None:
            chans, era = {}, "broken"
        else:
            chans = from_index(html)
            era = "index" if chans else "broken"
    snaps.append({"sha": sha, "date": date, "subj": subj, "era": era,
                  "channels": {k: v[0] for k, v in sorted(chans.items())},
                  "names": {k: v[1] for k, v in sorted(chans.items()) if v[1]}})

json.dump(snaps, open(f"{OUT}/snaps.json","w"))
prev = None
for i, x in enumerate(snaps):
    n = len(x["channels"])
    if prev is None or abs(n-prev) >= 8 or x["era"] == "broken":
        print(f"{i:3d} {x['date']} {x['era']:6s} {n:4d}  {x['subj'][:45]}")
    prev = n
