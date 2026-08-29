import json, subprocess, re, csv, os, collections, html as _html
import sys, os
REPO = os.environ.get("TGNAV_REPO", sys.argv[1] if len(sys.argv) > 1 else "tgnav.github.io")
OUT  = os.environ.get("TGNAV_OUT", sys.argv[2] if len(sys.argv) > 2 else ".")
os.makedirs(OUT, exist_ok=True)
snaps=[s for s in json.load(open(f"{OUT}/snaps.json")) if s["era"]!="broken"]
MIG=next(i for i,s in enumerate(snaps) if s["era"]=="detail")
MIG_DATE=snaps[MIG]["date"]

def sh(*a):
    r=subprocess.run(["git","-C",REPO,*a],capture_output=True)
    return None if r.returncode else r.stdout.decode("utf-8","replace")

TITLE=re.compile(r'<title>(.*?)</title>',re.S)
def clean_title(t):
    t=_html.unescape(re.sub(r'<[^>]+>','',t))
    t=re.sub(r'\s+',' ',t).strip()
    t=re.sub(r'\s*[-–|｜]\s*(Telegram|TG).*$','',t)
    return t.split(' - ')[0].strip()

# ---------- events ----------
ch={}
for i,s in enumerate(snaps):
    cur=set(s["channels"]); prev=set(snaps[i-1]["channels"]) if i else set()
    for k in sorted(cur-prev):
        r=ch.setdefault(k,{"key":k,"slug":s["channels"][k],"adds":[],"removes":[],"name":""})
        r["adds"].append((i,s["date"],s["sha"],s["subj"]))
        if s.get("names",{}).get(k): r["name"]=clean_title(s["names"][k])
    for k in sorted(prev-cur):
        ch[k]["removes"].append((i,s["date"],s["sha"],s["subj"],snaps[i-1]["sha"],snaps[i-1]["date"]))

HEAD=set(snaps[-1]["channels"])
for k,r in ch.items():
    r["present"]=k in HEAD
    r["first_i"],r["first_date"]=r["adds"][0][0],r["adds"][0][1]
    r["last_date"]=snaps[-1]["date"] if r["present"] else r["removes"][-1][5]
    r["gone_date"]=""  if r["present"] else r["removes"][-1][1]
    r["gone_subj"]="" if r["present"] else r["removes"][-1][3]
    r["stints"]=len(r["adds"])

# ---------- names ----------
for k,r in ch.items():
    if r["present"]:
        p=f"{REPO}/detail/{k}/index.html"
        if os.path.exists(p):
            m=TITLE.search(open(p,encoding="utf-8").read())
            if m: r["name"]=clean_title(m.group(1))
    elif r["removes"]:
        h=sh("show",f"{r['removes'][-1][4]}:detail/{r['slug']}/index.html")
        if h:
            m=TITLE.search(h)
            if m: r["name"]=clean_title(m.group(1))

# strip a trailing "@handle" the site appends to some titles
for k,r in ch.items():
    n=re.sub(r'\s*[@＠]'+re.escape(k)+r'\s*$','',r["name"],flags=re.I).strip(' -|｜')
    if n: r["name"]=n

# ---------- current categories ----------
cat=collections.defaultdict(set)
D=re.compile(r'/detail/([^/"\']+)/')
for root in ("channel","group"):
    for c in sorted(os.listdir(f"{REPO}/{root}")):
        p=os.path.join(REPO,root,c,"index.html")
        if os.path.isdir(os.path.join(REPO,root,c)) and os.path.exists(p):
            for sl in D.findall(open(p,encoding="utf-8").read()): cat[sl.lower()].add(c)
# anchor sections on channel/index.html and group/index.html
for f in ("channel/index.html","group/index.html"):
    h=open(os.path.join(REPO,f),encoding="utf-8").read()
    parts=re.split(r'id="([^"]{1,40})"',h)
    for j in range(1,len(parts)-1,2):
        name,body=parts[j],parts[j+1]
        if name.isascii() and name.lower() not in ("nsfw","ios资源"): continue
        for sl in D.findall(body): cat[sl.lower()].add(name)
def catstr(k): return "|".join(sorted(cat.get(k,[]))) or ""

# ---------- CSVs ----------
def era(i): return "旧站 Hugo" if i<MIG else "新站 Astro"
rows=sorted(ch.values(), key=lambda r:(r["first_i"], r["key"]))

with open(f"{OUT}/all_channels.csv","w",newline="",encoding="utf-8-sig") as f:
    w=csv.writer(f); w.writerow(["username","name","status","first_seen","last_seen","removed_at","stints","current_category","baseline_2023_07_30"])
    for r in rows:
        w.writerow([r["slug"],r["name"],"present" if r["present"] else "removed",
                    r["first_date"],r["last_date"],r["gone_date"],r["stints"],
                    catstr(r["key"]),"yes" if r["first_i"]==0 else ""])

added=[r for r in rows if r["first_i"]>0]
with open(f"{OUT}/added.csv","w",newline="",encoding="utf-8-sig") as f:
    w=csv.writer(f); w.writerow(["username","name","added_on","added_in_commit","still_present","current_category","site_era"])
    for r in added:
        w.writerow([r["slug"],r["name"],r["first_date"],r["adds"][0][2][:9],
                    "yes" if r["present"] else "no",catstr(r["key"]),era(r["first_i"])])

removed=[r for r in rows if not r["present"]]
with open(f"{OUT}/removed.csv","w",newline="",encoding="utf-8-sig") as f:
    w=csv.writer(f); w.writerow(["username","name","first_seen","last_seen","removed_on","removal_commit","removal_commit_msg","times_listed"])
    for r in sorted(removed,key=lambda r:r["gone_date"]):
        w.writerow([r["slug"],r["name"],r["first_date"],r["last_date"],r["gone_date"],
                    r["removes"][-1][2][:9],r["removes"][-1][3],r["stints"]])

# ---------- per-commit timeline ----------
with open(f"{OUT}/commit_timeline.csv","w",newline="",encoding="utf-8-sig") as f:
    w=csv.writer(f); w.writerow(["date","commit","message","total","added","removed","added_list","removed_list"])
    for i,s in enumerate(snaps):
        cur=set(s["channels"]); prev=set(snaps[i-1]["channels"]) if i else set()
        a,d=sorted(cur-prev),sorted(prev-cur)
        if i==0 or a or d:
            w.writerow([s["date"],s["sha"][:9],s["subj"],len(cur),len(a) if i else 0,len(d),
                        " ".join(a) if i else "", " ".join(d)])

# ---------- monthly ----------
mon=collections.defaultdict(lambda:[0,0])
for r in rows:
    if r["first_i"]>0: mon[r["first_date"][:7]][0]+=1
    for rm in r["removes"]: mon[rm[1][:7]][1]+=1
with open(f"{OUT}/monthly.csv","w",newline="",encoding="utf-8-sig") as f:
    w=csv.writer(f); w.writerow(["month","added","removed","net"])
    for m in sorted(mon): w.writerow([m,mon[m][0],mon[m][1],mon[m][0]-mon[m][1]])

print(f"period          : {snaps[0]['date']} -> {snaps[-1]['date']}  ({len(snaps)} commits)")
print(f"baseline set    : {sum(1 for r in rows if r['first_i']==0)}")
print(f"present at HEAD : {len(HEAD)}")
print(f"ever listed     : {len(rows)}")
print(f"ADDED (post-baseline): {len(added)}  (still present {sum(1 for r in added if r['present'])})")
print(f"REMOVED (gone now)   : {len(removed)}")
print(f"removal events total : {sum(len(r['removes']) for r in rows)}")
print(f"re-listed after removal: {sum(1 for r in rows if r['stints']>1)}")
print(f"names resolved  : {sum(1 for r in rows if r['name'])}/{len(rows)}")
print(f"category known  : {sum(1 for r in rows if catstr(r['key']))}")
print("\n机场测试 now:", sum(1 for r in rows if '机场测试' in catstr(r['key'])))
