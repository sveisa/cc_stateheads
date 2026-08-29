import re, os, csv, json, subprocess, html as H
REPO="tgnav"
def sh(*a):
    r=subprocess.run(["git","-C",REPO,*a],capture_output=True)
    return None if r.returncode else r.stdout.decode("utf-8","replace")

FIELDS=["订阅/群员数","Telegram ID","用户名","更新时间"]
def parse(h):
    out={"name":"","type":"","cat":"","subs":"","tgid":"","updated":"","desc":""}
    m=re.search(r'<title>(.*?)</title>',h,re.S)
    if m:
        t=H.unescape(re.sub(r'\s+',' ',m.group(1))).strip()
        out["name"]=re.sub(r'\s*[-–|｜]\s*(Telegram|TG)频道.*$','',t).split(' - ')[0].strip()
    b=h[h.find('<body'):]
    b=re.sub(r'<script.*?</script>','',b,flags=re.S)
    b=re.sub(r'<style.*?</style>','',b,flags=re.S)
    txt=H.unescape(re.sub(r'\s+',' ',re.sub(r'<[^>]+>',' ',b)))
    m=re.search(r'订阅/群员数\s*([0-9][0-9.,]*[KMkm]?)',txt)
    if m: out["subs"]=m.group(1)
    m=re.search(r'Telegram ID\s*(-?\d+)',txt)
    if m: out["tgid"]=m.group(1)
    m=re.search(r'更新时间\s*(\d{4}-\d{2}-\d{2})',txt)
    if m: out["updated"]=m.group(1)
    m=re.search(r'频道简介与描述\s*(.*?)\s*TGNAV TGNAV 精心收录',txt)
    if not m: m=re.search(r'(?:频道|群组)简介与描述\s*(.*?)\s*TGNAV',txt)
    if m: out["desc"]=m.group(1).strip()[:900]
    # category: the token(s) between the name and "-- 直达"
    if out["name"]:
        m=re.search(re.escape(out["name"])+r'\s*(频道|群组|机器人)\s*([^-]{0,30}?)\s*--\s*直达',txt)
        if m: out["type"],out["cat"]=m.group(1),m.group(2).strip()
    return out

rows=[]
alll=list(csv.DictReader(open('out/all_channels.csv',encoding='utf-8-sig')))
# last-seen sha for removed channels
snaps=[s for s in json.load(open('out/snaps.json')) if s["era"]!="broken"]
last_sha={}
for i,s in enumerate(snaps):
    for k in s["channels"]: last_sha[k]=s["sha"]
for i,s in enumerate(snaps):
    prev=set(snaps[i-1]["channels"]) if i else set()
    for k in prev-set(s["channels"]): last_sha[k]=snaps[i-1]["sha"]

for r in alll:
    slug=r['username']; key=slug.lower(); present=r['status']=='present'
    h=None
    if present:
        p=f"{REPO}/detail/{key}/index.html"
        if os.path.exists(p): h=open(p,encoding='utf-8').read()
    else:
        h=sh("show",f"{last_sha.get(key,'HEAD')}:detail/{slug}/index.html")
    m=parse(h) if h else {"name":r['name'],"type":"","cat":"","subs":"","tgid":"","updated":"","desc":""}
    if not m["name"]: m["name"]=r['name']
    m["name"]=re.sub(r'\s*\|\s*TGNAV\s*$','',m["name"],flags=re.I)
    m["name"]=re.sub(r'\s*[@＠]'+re.escape(key)+r'\s*$','',m["name"],flags=re.I).strip(' -|｜')
    rows.append({**r,**m})

with open('out/channels_meta.csv','w',newline='',encoding='utf-8-sig') as f:
    w=csv.DictWriter(f,fieldnames=list(rows[0].keys())); w.writeheader(); w.writerows(rows)
print("rows",len(rows))
print("with subs",sum(1 for r in rows if r['subs']),"with desc",sum(1 for r in rows if r['desc']),"with cat",sum(1 for r in rows if r['cat']))
import collections
print(collections.Counter(r['cat'] for r in rows).most_common(28))
