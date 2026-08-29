import csv, openpyxl, re
from openpyxl.styles import Font, PatternFill, Alignment
from openpyxl.utils import get_column_letter as L

SNAP="2026-08-29"
rows=list(csv.DictReader(open('out/channels_meta.csv',encoding='utf-8-sig')))
by={r['username'].lower():r for r in rows}

wb=openpyxl.load_workbook('orig.xlsx')                 # formulas
wv=openpyxl.load_workbook('orig.xlsx',data_only=True)  # cached values

# ---- drop the trailing all-empty (styled-only) rows Google exports ----
for _w in (wb,wv):
    for _ws in _w.worksheets:
        _last=max((c.row for r in _ws.iter_rows() for c in r if c.value not in (None,'')),default=1)
        if _ws.max_row>_last: _ws.delete_rows(_last+1,_ws.max_row-_last)

# ---- handles already present anywhere in the workbook ----
HRE=re.compile(r'(?:@|t\.me/|telegram\.me/)([A-Za-z0-9_]{3,64})')
EXIST=set()
for ws in wb.worksheets:
    for row in ws.iter_rows(values_only=True):
        for c in row:
            if isinstance(c,str): EXIST.update(m.lower() for m in HRE.findall(c))

# ---- tiers ----
NAME_KW=['机场','翻墙','科学上网','梯子','节点','vpn','clash','v2ray','ssr','trojan','shadowsocks',
         'v2board','免费节点','加速器','跑路','proxy','代理','测速','airport','小火箭','shadowrocket','订阅分享','白嫖']
HAND_A={'airport_chat','jichang_user','mfbp1','paoludaily'}
tierA,tierB,tierC=[],[],[]
for r in rows:
    if r['username'].lower() in EXIST: continue
    hits=[k for k in NAME_KW if k in r['name'].lower()]
    if r['cat']=='机场测试':      tierA.append((r,'TGNAV category 机场测试'))
    elif r['username'].lower() in HAND_A: tierA.append((r,'airport/node topic in name'))
    elif hits:                   tierB.append((r,'name matches: '+', '.join(hits)))
    else:                        tierC.append((r,''))
tierA.sort(key=lambda x:x[0]['username'].lower())
tierB.sort(key=lambda x:x[0]['username'].lower())
tierC.sort(key=lambda x:x[0]['username'].lower())

ARIAL=Font(name='Arial'); BOLD=Font(name='Arial',bold=True)
YEL=PatternFill('solid',fgColor='FFFF00'); HDR=PatternFill('solid',fgColor='D9D9D9')
def subs_num(s):
    if not s: return ''
    s=s.replace(',','')
    m=re.match(r'^([\d.]+)([KkMm]?)$',s)
    if not m: return s
    n=float(m.group(1)); u=m.group(2).lower()
    return int(n*(1000 if u=='k' else 1_000_000 if u=='m' else 1))

PROV=['Subscribers (TGNAV, %s)'%SNAP,'TGNAV category','TGNAV type',
      'First listed on TGNAV','Status on TGNAV','Last listed on TGNAV','Why included']
def prov_vals(r):
    return [subs_num(r['subs']), r['cat'], r['type'], r['first_seen'][:10],
            'live' if r['status']=='present' else 'removed', r['last_seen'][:10]]

def append_block(sheet, items, subs_fn, launch_fn, start, label):
    ws=wb[sheet]
    c=ws.cell(start,1,label); c.font=BOLD; c.fill=YEL
    for j,h in enumerate(PROV): 
        cc=ws.cell(start,10+j,h); cc.font=BOLD; cc.fill=HDR
    for i,(r,why) in enumerate(items):
        rw=start+1+i
        ws.cell(rw,1,r['name']).font=ARIAL
        ws.cell(rw,2,'@'+r['username']).font=ARIAL
        ws.cell(rw,3,f'={subs_fn}(B{rw})').font=ARIAL
        ws.cell(rw,4,f'={launch_fn}(B{rw})').font=ARIAL
        for j,v in enumerate(prov_vals(r)+[why]):
            ws.cell(rw,10+j,v).font=ARIAL
    return start+len(items)

# ---- value snapshot of the script-driven columns, for every existing row ----
for sheet,(sc,lc) in {'Telegram Channel Subscribers':(3,4),'TG Discussion Groups':(3,4)}.items():
    ws,wsv=wb[sheet],wv[sheet]
    last=max((c.row for r in ws.iter_rows() for c in r if c.value not in (None,'')),default=0)
    h1=ws.cell(1,8 if sheet=='TG Discussion Groups' else 20,'')  # placeholder no-op
for sheet in ('Telegram Channel Subscribers','TG Discussion Groups'):
    ws,wsv=wb[sheet],wv[sheet]
    col = 17  # Q
    ws.cell(1,col,  'Subscribers (value snapshot %s)'%SNAP).font=BOLD
    ws.cell(1,col+1,'Launch date (value snapshot)').font=BOLD
    ws.cell(1,col).fill=HDR; ws.cell(1,col+1).fill=HDR
    for rw in range(2, ws.max_row+1):
        if not ws.cell(rw,2).value: continue
        for k,src in ((0,3),(1,4)):
            v=wsv.cell(rw,src).value
            if v not in (None,''): ws.cell(rw,col+k,v).font=ARIAL

A_ch=[t for t in tierA if t[0]['type']!='群组']
A_gr=[t for t in tierA if t[0]['type']=='群组']
append_block('Telegram Channel Subscribers', A_ch,'GET_TG_SUBS','GET_TG_LAUNCH',70,
             'Added from TGNAV directory scrape (%s) — airport / node / VPN channels not already listed'%SNAP)
append_block('TG Discussion Groups', A_gr,'GET_GRP_SUBS','GET_GRP_LAUNCH',23,
             'Added from TGNAV directory scrape (%s) — airport / node / VPN groups not already listed'%SNAP)

# ---- detail tabs ----
DET=['Channel name','Handle','t.me link','Type','TGNAV category','Subscribers (TGNAV, %s)'%SNAP,
     'Status on TGNAV','First listed on TGNAV','Last listed on TGNAV','Telegram ID','TGNAV updated',
     'Why flagged','Description (TGNAV)']
SLIM=[0,1,3,4,5,6,7,8]   # name, handle, type, category, subs, status, first, last
def detail_tab(title, items, note, desc_cap=900, slim=False):
    ws=wb.create_sheet(title)
    ws.cell(1,1,note).font=Font(name='Arial',bold=True,italic=True)
    hdrs=[DET[i] for i in SLIM] if slim else DET
    for j,h in enumerate(hdrs,1):
        c=ws.cell(2,j,h); c.font=BOLD; c.fill=HDR
    for i,(r,why) in enumerate(items):
        rw=3+i
        vals=[r['name'],'@'+r['username'],'https://t.me/'+r['username'],r['type'],r['cat'],
              subs_num(r['subs']),'live' if r['status']=='present' else 'removed',
              r['first_seen'][:10],r['last_seen'][:10],r['tgid'],r['updated'],why,r['desc'][:desc_cap]]
        if slim: vals=[vals[i] for i in SLIM]
        for j,v in enumerate(vals,1):
            c=ws.cell(rw,j,v)
            if not slim: c.font=ARIAL
    widths=[30,22,30,8,14,16,14,20,20,16,14,26,70]
    if slim: widths=[widths[i] for i in SLIM]
    for j,w in enumerate(widths,1):
        ws.column_dimensions[L(j)].width=w
    ws.freeze_panes='A3'
    return ws

detail_tab('TGNAV additions (detail)', tierA,
  'The %d channels/groups appended to the two tabs on the left. Source: tgnav.org directory, scraped from the tgnav/tgnav.github.io git history on %s.'%(len(tierA),SNAP))
detail_tab('TGNAV needs screening', tierB,
  'Flagged by an airport/VPN keyword in the channel name but NOT clearly airport-focused — screen these against the inclusion criteria on the Search Method tab before adding. %d rows.'%len(tierB))
detail_tab('TGNAV all other channels', tierC, desc_cap=0, slim=True, note=
  'The rest of the TGNAV directory — no airport/VPN signal found. Kept for completeness; not expected to meet the inclusion criteria. %d rows.'%len(tierC))

# ---- notes tab ----
ws=wb.create_sheet('TGNAV scrape notes')
ws.column_dimensions['A'].width=118
NOTES=[
 ("H","TGNAV scrape — what was added and how"),
 ("","Prepared %s. Source: the TGNAV Telegram directory (tgnav.org), reconstructed from every commit of"%SNAP),
 ("","github.com/tgnav/tgnav.github.io between 2023-07-30 and 2026-08-28 (406 commits)."),
 ("",""),
 ("H","IMPORTANT — columns C and D will show #NAME? until you re-attach the Apps Script"),
 ("","Columns C and D of the first two tabs use the custom functions GET_TG_SUBS / GET_TG_LAUNCH /"),
 ("","GET_GRP_SUBS / GET_GRP_LAUNCH. Those live in an Apps Script bound to the ORIGINAL spreadsheet and do"),
 ("","not travel with a copy. To restore them: open the original sheet, Extensions > Apps Script, copy the"),
 ("","code, then paste it into Extensions > Apps Script in this file. The formulas are unchanged, so every"),
 ("","row (old and new) will fill in once the script is attached."),
 ("","Meanwhile, columns Q and R hold a value snapshot of what those formulas last returned, so no number is"),
 ("","lost, and column J holds the subscriber count TGNAV itself reported for the newly added rows."),
 ("",""),
 ("H","What was added"),
 ("","%d channels appended to 'Telegram Channel Subscribers' (from row 71) and %d groups appended to"%(len(A_ch),len(A_gr))),
 ("","'TG Discussion Groups' (from row 24). Existing rows, formulas and summary statistics are untouched —"),
 ("","the MEDIAN/AVERAGE/SUM at C56:C59 still cover only your original C2:C55, so the new unscreened rows"),
 ("","do not move your figures."),
 ("",""),
 ("H","How channels were selected"),
 ("","TGNAV listed %d distinct channels, groups and bots across the period. %d of them were already in this"%(len(rows),len(rows)-len(tierA)-len(tierB)-len(tierC))),
 ("","workbook. The remainder were split three ways:"),
 ("","  • TGNAV additions (detail) — %d rows. Everything in TGNAV's own 机场测试 (airport-testing) category,"%len(tierA)),
 ("","    plus four channels/groups whose names are explicitly about airports, nodes or 跑路 reports."),
 ("","    These are the rows appended to the first two tabs."),
 ("","  • TGNAV needs screening — %d rows. An airport/VPN keyword appears in the name, but the channel is"%len(tierB)),
 ("","    not obviously airport-focused (proxy client software, general 破解软件/白嫖 resource channels)."),
 ("","    They are listed with the keyword that flagged them so you can screen them yourself."),
 ("","  • TGNAV all other channels — %d rows. No airport/VPN signal; almost all are anime, wallpaper, news,"%len(tierC)),
 ("","    software or NSFW channels well outside this list's scope."),
 ("","This split follows the inclusion criterion on the Search Method tab: include channels that only discuss"),
 ("","airport-related issues, exclude those that raise them only occasionally. Move rows between tabs freely."),
 ("",""),
 ("H","Caveats"),
 ("","• TGNAV's 机场测试 category only exists in the site's July 2026 rebuild, so a channel that was listed"),
 ("","  and removed before then carries no category and may be under-detected."),
 ("","• 'First/Last listed on TGNAV' are the dates the channel appeared in the TGNAV directory, not the"),
 ("","  channel's own launch date. Launch dates are left to your GET_TG_LAUNCH function."),
 ("","• Subscriber counts in column J are TGNAV's own figures as of its last data update, rounded by TGNAV"),
 ("","  (e.g. 14.0K), not live counts."),
 ("","• %d of the added rows are channels TGNAV has since removed; they may be dead, renamed or private."%sum(1 for r,_ in tierA if r['status']!='present')),
]
for i,(kind,txt) in enumerate(NOTES,1):
    c=ws.cell(i,1,txt)
    c.font=Font(name='Arial',bold=(kind=="H"),sz=11)
    c.alignment=Alignment(vertical='top')

for sheet in ('Telegram Channel Subscribers','TG Discussion Groups'):
    for j in range(10,19): wb[sheet].column_dimensions[L(j)].width=20


import csv as _csv
DETC=['Channel name','Handle','t.me link','Type','TGNAV category','Subscribers (TGNAV, %s)'%SNAP,
      'Status on TGNAV','First listed on TGNAV','Last listed on TGNAV','Telegram ID','TGNAV updated',
      'Why flagged','Description (TGNAV)']
def dump(fn, items):
    with open(fn,'w',newline='',encoding='utf-8-sig') as f:
        w=_csv.writer(f); w.writerow(DETC)
        for r,why in items:
            w.writerow([r['name'],'@'+r['username'],'https://t.me/'+r['username'],r['type'],r['cat'],
                        subs_num(r['subs']),'live' if r['status']=='present' else 'removed',
                        r['first_seen'][:10],r['last_seen'][:10],r['tgid'],r['updated'],why,r['desc']])
dump('out/tgnav_additions.csv', tierA)
dump('out/tgnav_needs_screening.csv', tierB)
dump('out/tgnav_all_other_channels.csv', tierC)

wb.save('new_sheet.xlsx')
print("tierA",len(tierA),"(channels",len(A_ch),"groups",len(A_gr),") tierB",len(tierB),"tierC",len(tierC))
print("already in workbook:",len(EXIST))
print("saved new_sheet.xlsx")
