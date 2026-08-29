# TGNAV channel churn, July 2023 – August 2026

What entered and left the Telegram channel directory published at
[tgnav/tgnav.github.io](https://github.com/tgnav/tgnav.github.io), reconstructed
commit by commit from the built pages in that repo's git history.

## Headline

| | |
|---|---|
| Period | 2023-07-30 → 2026-08-28 (406 first-parent commits) |
| Listed at the first commit | 203 |
| **Added since** | **835** (725 still listed) |
| **Removed and still gone** | **212** |
| Listed today | 826 |
| Distinct channels ever | 1,038 |

Almost all of the movement is three site rebuilds — 729 of the 835 additions and
161 of the 212 removals land in three commits:

| | Date | Commit | Δ | Total after |
|---|---|---|---|---|
| Rebuild 1 | 2024-09-30 | `01933c070` 重大更新：新增详情页面 | +118 / −4 | 302 |
| Rebuild 2 | 2025-02-05 | `25ad359c5` bulk data merge | +104 / −20 | 383 |
| Rebuild 3 | 2026-07-15 | `150a9f588` "test new version" | +534 / −140 | 789 |

The remaining 106 additions and 51 removals are spread across the other 403 commits.
Of the 203 channels present at the first commit, 101 are still listed. The median
removed channel had been listed for 645 days; only 27 lasted under 90 days. 32
channels were removed and later re-listed.

The 机场测试 (airport-testing) category exists only in the July 2026 site: all 37 of
its channels arrived with Rebuild 3, so the category has no earlier history.

## Files

| File | Contents |
|---|---|
| `added.csv` | 835 channels added after the first commit — handle, name, date added, whether still listed, current category, site era |
| `removed.csv` | 212 channels absent from the current site — handle, name, first/last listed, removal date, removing commit and message |
| `all_channels.csv` | all 1,038 channels with status, first/last listed, stint count, current category |
| `commit_timeline.csv` | per-commit totals plus the exact handles added and removed at each one |
| `monthly.csv` | added / removed / net per month |
| `report.html` | standalone browsable report (searchable, filterable, sortable) |

## Reproducing

```sh
git clone https://github.com/tgnav/tgnav.github.io.git
python3 01_snapshot_channels.py tgnav.github.io out   # per-commit channel sets -> out/snaps.json
python3 02_export_diffs.py       tgnav.github.io out   # -> the CSVs above
```

## Method, and its limits

The repo publishes a **built site, not a database**, so the channel list has to be
read back out of the HTML. Three markup eras need handling:

1. **2023-07 → 2023-11** — homepage cards carry `data-url="/go?url=<handle>"`.
2. **2023-11 → 2024-09** — the same cards carry a bare `data-url="<handle>"`, then
   after the 2024-08-14 "代码压缩" commit the attribute is unquoted (`data-url=<handle>`).
3. **2024-09-30 →** — one directory per channel under `detail/<handle>/`.

Handles are compared case-insensitively (Telegram usernames are). Promoted
airport/VPN adverts — cards pointing at a domain rather than a `t.me` handle — are
excluded. Channel names come from each page's `<title>` at the channel's last
appearance.

Known limits:

- **The 2024-09-30 rebuild is a measurement change as well as a content change.**
  Before it, the homepage *was* the whole published directory; after it, the site
  publishes detail pages. Its +118 is a real jump in what the site showed, but not
  118 separate editorial decisions.
- **One commit is skipped.** `清除页面（路由错误）` (2026-07-15 12:49) published a
  broken build with no pages; counting it would register a mass deletion. The fix
  landed a minute later.
- **Categories are current-only.** Category pages (`channel/<cat>/`) exist only in
  the July 2026 site, so a channel can be placed in a category as it stands today,
  not historically. 112 channels that are live today are linked from no listing
  page at all and show as uncategorised.

## Merging into the airport-channel research list

`04_merge_into_workbook.py` takes the existing *List of Telegram Channels / Blogs / Forums*
workbook (exported as .xlsx) and appends the TGNAV channels it does not already contain.
`03_channel_metadata.py` first pulls per-channel metadata (name, type, category, subscriber
count, Telegram ID, description) out of the TGNAV detail pages into `channels_meta.csv`.

The workbook already contained **66 handles**. Of the 1,038 TGNAV channels, 18 were among
them; the remaining 1,020 were split three ways, following the inclusion criterion on the
workbook's own *Search Method* tab (include channels that only discuss airport-related
issues; exclude those that raise them only occasionally):

| Tier | Rows | What it is | Where it goes |
|---|---|---|---|
| `tgnav_additions.csv` | 26 | TGNAV's own 机场测试 category, plus `@airport_chat`, `@jichang_user`, `@mfbp1`, `@paoludaily` | appended to *Telegram Channel Subscribers* (24 channels, from row 71) and *TG Discussion Groups* (2 groups, from row 24) |
| `tgnav_needs_screening.csv` | 20 | an airport/VPN keyword in the name, but not obviously airport-focused (proxy clients, general 破解软件/白嫖 channels) | own tab, with the matched keyword shown |
| `tgnav_all_other_channels.csv` | 974 | no airport/VPN signal — anime, wallpaper, news, software, NSFW | own tab |

`tgnav_merged_workbook.xlsx` is the result: the five original tabs byte-for-byte unchanged,
plus the appended rows and four new tabs.

Two things to know about the merged workbook:

- **Columns C and D are Apps Script custom functions** (`GET_TG_SUBS`, `GET_TG_LAUNCH`,
  `GET_GRP_SUBS`, `GET_GRP_LAUNCH`). They live in a script bound to the *original*
  spreadsheet and do not travel with a copy, so they read `#NAME?` until the script is pasted
  into the copy (Extensions → Apps Script). The formulas are untouched, so every row fills in
  once it is. Columns Q and R hold a value snapshot of what they last returned, and column J
  holds TGNAV's own subscriber count for the appended rows, so no number is lost either way.
- **Summary statistics are deliberately untouched.** `C56:C59` on *Telegram Channel
  Subscribers* still cover only the original `C2:C55`, so the appended, unscreened rows do
  not move the median, mean or totals.

## Checking removed channels against Telegram (`05_check_telegram_status.py`)

TGNAV can say a channel was dropped from the directory; it cannot say whether the
channel is dead, renamed, went private, or is simply no longer listed. Those are
different things, and "dropped from TGNAV" is a poor proxy for any of them — 140 of
the 212 removals happened in the single July 2026 rebuild commit, which looks like an
editorial re-scoping rather than 140 channels going dark at once.

`05_check_telegram_status.py` resolves each handle against its public `t.me` page:

```sh
pip install requests
python3 05_check_telegram_status.py removed.csv -o removed_enriched.csv --activity
```

Output columns: `verdict` (live / gone or renamed / resolves but no counter /
fetch error), `name_now`, `subscribers_now`, `kind` (channel, group, bot),
`last_post` (with `--activity`), `description_now`.

Notes:

- No Telegram account needed — `t.me/<handle>` is a public preview page.
- Default pacing is ~1.5 s between requests with jitter; `--activity` doubles the
  request count by also fetching `t.me/s/<handle>` for the newest post date.
  212 handles takes roughly 5 minutes, or 10 with `--activity`.
- Interrupt-safe: re-run the same command and it skips rows already written.
- A handle that no longer resolves is reported as "gone or renamed" rather than
  "dead" — Telegram serves the same generic page in both cases, and a freed handle
  can be re-registered by someone else, so a live result is not proof of continuity
  either. Check `name_now` against `name_tgnav` before treating a row as the same channel.
- This could not be run from the Claude session that produced these files: `t.me` is
  blocked by the workspace's egress policy (403 at the proxy). Run it locally.
