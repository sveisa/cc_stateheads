#!/usr/bin/env python3
import csv
import os
import re
from pathlib import Path

# Input and output paths
CSV_FILE = "/root/.claude/uploads/9910284c-78c6-5695-a5e2-5c85c39fa1ea/9e164664-gab_newsletters_20172021__newslettersnoradio.csv"
OUTPUT_DIR = "/home/user/cc_stateheads/newsletters"

# Create output directory
Path(OUTPUT_DIR).mkdir(parents=True, exist_ok=True)

def sanitize_filename(title):
    """Remove/replace characters that are invalid in filenames."""
    # Replace invalid characters with underscores or strip them
    title = re.sub(r'[<>:"/\\|?*]', '', title)
    title = title.strip()
    return title

def main():
    count = 0
    with open(CSV_FILE, 'r', encoding='utf-8') as f:
        reader = csv.DictReader(f)
        for row in reader:
            count += 1

            title = row.get('Title', '').strip()
            year = row.get('year', '').strip()
            publish_date = row.get('Publish Date', '').strip()
            text = row.get('text', '').strip()

            if not title or not year:
                print(f"Row {count}: Skipping (missing title or year)")
                continue

            # Create filename: Title year.txt
            safe_title = sanitize_filename(title)
            filename = f"{safe_title} {year}.txt"
            filepath = os.path.join(OUTPUT_DIR, filename)

            # Create content: Title [Publish Date]\n\ntext
            content = f"{title} [{publish_date}]\n\n{text}"

            # Write to file
            try:
                with open(filepath, 'w', encoding='utf-8') as out:
                    out.write(content)
                print(f"Row {count}: Created {filename}")
            except Exception as e:
                print(f"Row {count}: ERROR writing {filename}: {e}")

    print(f"\n=== Complete ===")
    print(f"Processed {count} rows")
    print(f"Output directory: {OUTPUT_DIR}")

if __name__ == "__main__":
    main()
