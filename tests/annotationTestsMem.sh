#!/usr/bin/env bash
set -euo pipefail

LOGFILE="annotation_tests.log"
touch "$LOGFILE"

FILE_PATH="filename"
OUT_DIR="dir"
REFERENCE="ref"
mkdir -p "$OUT_DIR"

# začiatok merania času
start_time=$(date +%s)
start_ts=$(date '+%Y-%m-%dT%H:%M:%S')
echo "[$start_ts] START ANNOTATING: $FILE_PATH TO: $OUT_DIR" >> "$LOGFILE"

# pripravíme si temp súbor na výstup memory-štatistík
mem_tmp=$(mktemp)

# samotné anotovanie s meraním pamäte
/usr/bin/time -v -o "$mem_tmp" sbt "run -f $FILE_PATH -a $REFERENCE -o $OUT_DIR"
EXIT_CODE=$?

# koniec merania času
end_time=$(date +%s)
end_ts=$(date '+%Y-%m-%dT%H:%M:%S')
duration=$((end_time - start_time))

# vyparsujeme Max RSS (v kB)
max_rss=$(grep "Maximum resident set size" "$mem_tmp" | awk '{print $6}')
rm "$mem_tmp"

# zapíšeme do logu
echo "[$end_ts] END ANNOTATING: $FILE_PATH TO: $OUT_DIR EXIT_CODE: $EXIT_CODE DURATION: ${duration}s MAX_RSS: ${max_rss}KB" >> "$LOGFILE"

# aj na stdout
echo "[$end_ts] Exit code: $EXIT_CODE; Duration: ${duration}s; Max RSS: ${max_rss}KB"