#!/usr/bin/env bash

LOGFILE="annotation_tests.log"
touch "$LOGFILE"

FILE_PATH="Small.vcf"
OUT_DIR="anotovaneveci"
REFERENCE="hg38"

mkdir -p "$OUT_DIR"


# záznam začiatku v sekundách
start_time=$(date +%s)
start_ts=$(date '+%Y-%m-%dT%H:%M:%S')

echo "[$start_ts] START ANNOTATING: $FILE_PATH TO: $OUT_DIR" >> "$LOGFILE"

# samotné anotovanie
sbt "run -f $FILE_PATH -a $REFERENCE -o $OUT_DIR"
EXIT_CODE=$?

# záznam konca v sekundách
end_time=$(date +%s)
end_ts=$(date '+%Y-%m-%dT%H:%M:%S')

# vypočítame trvanie v sekundách
duration=$((end_time - start_time))

echo "[$end_ts] END ANNOTATING: $FILE_PATH TO: $OUT_DIR EXIT_CODE: $EXIT_CODE DURATION: ${duration}s" >> "$LOGFILE"

# aj na stdout
echo "[$end_ts] Exit code: $EXIT_CODE; Duration: ${duration}s"

