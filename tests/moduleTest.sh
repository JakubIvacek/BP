#!/usr/bin/env bash
set -euo pipefail

LOGFILE="tests-databases.log"
touch "$LOGFILE"

# === Funkcia na log s duration ===
# $1 = fáza (napr. "SET_CREDENTIALS"), $2 = exit_code, $3 = start_time (sekundy), $4 = end_time (sekundy), $5 = voliteľný extra text
log_with_duration() {
  local phase=$1 exit_code=$2 start_time=$3 end_time=$4
  local extra="${5:-}"
  local start_ts end_ts duration
  start_ts=$(date -d "@$start_time" '+%Y-%m-%dT%H:%M:%S')
  end_ts=$(date -d "@$end_time"   '+%Y-%m-%dT%H:%M:%S')
  duration=$(( end_time - start_time ))
  echo "[$end_ts] END_${phase} EXIT_CODE: $exit_code DURATION: ${duration}s ${extra}" >> "$LOGFILE"
}

# === 3) Spracovanie databáz ===
DBS=( "gencode" "cosmic" "1000genomes" "uniprot" )

for idx in "${!DBS[@]}"; do
  db="${DBS[$idx]}"

  # --- Download fáza ---
  phase="DOWNLOAD+LIFTOVER_$db"
  start_time=$(date +%s)
  START_TS=$(date '+%Y-%m-%dT%H:%M:%S')
  echo "[$START_TS] START_DOWNLOAD+LIFTOVER $db" >> "$LOGFILE"

  sbt "run -d $db"
  exit_code=$?
  end_time=$(date +%s)

  log_with_duration "DOWNLOAD+LIFTOVER_${db}" "$exit_code" "$start_time" "$end_time"

  # --- Info fáza ---
  phase="INFO_$db"
  start_time=$(date +%s)
  START_TS=$(date '+%Y-%m-%dT%H:%M:%S')
  echo "[$START_TS] START_INFO $db" >> "$LOGFILE"

  sbt "run -i $db"
  exit_code=$?
  end_time=$(date +%s)

  log_with_duration "INFO_${db}" "$exit_code" "$start_time" "$end_time"

  # --- Delete fáza (2x) ---
  for del_run in 1 2; do
    delete_id=$(( idx * 2 + del_run ))
    phase="DELETE_${db}_$delete_id"
    start_time=$(date +%s)
    START_TS=$(date '+%Y-%m-%dT%H:%M:%S')
    echo "[$START_TS] DELETE_START $db delete_id#$delete_id" >> "$LOGFILE"

    sbt "run -r $delete_id"
    exit_code=$?
    end_time=$(date +%s)

    log_with_duration "DELETE_${db}_$delete_id" "$exit_code" "$start_time" "$end_time"

    if [ $exit_code -ne 0 ]; then
      echo "Chyba pri DELETE run #$del_run pre $db delete_id#$delete_id, ukončujem."
      exit $exit_code
    fi
  done
done

echo "Všetky fázy pre všetky databázy dokončené. Log nájdeš v $LOGFILE"