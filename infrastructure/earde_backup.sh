#!/usr/bin/env bash
# Runs as a cron job on the Hetzner VPS.
# Uses the postgres system user so pg_dump can authenticate via peer auth —
# avoids storing passwords in plaintext or in .pgpass.
set -euo pipefail

DB_NAME="earde_prod"
BACKUP_DIR="/var/backups/earde"
TIMESTAMP=$(date +"%Y%m%d_%H%M%S")
BACKUP_FILE="${BACKUP_DIR}/earde_db_backup_${TIMESTAMP}.sql.gz"
RETENTION_DAYS=7

mkdir -p "${BACKUP_DIR}"

# pg_dump exits non-zero on failure; pipefail ensures gzip exit code doesn't
# mask it. PIPESTATUS[0] is a second guard for clarity.
sudo -u postgres pg_dump "${DB_NAME}" | gzip > "${BACKUP_FILE}"

if [ "${PIPESTATUS[0]}" -ne 0 ]; then
    echo "ERROR: pg_dump failed for ${DB_NAME}" >&2
    rm -f "${BACKUP_FILE}"
    exit 1
fi

echo "Backup written to ${BACKUP_FILE}"

# Rotate: remove backups older than RETENTION_DAYS to prevent unbounded growth.
find "${BACKUP_DIR}" -maxdepth 1 -name "earde_db_backup_*.sql.gz" \
    -mtime "+${RETENTION_DAYS}" -delete

echo "Rotation complete: removed backups older than ${RETENTION_DAYS} days"
