#!/usr/bin/env bash
# Read-only query against the snapshot database named by SPIKE_DB.
exec docker exec -i -e PGOPTIONS="-c default_transaction_read_only=on" grit-postgres-1 psql -U grit -d "$SPIKE_DB" -At "$@"
