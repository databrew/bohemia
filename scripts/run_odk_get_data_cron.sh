#!/bin/bash
# Runs odk_get_data for instance id's not already retrieved.
echo "Started the data retrieval"
scripts/update_database.R
echo "ODK data retrieval complete"
