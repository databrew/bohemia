import psycopg2
import pandas as pd
import logging
import yaml
from sqlalchemy import create_engine
from datetime import datetime
import pandas.io.sql as pdsql
import os

# Set up log file for job
logging.basicConfig(filename="logs/apply_corrections.log", level=logging.DEBUG)

# Read in credentials
with open(r'../credentials/credentials.yaml') as file:
    creds = yaml.load(file, Loader=yaml.FullLoader)

# Define whether working locally or not
is_local = False
if is_local:
    dbconn = psycopg2.connect(dbname="bohemia") #psycopg2.connect(dbname="bohemia", user="bohemia_app", password="")
    engine_string = "postgresql:///bohemia"
else:
    dbconn = psycopg2.connect(dbname='bohemia', user = creds['psql_master_username'], password = creds['psql_master_password'], host = creds['endpoint'], port = 5432)
    engine_string = "postgresql+psycopg2://{user}:{password}@{host}:{port}/{database}".format(
        user=creds['psql_master_username'],
        password=creds['psql_master_password'],
        host=creds['endpoint'],
        port='5432',
        database='bohemia',
    )

# Initialize connection to the database
cur = dbconn.cursor()
engine = create_engine(engine_string)
# engine.table_names()

# Read in corrections table
result = engine.execute('SELECT * FROM corrections')
corrections = pd.DataFrame(data = iter(result), columns = result.keys())

# Read in anomalies table
result = engine.execute('SELECT * FROM anomalies')
anomalies = pd.DataFrame(data = iter(result), columns = result.keys())

# Read in fixes table
result = engine.execute('SELECT * FROM fixes')
fixes = pd.DataFrame(data = iter(result), columns = result.keys())

# Keep only those which aren't already done
do_these = corrections[~corrections['id'].isin(fixes['id'])]
show_these = do_these[['id', 'response_details', 'instance_id']]


# Define function for implementing corrections
def implement(id, query = '', who = 'Joe Brew', cur = cur, dbconn = dbconn):
    # Implement the actual fix to the database
    try:
        print('Executing this query:\n')
        print(query)
        cur.execute(query)
    except:
        cur.execute("ROLLBACK")
        print('Problem executing:\n')
        print(query)
        return
    done_at = datetime.now()
    # State the fact that it has been fixed
    cur.execute(
        """
        INSERT INTO fixes (id, done_by, done_at, resolution_code) VALUES(%s, %s, %s, %s)
        """,
        (id, who, done_at, query)
    )
    dbconn.commit()

# Go one-by-one through "show_these" and implement changes
# show_these.iloc[5]
implement(id = 'strange_wid_f8b44ed0-4636-4f4a-a19d-5d40b5117ca5', query = "UPDATE clean_minicensus_main SET wid='375' WHERE instance_id='f8b44ed0-4636-4f4a-a19d-5d40b5117ca5'")
implement(id = 'strange_wid_9906d156-cc9b-4f5a-b341-b05bb819c2bf', query = "UPDATE clean_minicensus_main SET wid='325' WHERE instance_id='9906d156-cc9b-4f5a-b341-b05bb819c2bf'")
implement(id = 'strange_wid_6ffa7378-b1fe-4f39-9a96-9f14fd97704e', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='6ffa7378-b1fe-4f39-9a96-9f14fd97704e'")
implement(id = 'strange_wid_dff375c4-ca51-43f3-b72b-b2baa734a0ab', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='dff375c4-ca51-43f3-b72b-b2baa734a0ab'")
implement(id = 'strange_wid_6eeff804-3892-4164-8964-1cb70556fcc0', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='6eeff804-3892-4164-8964-1cb70556fcc0'")
implement(id = 'strange_wid_edc83ea9-72c0-463c-a1a0-66701c7e5eb7', query = "UPDATE clean_minicensus_main SET wid='395' WHERE instance_id='edc83ea9-72c0-463c-a1a0-66701c7e5eb7'")

implement(id = 'strange_wid_5f466226-1d75-40a9-97fc-5e8cd84448c9', query = "UPDATE clean_minicensus_main SET wid='37' WHERE instance_id='5f466226-1d75-40a9-97fc-5e8cd84448c9'")
implement(id = 'missing_wid_23632449-cb8d-4ea2-a705-4d9f145b352c', query = "UPDATE clean_minicensus_main SET wid='80' WHERE instance_id='23632449-cb8d-4ea2-a705-4d9f145b352c'")
implement(id = 'missing_wid_ee4aca39-2370-49c2-a01e-a295638038e9', query = "UPDATE clean_minicensus_main SET wid='14' WHERE instance_id='ee4aca39-2370-49c2-a01e-a295638038e9'")
implement(id = 'repeat_hh_id_564fe4e1-1978-4bc5-84b4-d80adb7a9bde,7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059', query = "DELETE FROM clean_minicensus_main WHERE instance_id='7ac74d0a-7eb9-4651-a2a6-ee7d8edd7059'")
implement(id = 'repeat_hh_id_36527774-d88c-4b97-8722-b881171ff77c,3be77a06-5646-49fe-9037-f0ff3bc40543', query = "DELETE FROM clean_minicensus_main WHERE instance_id='36527774-d88c-4b97-8722-b881171ff77c'")

dbconn.commit()
cur.close()
dbconn.close()