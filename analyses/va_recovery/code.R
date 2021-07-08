library(yaml)
library(dplyr)
library(DBI)
library(RPostgres)

# Define whether local or not
is_local <- FALSE

# Read in credentials
creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')

# Connect to the database
drv <- RPostgres::Postgres()
if(is_local){
  con <- dbConnect(drv, dbname='bohemia')
} else {
  psql_end_point = creds$endpoint
  psql_user = creds$psql_master_username
  psql_pass = creds$psql_master_password
  con <- dbConnect(drv, dbname='bohemia', host=psql_end_point, 
                   port=5432,
                   user=psql_user, password=psql_pass)
}

va <- dbGetQuery(conn = con, 
                          paste0("SELECT * FROM va"))
dbDisconnect(con)

# Create the spreadsheet for filling out info by fieldworkers
pd <- va %>%
  dplyr::select(instance_id,
                hh_id,
                death_id,
                wid,
                id10476,
                server) %>%
  mutate(characters = nchar(id10476)) %>%
  mutate(characters = ifelse(is.na(characters), 0, characters)) %>%
  mutate(is_truncated = characters >= 253) %>%
  mutate(country = ifelse(grepl('ihi', server), 'Tanzania', 'Mozambique')) %>%
  mutate(missing_text = '',
         full_text = '')

# Write csvs
pd_moz <- pd %>% filter(country == 'Mozambique')
pd_tza <- pd %>% filter(country == 'Tanzania')
readr::write_csv(pd_moz, 'mozambique_vas.csv')
readr::write_csv(pd_tza, 'tanzania_vas.csv')

# summarise
pd %>%
  group_by(country, is_truncated) %>%
  tally %>%
  ungroup %>%
  group_by(country) %>%
  mutate(p = n / sum(n) * 100)
