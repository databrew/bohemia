is_local <- FALSE
library(dplyr)
library(RPostgres)
library(yaml)
library(readr)

# Define credentials
creds <- list(dbname = 'vadb')

# Read in users from the main bohemia app
users <- read_csv('credentials/vatoolusers.csv') # created manually

# Define connection
drv <- RPostgres::Postgres()
if(is_local){
  con <- dbConnect(drv = drv,
                   dbname = creds$dbname)
  # # create cods data 
  # cods <- 
  #   tibble(user_id = c('test_entry'),
  #          death_id= c('MTM-013-701'),
  #          cod_code_1 = c('T67'),
  #          cod_1 = c('Effects of heat and light'),
  #          cod_code_2 = c('A06'),
  #          cod_2 = c('Dysentery Acute/Chronic'),
  #          cod_code_3 = c('G03'),
  #          cod_3 = c('Meningitis'),
  #          time_stamp = c("2021-02-16 09:46:39 EDT"))
} else {
  creds <- yaml::yaml.load_file('credentials/va_tool_credentials.yaml')
  psql_end_point = creds$e
  psql_user = creds$u
  psql_pass = creds$p
  con <- dbConnect(drv, dbname='vadb', host=psql_end_point, 
                   port=5432,
                   user=psql_user, password=psql_pass)
}

# Write table
dbWriteTable(conn = con, name = 'vatool_users', value = users, overwrite = TRUE)
# if(is_local){
#   dbWriteTable(conn = con, name = 'vatool_cods', value = cods, overwrite = TRUE)
# }

dbDisconnect(con)
