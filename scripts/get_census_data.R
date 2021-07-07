# Define the country
country <- 'Mozambique'
iso <- tolower(substr(country, 1, 3))

library(readr)
library(aws.s3)
library(dplyr)
library(tidyr)
library(stringr)
library(sp)
library(babynames)
library(bohemia)

# Configure AWS bucket info

# Read in credentials for S3 bucket
s3creds <- read_csv('../credentials/bohemiacensuss3credentials.csv')

# Read in credentials for ODK Aggregate server
odk_collect_creds <- yaml::yaml.load_file('../credentials/credentials.yaml')

# Read in credentials for ODK X server
xcreds <- list(user = odk_collect_creds$odkx_user,
               pass = odk_collect_creds$odkx_pass,
               server = odk_collect_creds$odkx_server)

# Set environment variables for AWS s3
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = s3creds$`Access key ID`,
  "AWS_SECRET_ACCESS_KEY" = s3creds$`Secret access key`,
  "AWS_DEFAULT_REGION" = "eu-west-3"
)

# # Get bucket details
# get_bucket(bucket = 'bohemiacensus')

# Define some parameters for ODK-X retrieval
suitcase_dir <- '/home/joebrew/Documents/suitcase'
briefcase_dir <- '/home/joebrew/Documents/briefcase'
briefcase_storage_dir <- getwd()#briefcase_dir
jar_file <- 'ODK-X_Suitcase_v2.1.8.jar'
jar_file_briefcase <- 'ODK-Briefcase-v1.18.0.jar'
odkx_path <- '/home/joebrew/Documents/bohemia/odkx/app/config' # must be full path!
kf <- '../credentials/bohemia_priv.pem' #path to private key for name decryption
use_real_names <- TRUE # whether to decrypt names (TRUE) or use fakes ones (false)
is_linux <- Sys.info()['sysname'] == 'Linux'
keyfile = '../credentials/bohemia_priv.pem'
keyfile_public = '../credentials/bohemia_pub.pem'
download_dir <- getwd()

# Check the directory
this_dir <- getwd()
split_dir <- unlist(strsplit(this_dir, split = '/'))
check <- split_dir[length(split_dir)] == 'scripts' & split_dir[length(split_dir) - 1] == 'bohemia' 
if(!check){
  message('YOU ARE IN THE WRONG DIRECTORY. MAKE SURE YOU ARE IN bohemia/scripts')
}

# Retrieve the ODK Aggregate forms
url <- odk_collect_creds[paste0(iso, '_odk_server')]
user <- odk_collect_creds[paste0(iso, '_odk_user')]
password <- odk_collect_creds[paste0(iso, '_odk_pass')]

# Function for pulling briefcase stuff
get_data_briefcase <- function(url,
                               id,
                               user,
                               password,
                               briefcase_dir,
                               jar_file_briefcase,
                               briefcase_storage_dir,
                               dry_run = FALSE){
  owd <- getwd()
  setwd(briefcase_dir)
  cli_text <- paste0(
    'java -Xms512m -Xmx16g -jar ',
    jar_file_briefcase, ' --pull_aggregate',
    ' -U ', url,
    ' -u ', user,
    ' -p ', password,
    ' -id ', id,
    ' -sd ', briefcase_storage_dir,
    ' -e ',
    ' -ed ', briefcase_storage_dir,
    ' -ef ', paste0(id, '.csv')
  )
  message(cli_text)
  if(!dry_run){
    system(cli_text)
  }
  setwd(owd)
}

# Get refusals, etc.
ids <- c('refusalsabsences', 'enumerationscensus', 'va153census', 'passivemalariasurveillancemoz')
for(i in 1:length(ids)){
  id <- ids[i]
  get_data_briefcase(
    url = url,
    id = id,
    user = user,
    password = password,
    briefcase_dir = briefcase_dir,
    briefcase_storage_dir = briefcase_storage_dir,
    jar_file_briefcase = jar_file_briefcase,
    dry_run = FALSE
  )
}

# Read in all tables for ODK Aggregate
# (eventually will need to adjust for repeats)
agg_list <- list()
for(i in 1:length(ids)){
  this_id <- ids[i]
  this_path <- paste0(file.path(briefcase_storage_dir, this_id), '.csv')
  this_data <- read_csv(this_path)
  agg_list[[i]] <- this_data
}
names(agg_list) <- ids



# Define tables to be retrieved
table_names <- c('census',
                 'ento_household',
                 'ento_livestock', 
                 'ento_mosquito_net',
                 'fw_location',
                 'hh_death',
                 'hh_geo_location',
                 'hh_latrine',
                 'hh_member',
                 'hh_mosquito_net',
                 'hh_travel',
                 'hh_water_body')
data_list <- list()
for(i in 1:length(table_names)){
  try({
    this_table <- table_names[i]
    odkx_retrieve_data(suitcase_dir = suitcase_dir,
                       jar_file = jar_file,
                       server_url = xcreds$server,
                       table_id = this_table,
                       user = xcreds$user,
                       pass = xcreds$pass,
                       is_linux = is_linux,
                       download_dir = download_dir,
                       attachments = FALSE, 
                       dry_run = TRUE)
    Sys.sleep(1)
    df <- readr::read_csv(paste0('default/',
                                 this_table,
                                 '/link_unformatted.csv'))
    data_list[[i]] <- df
  })

}
names(data_list) <- table_names
# save.image('~/Desktop/joe.RData')

## Encrypt names
# data_list$hh_member$name <- encrypt_private_data(
#   data = data_list$hh_member$name,
#   keyfile = keyfile_public)
# data_list$hh_death$hh_death_name <- 
#   encrypt_private_data(
#     data = data_list$hh_death$hh_death_name,
#     keyfile = keyfile_public)
# data_list$hh_death$hh_death_surname <- 
#   encrypt_private_data(
#     data = data_list$hh_death$hh_death_surname,
#     keyfile = keyfile_public)


# Put the objects into S3
temp_dir <- tempdir()

# ODK-X
file_name <- paste0('census_', country, '_',
                    as.character(as.character(Sys.time())),
                    '.RData')
full_path <- file.path(temp_dir, file_name)
save(data_list, file = full_path)
put_object(
  file = full_path,
  object = file_name,
  bucket = "bohemiacensus"
)

# ODK AGG
file_name <- paste0('agg_', country, '_',
                    as.character(as.character(Sys.time())),
                    '.RData')
full_path <- file.path(temp_dir, file_name)
save(agg_list, file = full_path)
put_object(
  file = full_path,
  object = file_name,
  bucket = "bohemiacensus"
)

buck <- get_bucket(bucket = 'bohemiacensus')
message(length(buck), ' objects in the bucket')


