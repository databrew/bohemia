# Libraries
library(readr)
library(aws.s3)
library(dplyr)
library(tidyr)
library(stringr)
library(sp)
library(bohemia)


# Define some parameters
country <- 'Tanzania'
iso <- tolower(substr(country, 1, 3))
use_real_names <- TRUE
keyfile = '../credentials/bohemia_priv.pem'
keyfile_public = '../credentials/bohemia_pub.pem'

# Read in credentials for S3 bucket
s3creds <- read_csv('../../credentials/bohemiacensuss3credentials.csv')


# Set environment variables for AWS s3
Sys.setenv(
  "AWS_ACCESS_KEY_ID" = s3creds$`Access key ID`,
  "AWS_SECRET_ACCESS_KEY" = s3creds$`Secret access key`,
  "AWS_DEFAULT_REGION" = "eu-west-3"
)


buck <- get_bucket(bucket = 'bohemiacensus')


# Define some parameters
kf <- '../../credentials/bohemia_priv.pem' #path to private key for name decryption
creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')
is_linux <- Sys.info()['sysname'] == 'Linux'
keyfile = '../../../credentials/bohemia_priv.pem'
keyfile_public = '../../../credentials/bohemia_pub.pem'
output_file = '/home/joebrew/Desktop/visit_control_sheet.csv'

# Check the directory
this_dir <- getwd()
split_dir <- unlist(strsplit(this_dir, split = '/'))
check <- split_dir[length(split_dir)] == 'visit_control' & split_dir[length(split_dir) - 1] == 'scripts' & split_dir[length(split_dir) - 2] == 'bohemia'
if(!check){
  message('YOU ARE IN THE WRONG DIRECTORY. MAKE SURE YOU ARE IN bohemia/scripts/visit_control')
}

# Retrieve objects from s3
buck_names <- buck_times <-  c()
for(i in 1:length(buck)){
  buck_names[i] <- buck[i]$Contents$Key
  buck_times[i] <- buck[i]$Contents$LastModified
}
buck_df <- tibble(file = buck_names,
                  date_time = buck_times) %>%
  mutate(type = ifelse(grepl('agg', file), 'Aggregate', 'Census'))
buck_df_keep <- buck_df %>%
  arrange(desc(date_time)) %>%
  filter(grepl(country, file)) %>%
  group_by(type) %>%
  filter(date_time == dplyr::first(date_time))

if(nrow(buck_df_keep) > 0){
  for(i in 1:nrow(buck_df_keep)){
    this_file <- buck_df_keep$file[i]
    this_object_name <- unlist(strsplit(this_file, '_'))[1]
    local_file <- paste0(this_object_name, '.RData')
    save_object(
      object = this_file,
      bucket = 'bohemiacensus',
      file = local_file)
    load(local_file)
    file.remove(local_file)
  }
}
# # The above loaded two objects:
# agg_list # the odk aggregate data
# data_list # the odk-x census data

message('Loading minicensus data')

# Read in minicensus data
file_name <- paste0(country, '_minicensus_data.RData')
if(file_name %in% dir()){
  load(file_name)
} else {
  minicensus_data <- load_odk_data(the_country = country,
                          credentials_path = '../../credentials/credentials.yaml', # request from Databrew
                          users_path = '../../credentials/users.yaml', # request from Databrew
                          efficient = FALSE)
  save(minicensus_data,
       file = file_name)
}
out_list <- minicensus_data

## We now have three objects in memory
# agg_list # the odk aggregate data
# data_list # the odk-x census data
# out_list # the minicensus data


# Decrypt names
if(use_real_names){
  # data_list$hh_member$name <- decrypt_private_data(data_list$hh_member$name,
  #                                                  keyfile = kf)
  if(country == 'Mozambique'){
    out_list$enumerations$sub_name <- decrypt_private_data(out_list$enumerations$sub_name, keyfile = kf)
    out_list$enumerations$chefe_name <- decrypt_private_data(out_list$enumerations$chefe_name, keyfile = kf)  
  }
  
  out_list$minicensus_repeat_death_info$death_name <- decrypt_private_data(out_list$minicensus_repeat_death_info$death_name, keyfile = kf)
  out_list$minicensus_repeat_death_info$death_surname <- decrypt_private_data(out_list$minicensus_repeat_death_info$death_surname, keyfile = kf)
  out_list$minicensus_people$first_name <- decrypt_private_data(out_list$minicensus_people$first_name, keyfile = kf)
  out_list$minicensus_people$last_name <- decrypt_private_data(out_list$minicensus_people$last_name, keyfile = kf)
  out_list$va$id10007 <- decrypt_private_data(out_list$va$id10007, keyfile = kf)
  out_list$va$id10017 <- decrypt_private_data(out_list$va$id10017, keyfile = kf)
  out_list$va$id10018 <- decrypt_private_data(out_list$va$id10018, keyfile = kf)
  out_list$va$id10061 <- decrypt_private_data(out_list$va$id10061, keyfile = kf)
  out_list$va$id10062 <- decrypt_private_data(out_list$va$id10062, keyfile = kf)
} else {
  out_list$enumerations$sub_name <- fake_names(length(out_list$enumerations$sub_name))
  out_list$enumerations$chefe_name <- fake_names(length(out_list$enumerations$chefe_name))
  out_list$minicensus_repeat_death_info$death_name <- fake_names(length(out_list$minicensus_repeat_death_info$death_name), words = 1)
  out_list$minicensus_repeat_death_info$death_surname <- fake_names(length(out_list$minicensus_repeat_death_info$death_surname), words = 1)
  out_list$minicensus_people$first_name <- fake_names(length(out_list$minicensus_people$first_name), words = 1)
  out_list$minicensus_people$last_name <- fake_names(length(out_list$minicensus_people$last_name), words = 1)
  out_list$va$id10007 <- fake_names(length(out_list$va$id10007))
  out_list$va$id10017 <- fake_names(length(out_list$va$id10017))
  out_list$va$id10018 <- fake_names(length(out_list$va$id10018))
  out_list$va$id10061 <- fake_names(length(out_list$va$id10061))
  out_list$va$id10062 <- fake_names(length(out_list$va$id10062))
}

# Update names of the minicensus data object
for(i in 1:length(names(out_list))){
  this_name <- names(out_list)[i]
  if(grepl('minicensus_', this_name)){
    new_name <- gsub('minicensus_', 'clean_minicensus_', this_name)
    names(out_list)[i] <- new_name
  }
}

# Create objects required for visit control sheet
census_data <- out_list$clean_minicensus_main
census_people <- out_list$clean_minicensus_people
census_subs <- out_list$clean_minicensus_repeat_hh_sub


# Generate the visit control sheet
df <- list_generation_visit_control(census_data = census_data,
                              census_people = census_people,
                              census_subs = census_subs,
                              agg_list = agg_list,
                              keyfile = keyfile,
                              keyfile_public = keyfile_public,
                              location = NULL, #c('NOR', 'CHM', 'BBB', 'AGO', 'CHT'), # set to null in order to do all locations
                              output_file = NULL, #output_file, # set to null in order to return a dataframe in memory
                              fake_data = FALSE,
                              add_ids = NULL, # set as a number to add additional empty ids
                              html = FALSE) # set to TRUE if you want an html table ready for printing


# Needs to conform to these specs: https://docs.google.com/document/d/1AB4AuEiMwPQdBIQvrWaus1XY36gFGmtZ4f65kzgYeug/edit?ts=6087c05d&pli=1
