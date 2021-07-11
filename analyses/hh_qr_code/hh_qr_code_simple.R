
library(magick)
library(bohemia)
library(qrcode)
library(tidyverse)
library(gsheet)

# Define the country
country <- 'Tanzania'

# Read in minicensus data
file_name <- paste0('minicensus_data_', country, '.RData')
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

codes <- sort(unique(minicensus_data$minicensus_main$hh_id))

# codes <- paste0('ABC-', 111:222)

if(!dir.exists('certificates_simple')){
  dir.create('certificates_simple/')
}

for(i in 1:length(codes)){
  message(i)
  this_code <- codes[i]
  out_name <- paste0(
    'certificates_simple/',
    this_code,
    '.pdf'
  )
  if(!file.exists(out_name)){
    hh_qr_code_print_simple(hh_id = this_code,
                            save_file = out_name,
                            height=3.5,
                            width=3)
    dev.off()
  }  
}

# Get the "extra" 50 code margin
codes_df <- tibble(code = codes) %>%
  mutate(hamlet_code = substr(code, 1, 3)) %>%
  mutate(number = substr(code, 5,7)) %>%
  mutate(number = as.numeric(number)) %>%
  group_by(hamlet_code) %>%
  summarise(max_number = max(number, na.rm = TRUE))

hamlet_codes <- sort(unique(substr(codes, 1, 3)))
out_list <- list()
for(i in 1:nrow(codes_df)){
  this_code <- codes_df$hamlet_code[i]
  max_number <- codes_df$max_number[i]
  new_numbers <- (max_number + 1):(max_number + 50)
  new_codes <- paste0(this_code, '-', new_numbers)
  out_df <- tibble(code = new_codes)
  out_list[[i]] <- out_df
}
new_codes_df <- bind_rows(out_list)
codes <- new_codes_df$code

for(i in 1:length(codes)){
  message(i)
  this_code <- codes[i]
  out_name <- paste0(
    'certificates_simple/',
    this_code,
    '.pdf'
  )
  if(!file.exists(out_name)){
    hh_qr_code_print_simple(hh_id = this_code,
                            save_file = out_name,
                            height=3.5,
                            width=3)
    dev.off()
  }  
}



# Combine to pp per page
pp <- 12
setwd('certificates_simple')
dir.create('to_print')
files <- dir()
files <- files[grepl('.pdf', files)]
n <- numbers <- length(files)
ends <- (1:n)[1:n %% pp == 0]
starts <- ends - (pp-1)

for(i in 1:length(starts)){
  this_start <- starts[i]
  this_end <- ends[i]
  these_numbers <- this_start:this_end
  # these_numbers <- add_zero(these_numbers, 3)
  these_files <- paste0(files[these_numbers])
  file_string <- paste0(these_files, collapse = ' ')
  out_file <- gsub('.pdf', '', file_string)
  out_file <- gsub(' ', '', out_file)
  out_file <- paste0('to_print/', out_file, '.pdf')
  command_string <- paste0('pdfjam ', file_string,
                           " --nup 4x3 --landscape --trim '-1cm -1cm -1cm -1cm' --frame true --outfile ",
                           out_file)
  system(command_string)
}
setwd('to_print')
system('pdftk *.pdf cat output all.pdf')
setwd('..')
setwd('..')

