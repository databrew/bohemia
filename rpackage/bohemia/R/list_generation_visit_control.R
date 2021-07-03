#' Visit control sheet list generation
#' 
#' Render a visit control sheet
#' @param census_data A dataframe in the format of the minicensus (household level)
#' @param census_people A dataframe in the format of the minicensus people table
#' @param census_subs A dataframe in the format of the
#' @param agg_list A list of dataframes including: refusalsabsences, enumerationscensus, va153census
#' @param keyfile The path to the private key
#' @param keyfile_public The path to the public key
#' @param location A three letter location code (or character vector of multiple codes). If NULL, all locations to be used
#' @param output_file The name of the spreadsheet to be written (csv file); if null, will return the table in memory
#' @param fake_data Whether to use fake data or not
#' @param html Whether to return a print-ready html table (intead of the default csv)
#' @param add_ids How many additional (non-used) IDs to add for the hamlet in question. By default, NULL (meaning none)
#' @import dplyr
#' @import PKI
#' @import readr
#' @import rmarkdown
#' @return A table will be created
#' @export

list_generation_visit_control <- function(census_data,
                                          census_people,
                                          census_subs,
                                          agg_list,
                                          keyfile = NULL,
                                          keyfile_public = NULL,
                                          location = NULL,
                                          output_file = NULL,
                                          fake_data = FALSE,
                                          html = FALSE,
                                          add_ids = NULL){
  
  # # REMOVE THE BELOW
  # keyfile <- '../../../credentials/bohemia_priv.pem'
  # keyfile_public <- '../../../credentials/bohemia_pub.pem'
  # library(dplyr)
  # library(PKI)
  # library(bohemia)
  
  # Needs to conform to these specs:
  # https://docs.google.com/document/d/1AB4AuEiMwPQdBIQvrWaus1XY36gFGmtZ4f65kzgYeug/edit?ts=6087c05d&pli=1
  
  # Deal with keyfile
  if(is.null(keyfile)){
    stop('You must specify the path to a private key (pem file) for decrypting names.')
  }
  if(is.null(keyfile_public)){
    stop('You must specify the path to a public key (pem file) for encrypting names.')
  }
  
  # Extract refusals
  ref <- agg_list$refusalsabsences
  
  # Deal with location
  locs <- bohemia::locations
  if(!is.null(location)){
    # Filter down the locations
    locs <- locs %>% filter(code %in% location)
    # Filter down the census data (happens later)
    # Filter down the refusals / absences
    ref <- ref %>%
      filter(`group_location-hh_hamlet_code` %in% location)
    # Get enumerations
    country <- locs$Country[1]
    if(country == 'Mozambique'){
      enum <- agg_list$enumerationscensus %>%
        filter(!substr(`group_location-agregado`, 1, 3) %in% location)
    }
    
  }
  if(nrow(locs) < 1){
    stop('No data found for the specified locations.')
  }
  
  if(fake_data){
    ################################# Create dummy data #########################################
    # Encrypt the names
    the_names <- c('John Doe', 'Jane Doe', 'Tarzan Jungle', 'Bob Smith', 'Alice Wonderland')
    the_names_encrypted <- encrypt_private_data(data = the_names, keyfile = keyfile_public)
    # Decrypt the names
    the_names <- decrypt_private_data(data = the_names_encrypted, keyfile = keyfile)
    
    # Create some household IDs for the geographies in question
    indices <- sample(1:nrow(locs), length(the_names), replace = TRUE)
    codes <- locs$code[indices]
    hhids <- paste0(codes, '-', sample(c('123','234', '345', '456'), length(the_names), replace = TRUE))  
    df <- locs[indices,] %>% dplyr::select(District, Ward, Village, Hamlet)
    df$hhid <- hhids
    df$hh_head_name <- sample(the_names, nrow(df), replace = TRUE)
    df$hh_sub_name <- sample(the_names, nrow(df), replace = TRUE)  
    get_contact <- function(){ paste0(sample(0:9, 8, replace = T), collapse = '')}
    make_contact <- function(n){out <- c(); for(i in 1:n){out[i] <- get_contact()};return(out)}
    df$contact_information <- make_contact(n = nrow(df))
    df$previous_attempts <- 0
    df <- df %>% arrange(hhid) %>%
      dplyr::select(hhid, hh_head_name, hh_sub_name,
                    District, Ward, Village, Hamlet, contact_information, previous_attempts)
    
  } else {
    
    # Get the right-hand side for previous attempts
    right_absences <- ref %>%
      filter(`group_census-ra` == 'absence') %>%
      filter(`group_description-activity` == 'census') %>%
      group_by(hhid = `group_description-hh_id`) %>%
      summarise(previous_attempts = n())
    right_refusals <- ref %>%
      filter(`group_census-ra` == 'refusal') %>%
      group_by(hhid = `group_description-hh_id`) %>%
      summarise(refusal = TRUE)
      
    
    df <- census_data %>%
      filter(hh_hamlet_code %in% locs$code) %>%
      dplyr::rename(hhid = hh_id) %>%
      arrange(hhid) %>%
      # Remove the refusals
      left_join(right_refusals) %>%
      filter(is.na(refusal)) %>% 
      # Get the count of previous absences
      left_join(right_absences) %>%
      mutate(previous_attempts = ifelse(is.na(previous_attempts), 0, previous_attempts)) %>%
      # mutate(previous_attempts = 0) %>% # placeholder
      dplyr::select(hhid,
                    District = hh_district, 
                    Ward = hh_ward, 
                    Village = hh_village, 
                    Hamlet = hh_hamlet, 
                    contact_information = hh_contact_info_number,
                    previous_attempts,
                    instance_id)
    
    # Retrieve the household head and sub names
    hhh <- census_data %>%
      mutate(hh_head_id = as.numeric(as.character(hh_head_id))) %>%
      dplyr::select(hh_head_id,
                    instance_id) %>%
      left_join(census_people %>%
                  mutate(num = as.numeric(as.character(num))) %>%
                  dplyr::select(instance_id, hh_head_id = num,
                                first_name,
                                last_name,
                                pid)) %>%
      mutate(hh_head_name = paste0(first_name, ' ', last_name, ' ', pid)) %>%
      dplyr::select(instance_id, hh_head_name)
    
    hhs <- census_subs %>%
      mutate(hh_sub_id = as.numeric(as.character(hh_sub_id))) %>%
      filter(!is.na(hh_sub_id)) %>%
      dplyr::select(hh_sub_id,
                    instance_id) %>%
      left_join(census_people %>%
                  mutate(num = as.numeric(as.character(num))) %>%
                  dplyr::select(instance_id, hh_sub_id = num,
                                first_name,
                                last_name,
                                pid)) %>%
      mutate(hh_sub_name = paste0(first_name, ' ', last_name, ' ', pid)) %>%
      dplyr::select(instance_id, hh_sub_name) %>%
      group_by(instance_id) %>%
      summarise(hh_sub_name = paste0(hh_sub_name, collapse = '; '))
    
    # Join hh head and subs to main table
    df <- left_join(df, hhh) %>%
      left_join(hhs) %>%
      arrange(hhid) %>%
      dplyr::select(hhid, hh_head_name, hh_sub_name,
                    District, Ward, Village, Hamlet, contact_information, previous_attempts)
  }
  
  
  # Add ids
  if(!is.null(add_ids)){
    # Get the maximum ID for each hamlet
    max_ids <- df %>%
      mutate(code = substr(hhid, 1, 3)) %>%
      arrange(hhid) %>%
      group_by(code) %>%
      summarise(max_id = dplyr::last(hhid))
    # For each row, get the new ones
    out_list <- list()
    for(i in 1:nrow(max_ids)){
      this_max_id <- max_ids$max_id[i]
      max_number <- as.numeric(substr(this_max_id, 5,7))
      this_code <- max_ids$code[i]
      new_ids <- (max_number + 1):(max_number + add_ids)
      out_df <- tibble(hhid = paste0(this_code, '-', new_ids))
      out_list[[i]] <- out_df
    }
    new_df <- bind_rows(out_list)
    # Join to empty columns
    right <- df[0,]
    for(j in 1:ncol(right)){
      this_column_name <- names(right)[j]
      if(!this_column_name %in% c('hhid', 'District', 'Ward', 'Village', 'Hamlet')){
        new_df[,this_column_name] <- NA
      }
    }
    # Get geographic information
    new_df <- left_join(
      new_df %>% mutate(code = substr(hhid, 1, 3)),
      bohemia::locations %>% dplyr::select(code, District, Ward, Village, Hamlet) 
    ) %>%
      dplyr::select(-code)
    # Add all together
    df <- bind_rows(
      df, 
      new_df
    ) %>%
      arrange(hhid)
  }
  
  # Add row number
  df$number <- 1:nrow(df)
  
  # Add date of generation
  df$date_generation <- as.character(Sys.Date())
  
  # Add observations
  df$observations <- ' '
  
  
  if(html){
    names(df) <- toupper(names(df))
    bohemia::prettify(df, nrows = nrow(df), download_options = TRUE)
  } else if(!is.null(output_file)){
    message('Writing a csv to ', output_file)
    write_csv(df, output_file)
  } else {
    return(df)
  }
}
