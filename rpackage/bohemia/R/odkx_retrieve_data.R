#' Visit control sheet list generation
#' 
#' Render a visit control sheet
#' @param suitcase_dir The directory of the suitcase jar file
#' @param jar_file The name of the suitcase jar file (without the full path)
#' @param server_url The url of the ODK-X server, including the https prefix
#' @param table_id The name of the table to be downloaded
#' @param user Username
#' @param pass Password
#' @param is_linux Whether or not on a linux system
#' @param download_dir Path to download the data; if NULL, will download to the directory of the jar file by default
#' @param attachments Boolean; whether or not to download attachments too (default is TRUE)
#' @param dry_run Whether to carry out
#' @import dplyr
#' @import readr
#' @return A table will be created
#' @export

odkx_retrieve_data <- function(suitcase_dir, jar_file, server_url, table_id, user, pass, is_linux = FALSE, download_dir = NULL, attachments = TRUE, dry_run = FALSE){
  owd <- getwd()
  setwd(suitcase_dir)
  
  update_string <- 
    push_text <- paste0(
      "java -Xms512m -Xmx16g -jar ", jar_file, " -cloudEndpointUrl '", server_url, "' -appId 'default' -tableId '", table_id, "' ", ifelse(attachments, "-a", ""),  " -username '", user, "' -password '", pass, "' -download ", ifelse(!is.null(download_dir), paste0("-path ", download_dir), ""),  " -dataVersion 2")
  message(update_string)
  if(!is_linux){
    update_string <- gsub("'", "", update_string)
  }
  if(dry_run){
    message(update_string)
  } else {
    system(update_string)
    setwd(owd)
    dd <- ifelse(is.null(download_dir), suitcase_dir, download_dir)
    message('DATA DOWNLOADED TO ', dd)
  }
  
}

# java -jar ODK-X_Suitcase_v2.1.7.jar -cloudEndpointUrl 'https://godatago.io' -appId 'default' -tableId 'census' -a -username 'eldo' -password 'thewolf' -download -path /home/joebrew/Desktop/odkx -dataVersion 2