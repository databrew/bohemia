#' Format enumerations ODK data for the bohemia database
#' 
#' Take the enumerations data (as generated by "odk_get_data") and format it so it's ready for popping into the enumerations table of the bohemia database
#' @param data The list of dataframes (non_repeats and repeats) generated by odk_get_data for enumerations
#' @param keyfile The public key file to be used for encryption
#' @return A list of dataframes formatted and named in accordance with the bohemia database
#' @import dplyr
#' @export

format_enumerations <- function(data, keyfile){
  
  ## MAIN PART OF enumerations
  df <- data$non_repeats
  df <- df %>% dplyr::rename(instance_id = instanceID)
  names(df) <- tolower(names(df))
  # Remove all the notes
  df <- df[,!grepl('note_', names(df))]

  # Clean up some variables
  df <- df %>%
    # Hamlet and village clean-up
    mutate(hamlet = ifelse(!is.na(hamlet_other),
                           hamlet_other,
                              hamlet)) %>%
    mutate(village = ifelse(!is.na(village_other),
                            village_other,
                               village)) %>%
    dplyr::select(-hamlet_code_list,
                  -hamlet_code_not_list,
                  -hamlet_other,
                  -village_other,
                  -other_location) %>%
    # paint location cleanup
    mutate(localizacao_agregado = ifelse(!is.na(localizacao_agregado_free),
                                         localizacao_agregado_free,
                                         localizacao_agregado)) %>%
    dplyr::select(-localizacao_agregado_free) %>%
    # The below are select multiple, so it's theoretically possible to combine the other option
    mutate(construction_material = paste0(ifelse(!is.na(construction_material), construction_material, ''),
                                          ' ',
                                          ifelse(!is.na(construction_material_free), construction_material_free, ''))) %>%
    mutate(wall_material = paste0(ifelse(!is.na(wall_material), wall_material, ''),
                                          ' ',
                                          ifelse(!is.na(wall_material_free), wall_material_free, ''))) %>%
    dplyr::select(-wall_material_free,
                  -instancename,
                  -construction_material_free)
  df$hamlet_code_from_hhid <- df$hamlet_code_from_vizinho1 <- df$hamlet_code_from_vizinho2 <- NULL
  
  # Clean up characters in names
  df <- df %>%
    mutate(initials_chefe = unlist(lapply(lapply(strsplit(chefe_name, ' '), function(x){substr(x, 1, 1)}), function(y){toupper(paste0(y, collapse = ''))}))) %>%
    mutate(initials_sub = unlist(lapply(lapply(strsplit(sub_name, ' '), function(x){substr(x, 1, 1)}), function(y){toupper(paste0(y, collapse = ''))}))) %>%
    mutate(sub_name = encrypt_private_data(data = sub_name, keyfile = keyfile),
           chefe_name = encrypt_private_data(data = chefe_name, keyfile = keyfile))
  
  # NO REPEATS IN ENUMERATIONS
  # Return the formatted data
  out <- list(df)
  names(out) <- c('enumerations')
  # Clean up uuid column
  for(i in 1:length(out)){
    if('instance_id' %in% names(out[[i]])){
      out[[i]]$instance_id <- gsub('uuid:', '', out[[i]]$instance_id)
    }
    
  }
  return(out)
}