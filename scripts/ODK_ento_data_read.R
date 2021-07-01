devtools::install_github("validmeasures/odkr")
library(odkr)
#library(bohemia)

work_dir <- getwd()
get_briefcase(destination = work_dir)
url <- "https://bohemia.systems"
user <- ""  # Please fill username and password here 
password <- ""
ids <- c('entoa1a2','entoa3','entob1b2','entoc1c2','entohhfollowup','entohhfirstvisit','entolivestock','entolivestockfollowup','entoscreening' ) 
#dowloadings forms and reating .csv files
for(i in 1:length(ids)){
  id <- ids[i]
  pull_remote(target = work_dir,
              id = id,
              to = work_dir,
              from = url,
              username = user,
              password = password)
  
  export_data(target = work_dir,
              id = id,
              from = work_dir,
              to = work_dir,
              filename = paste0(id, '.csv'))
}  
#reading .csv file to the memory  
data_ento <- list()
for(i in 1:length(ids)){
  file_name<-paste0(ids[i], '.csv')
  data_ento[[i]]=read.csv(file=file_name)
}
names(data_ento) <- ids  # giving names to the list items
str(data_ento)
