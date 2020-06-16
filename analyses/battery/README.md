Battery test
================

``` r
# Libraries
library(databrew)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(bohemia)
library(gsheet)
```

``` r
# Read in credentials
creds <- yaml::yaml.load_file('../../credentials/credentials.yaml')

# Sync traccar data
sync_workers_traccar(credentials = creds)

# Read in google data bout which tablets we're experimenting on
experiment <- gsheet2tbl('https://docs.google.com/spreadsheets/d/11T8oB0NBQWiWIymx8F1dBDOlri_YPZE0CO6FnJvg5uA/edit?usp=sharing')
```

``` r
# Get tablet data
df <- bohemia::get_traccar_data(url = creds$traccar_server,
                                user = creds$traccar_user,
                                pass = creds$traccar_pass)
# Keep only those in the experiment
keep <- df %>%
  mutate(bohemia_id = as.numeric(uniqueId)) %>%
  filter(bohemia_id %in% experiment$bohemia_id) %>%
  left_join(experiment %>% dplyr::select(bohemia_id, Interval)) %>%
  dplyr::select(bohemia_id, name, Interval, status, id)

# Get battery life
out_list <- list()
for(i in 1:nrow(keep)){
  this_id <- keep$id[i]
  out <- get_positions_from_device_id(
                                url = creds$traccar_server,
                                user = creds$traccar_user,
                                pass = creds$traccar_pass,
                                device_id = this_id)

  out_list[[i]] <- out
}
out <- bind_rows(out_list)
```

``` r
# Analyze
# Extract battery life
extract_battery <- function(x){
  a <- strsplit(x, ' ')
  a <- lapply(a, function(x){x[1]})
  a <- unlist(a)
  b <- strsplit(a, ':', fixed = T)
  b <- lapply(b, function(x){x[2]})
  b <- unlist(b)
  as.numeric(b)
}
out$battery <- extract_battery(out$valid)

# Join the Interval setting
out <- out %>%
  left_join(keep %>% dplyr::rename(deviceId = id))

# Plot
ggplot(data = out,
       aes(x = deviceTime,
           y = battery,
           color = factor(Interval))) +
  geom_line()
```

![](figures/unnamed-chunk-4-1.png)<!-- -->

# Technical details

This document was produced on 2020-05-24 on a Linux machine (release
5.3.0-53-generic. To reproduce, one should take the following steps:

  - Clone the repository at <https://github.com/databrew/bohemia>

  - “Render” (using `rmarkdown`) the code in
    `analysis/clustering/README.Rmd`

Any questions or problems should be addressed to <joe@databrew.cc>