library(splitstackshape)
library(tidyverse)

data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

not_included <- c("GA","AZ", "DE", "ID", "WA", "OK", "NM", "NH", "NV", "NE", "MN", "ICE", "FL", "TX", "AR", "MT")
special_systems <- c("ICE", "AR", "TX", "FL", "MT")

agg_data_srs <- agg_data |>
  filter(!(c_system_abbr %in% not_included),
         !is.na(ind_link), 
         !is.na(ind_first) & !is.na(ind_last)) |>
  mutate(ind_name = if_else(
    is.na(ind_middle), paste(ind_first, ind_last, sep = ' '), paste(ind_first, ind_middle, ind_last, sep = ' ')
  )) |>
  select(c_system_abbr, ind_name, ind_link) 

initial_sample <- sample_n(agg_data_srs, 250)

agg_data_strat <- agg_data |>
  filter(c_system_abbr %in% special_systems, 
         !is.na(ind_link), 
         !is.na(ind_first) & !is.na(ind_last)) |>
  mutate(ind_name = if_else(
    is.na(ind_middle), paste(ind_first, ind_last, sep = ' '), paste(ind_first, ind_middle, ind_last, sep = ' ')
  )) |>
  select(c_system_abbr, ind_name, ind_link) 

stratified_sample <- stratified(agg_data_strat, 'c_system_abbr', 5)

full_sample <- full_join(initial_sample, stratified_sample, by = join_by(c_system_abbr))

write.csv(full_sample, 'data/full_link_sample.csv')


         



