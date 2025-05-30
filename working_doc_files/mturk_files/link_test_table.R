#Loading packages
library(splitstackshape)
library(tidyverse)
library(here)

#Loading in press release data
data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

#Loading in system instructions
data_file_path <- here('data', 'mturk_system_instructions.csv')
system_instructions <- read_csv(data_file_path)

#Systems NOT to include (invalid/missing links) and special systems (MUST be in sample)
not_included <- c("HI", "GA", "AZ", "DE", "ID", "WA", "OK", "NM", "NH", "NV", "NE", "MN", "ICE", "FL", "TX", "AR", "MT")
special_systems <- c("ICE", "AR", "TX", "FL", "MT")

#General sample of normal systems -- was originally an SRS but ended up including all observations because n < 200 after filtering
agg_data_srs <- agg_data |>
  separate(c_ind_dod_ymd, into = c("dod_year", "dod_month", "dod_day"), sep = "-") |>
  filter(
    dod_year == '2023',
    !(c_system_abbr %in% not_included),
    !is.na(c_system_abbr),
    !is.na(ind_link), 
    !is.na(ind_first) & !is.na(ind_last)
    ) |>
  mutate(ind_name = if_else(
    is.na(ind_middle), paste(ind_first, ind_last, sep = ' '), paste(ind_first, ind_middle, ind_last, sep = ' ')
  )) |>
  select(c_system_abbr, ind_name, ind_link) 

#Creating stratified sample of special systems (require specific instructions/multiple steps)
agg_data_strat <- agg_data |>
  separate(c_ind_dod_ymd, into = c("dod_year", "dod_month", "dod_day"), sep = "-") |>
  filter(
    dod_year == '2023',
    c_system_abbr %in% special_systems, 
    !is.na(ind_link), 
    !is.na(ind_first) & !is.na(ind_last)
    ) |>
  mutate(ind_name = if_else(
    is.na(ind_middle), paste(ind_first, ind_last, sep = ' '), paste(ind_first, ind_middle, ind_last, sep = ' ')
  )) |>
  select(c_system_abbr, ind_name, ind_link) 

#5 observations from each special system MAX, some states had n < 5 observations 
stratified_sample <- stratified(agg_data_strat, 'c_system_abbr', 5)

#Joining normal states to special
link_sample_no_instruc <- full_join(agg_data_srs, stratified_sample, by = join_by(c_system_abbr)) 

#Joining system_instructions (handwritten in Excel then downloaded and uploaded as CSV file) to link sample
full_test_sample <- link_sample_no_instruc |> 
  left_join(system_instructions, by = join_by(c_system_abbr)) |>
  mutate(ind_name.x = ifelse(is.na(ind_name.x), ind_name.y, ind_name.x),
         ind_link.x = ifelse(is.na(ind_link.x), ind_link.y, ind_link.x)) |>
  rename(ind_name = ind_name.x,
         ind_link = ind_link.x) |>
  select(c_system_abbr, ind_name, ind_link, instructions_mike_post_meeting) |>
  mutate(ind_link = case_when(
    c_system_abbr == "FL" ~ "https://www.fdc.myflorida.com/statistics-and-publications/inmate-mortality",
    c_system_abbr == "AL" ~ "https://doc.alabama.gov/News",
    TRUE ~ ind_link)) |>
  rename(mturk_indiv_death_task_url = ind_link,
         system_abbr = c_system_abbr,
         sys_indiv_death_collect_instr = instructions_mike_post_meeting,
         mturk_indiv_death_task_name = ind_name)

#Writing complete file
write.csv(full_test_sample, 'data/mturk_individ_core_task_tbl.csv')

