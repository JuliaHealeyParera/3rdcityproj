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
not_included <- c("HI", "AL", "GA", "AZ", "DE", "ID", "WA", "OK", "NM", "NH", "NV", "NE", "MN", "ICE", "FL", "TX", "AR", "MT")

#General sample of normal systems -- was originally an SRS but ended up including all observations because n < 200 after filtering
link_sample_no_instruc <- agg_data |>
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


#Joining system_instructions (handwritten in Excel then downloaded and uploaded as CSV file) to link sample
full_test_sample <- link_sample_no_instruc |> 
  left_join(system_instructions, by = join_by(c_system_abbr)) |>
  select(c_system_abbr, ind_name, ind_link, instructions_mike_post_meeting) |>
  rename(mturk_indiv_death_task_url = ind_link,
         system_abbr = c_system_abbr,
         sys_indiv_death_collect_instr = instructions_mike_post_meeting,
         mturk_indiv_death_task_name = ind_name)

#Writing complete file
write.csv(full_test_sample, 'data/mturk_individ_core_task_tbl_prototype_2.csv')

