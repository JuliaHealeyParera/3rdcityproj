#Loading libaries
library(tidyverse)
library(here)
library(survival)

#Loading data
data_file_path <- here('data', 'mturk_preliminary_results.csv')
prelim_data <- read_csv(data_file_path)

#determine concordance
#PILOT CONCORDANCE: 14.1844% for variable AGE 
#PILOT CONCORDANCE: 25.53191% for variable FIRST NAME 

sum <- prelim_data |>
  mutate(Answer.ind_first_name = tolower(Answer.ind_first_name)) |>
  group_by(Input.mturk_indiv_death_task_name) |>
  summarize(same_age = +(n_distinct(Answer.ind_age) == 1),
            same_first_name = +(n_distinct(Answer.ind_first_name) == 1)) |>
  summarize(concordance_same_age = sum(same_age) / n(),
            concordance_first_name = sum(same_first_name) / n())
