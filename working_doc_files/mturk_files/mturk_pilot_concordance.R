#Loading libaries
library(tidyverse)
library(here)
library(survival)
library(stringr)

#Loading data
data_file_path <- here('data', 'mturk_preliminary_results.csv')
prelim_data <- read_csv(data_file_path)

#determine concordance
#PILOT CONCORDANCE: 14.1844% for variable AGE 
#PILOT CONCORDANCE: 25.53191% for variable FIRST NAME 

conversion <- c(
  'january' = '1', 
  'jan' = '1', 
  'february' = '2', 
  'feb' = '2', 
  'march' = '3', 
  'mar' = '3',
  'april' = '4', 
  'may' = '5', 
  'june' = '6',
  'july' = '7',
  'august' = '8',
  'aug' = '8',
  'september' = '9',
  'sep' = '9',
  'october' = '10',
  'november' = '11',
  'december' = '12')

seps_conversion <- c(
  ', ' = '/', 
  '- ' = '/', 
  '\\. ' = '/',
  '-' = '/', 
  ' ' = '/',
  '\\.' = '/')

num_conversion <- c(
  '01/' = '1/',
  '02/' = '2/',
  '03/' = '3/',
  '04/' = '4/',
  '05/' = '5/',
  '06/' = '6/',
  '07/' = '7/',
  '08/' = '8/',
  '09/' = '9/'
)

fix_date = function(original) {
  replaced_case = tolower(original)
  replaced_month = str_replace_all(replaced_case, conversion)
  replaced_nums = str_replace_all(replaced_month, num_conversion)
  replaced_seps = str_replace_all(replaced_nums, seps_conversion)
  return(replaced_seps)
}

prelim_data <- prelim_data |>
  mutate(Answer.ind_first_name = tolower(Answer.ind_first_name),
         Answer.ind_race_eth = tolower(Answer.ind_race_eth),
         Answer.pr_date = map(Answer.pr_date, ~ fix_date(.x)),
         Answer.ind_dob_ymd = map(Answer.ind_dob_ymd, ~ fix_date(.x)))

sum <- prelim_data |>
  group_by(Input.mturk_indiv_death_task_name) |>
  summarize(same_age = +(n_distinct(Answer.ind_age) == 1),
            same_first_name = +(n_distinct(Answer.ind_first_name) == 1),
            same_race_eth = +(n_distinct(Answer.ind_race_eth) == 1),
            same_pr_date = +(n_distinct(Answer.pr_date) == 1),
            same_dob_date = +(n_distinct(Answer.ind_dob_ymd) == 1)) |>
  summarize(age = sum(same_age) / n(),
            first_name = sum(same_first_name) / n(),
            race_eth = sum(same_race_eth) / n(),
            pr_date = sum(same_pr_date) / n(),
            dob_date = sum(same_dob_date) / n()) |>
  pivot_longer(everything(), names_to = "variable", values_to = "concordance")  

