library(tidyverse)
library(here) 

data_file_path <- here('data', 'aggregate_data.csv')
data <- read_csv(data_file_path)

illinois_file_path <- here('data', 'IL_dataset.xlsx')
illinois <- read_csv(illinois_file_path)

##incident_date variable unclear

illinois |>
  rename(agency_name = "Responsible Agency",
         agency_type = "Agency Type",
         incident_date = "Incident Date",
         dod = "Date of Death",
         age = "Decedent Age at Death",
         sex = "Decedent Sex",
         race = "Decedent Race",
         ethnicity = "Decedent Ethnicity",
         loc_type = )

#psuedo code 


hawaii <- data |>
  filter(c_system_abbr == "HI") |>
  select(ind_first, ind_last, c_ind_first, c_ind_last, c_ind_dod_ymd)