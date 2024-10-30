library(tidyverse)
library(readxl)
library(here)
library(stringr)

#Dataset will need to be manually downloaded and run through this cleaner every few months, process is not automated 
data_file_path <- here('data', 'illinois_tester_dataset.xlsx')
illinois_uncleaned <- read_excel(
  data_file_path, 
  sheet="Reports",
  col_type=c('skip','text','skip','skip', 'date', 'numeric','numeric','text','text','text','text','text','text'))

#Rename column name
#Begin cleaning time of death variable
#Standardize date of death variable
#Add system variable 
#Remove irrelevant columns
illinois_cleaned<-illinois_uncleaned |>
  rename(ind_tod = "Time of Death",
         ind_dod = "Date of Death",
         resp_agency_facility  = "Responsible Agency",
         ind_age = "Decedent Age at Death",
         ind_gender = "Decedent Sex",
         ind_race = "Decedent Race",
         ind_ethnicity = "Decedent Ethnicity",
         death_loc_type = "Type of Location Where Event Causing Death Occurred",
         death_type_cod = "Manner of Death",
         ind_death_desc = "Narrative Description: Details of Circumstances Leading to Death") |>
  mutate(ind_tod = str_replace(strftime(as.POSIXlt(ind_tod*24*3600)), '1970-01-01 ', ''),
         ind_tod = str_replace(ind_tod, '1969-12-31 ', ''), 
         ind_dod = str_c(str_split(ind_dod, '-')[[1]][2], str_split(ind_dod, '-')[[1]][3], str_split(ind_dod, '-')[[1]][1], sep = "/"),
         system = "IL") |>
  select(-c(death_loc_type, death_type_cod))

#Fix time of death variable, which is initially loaded in as a proportion of the day at which the time occurs
#Multiply by 24 and 3600 above to get back to military time 
#Convert from military time to format specified in Qualtrics: hh:ss a.m./hh:ss p.m.
illinois_cleaned_v2 <-illinois_cleaned |>
         mutate(ind_tod = case_when(
           is.na(ind_tod) ~ NA, 
           as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 1]) > 12 ~ 
             sprintf(
                "%02d:%02d p.m.", 
                as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 1]) - 12,
                as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 2])),
           as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 1]) <= 12 ~
             sprintf(
               "%02d:%02d a.m.", 
               ifelse(
                 as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 1]) == 0, 
                 12, 
                 as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 1])),
               as.numeric(str_split(ind_tod, ':', simplify = TRUE)[, 2])),
           ))

write.csv(illinois_cleaned_v2, 'data/il_cleaned_data.csv')


### Deprecated code attempting to use rvest to automate dataset download -- not working

#library(chromote)
#library(rvest)

#il_dataset_url = 'https://icjia.illinois.gov/researchhub/datasets/death-in-custody-reports/'

#il_rvest_live <- rvest::read_html_live(il_dataset_url)
#il_rvest_live$view()

#il_rvest_live |> html_elements("Button")
#il_rvest_live$click('Button[class="v-btn v-btn--is-elevated v-btn--has-bg theme--light v-size--default"', n_clicks = 1)
#il_rvest_live |> html_elements('Button[class="v-btn v-btn--is-elevated v-btn--has-bg theme--light v-size--default"')