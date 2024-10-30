library(tidyverse)
library(readxl)
library(here)
library(stringr)

#Will need to manually redownload dataset every few months and feed it back into script
data_file_path <- here('data', 'montana_uncleaned.csv')
montana_uncleaned <- read_csv(data_file_path)

#Renamed columns
#Standardized year of death into Qualtrics date of death format
#Added system variable
#Separated name into first and last -- MIDDLE STILL INCLUDED IN FIRST NAME need to fix
#Separated race and ethnicity into their own variables
#Filled in death location with facility abbreviation if empty
montana_cleaned <- montana_uncleaned |>
  rename(ind_yod = "Year of Death",
         facility = "Facility",
         ind_name = "Offender Name",
         ind_age = "Age At Death",
         ind_gender = "Sex",
         ind_race_ethnicity = "Race-Ethnicity",
         ind_cod = "General Cause of Death",
         death_loc = "Other Location") |>
  mutate(
    ind_dod = sprintf("xx/xx/%d", as.numeric(ind_yod)),
    system = "MT",
    ind_first_name = ifelse(str_detect(ind_name, ','),
                            str_replace(str_split(ind_name, ",", simplify = TRUE)[, 2], ' ', ''),
                            str_split(ind_name, ' ', simplify = TRUE)[, 2]),
    ind_last_name = ifelse(str_detect(ind_name, ','),
                           str_split(ind_name, ",", simplify = TRUE)[, 1],
                           str_split(ind_name, ' ', simplify = TRUE)[, 1]),
    ind_ethnicity = trimws(str_split(ind_race_ethnicity, ',', simplify = TRUE)[, 2]),
    ind_race = str_split(ind_race_ethnicity, ',', simplify = TRUE)[, 1],
    death_loc = ifelse(death_loc == "N/A",
                       facility,
                       death_loc)
    )

write.csv(montana_cleaned, 'data/mt_cleaned_data.csv')
 
###Code for parsing middle name, corrupted somehow--fix later

#ind_middle_name = ifelse(str_detect(ind_name, ','),
#                         ifelse(dim(str_split(trimws(str_split(ind_name, ',', simplify = TRUE)[, 2]), ' ', simplify = TRUE))[2] == 2,
#                                str_split(trimws(str_split(ind_name, ',', simplify = TRUE)[, 2]), ' ', simplify = TRUE)[, 2],
#                                NA),
#                         ifelse(dim(str_split(ind_name, ' ', simplify = TRUE))[2] == 3,
#                                str_split(ind_name, ' ', simplify=TRUE)[, 3],
#                                NA))



###Depracated code attempting to automate URL scraping -- does not work

#library(chromote)
#library(rvest)
#library(sys)

#mt_dataset_url = 'https://dataportal.mt.gov/t/COR/views/MortalityinCorrections_17005883769990/DatainListFormat?%3AshowAppBanner=false&%3Adisplay_count=n&%3AshowVizHome=n&%3Aorigin=viz_share_link&%3Aembed=y'

#mt_rvest_live <- rvest::read_html_live(mt_dataset_url)
#mt_rvest_live$view()

#Sys.sleep(5)

#mt_rvest_live |> html_elements("Button")
#mt_rvest_live$click("Button[class='fppw03o low-density'", n_clicks = 1)