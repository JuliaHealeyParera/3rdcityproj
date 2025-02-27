library(tidyverse)
library(readxl)
library(rvest)
library(janitor)
library(gt)
library(gtsummary)
library(tidymodels)

# LIBS & FUNCTIONS ####
flexibly_clean_date = function(d){
  d = str_trim(d)
  date_to_return = case_when(d |> str_detect("^[0-9]{1,2}[/-][0-9]{2,4}$") ~ # e.g. "05/2017" month/year
                               paste0("01/", d) |> dmy(), #... 2017/05/01 (assume 1st of mo)
                             d |> str_detect("^[0-9]{4}$") ~ # e.g. 1972, exact year only
                               d |> paste0("0101") |> ymd(), # convert
                             (d |> str_detect("^[0-9]{5}$")) | (!is.na(as.numeric(d))) ~ # e.g. 44060 or 40282.35, Excel date epoch
                               janitor::excel_numeric_to_date(d |> as.numeric()), # convert
                             d |> str_detect("[0-9x]{1,2}[/-]0{1,2}[/-][0-9]{4}") ~ # "xx/00/2013" 
                               d |> str_replace_all("00", "01") |> mdy(), # remove 0s
                             d |> str_detect("^[0-9]{4}[/-][0-9]{1,2}[/-][0-9]{1,2}$") ~ # e.g. 1984/01/01, 2000
                               d |> ymd(), #... 2017/05/01 (assume 1st of Jan)
                             d |> str_detect("^[0-9]{2}[/-][0-9]{1,2}[/-][0-9]{4}$") ~ # 12/09/2003
                               d |> mdy(), 
                             d |> str_detect("^[0-9]{2}[/-][0-9]{1,2}[/-][0-9]{1,2}$") ~ # 12/09/03 (not best practice)
                               d |> mdy(), 
                             d |> str_detect("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$") ~ # 1/9/03 (not best practice)
                               d |> mdy(), 
                             d |> str_detect("^[1|2][0-9]{3}") ~ # year only e.g. 1984, 2000
                               paste0("01/01", d) |> dmy(), #... 2017/05/01 (assume 1st of Jan)
                             d |> str_detect("[0-9x]{1,2}[/-][0-9x]{1,2}[/-][0-9]{4}") ~ # "xx/xx/2013" 
                               d |> str_replace_all("xx", "01") |> mdy(), # remove xs
                             T ~ NA |> as_date()
                             
  )
  # Drop known and likely bad dates
  date_to_return = case_when(date_to_return > today() ~ NA_character_ |> ymd(),
                             date_to_return < ymd("1900/01/01") ~ NA_character_ |> ymd(), 
                             date_to_return == ymd("1111/11/11") ~ NA_character_ |> ymd(), 
                             T ~ date_to_return)
  return(date_to_return) 
}

#.####
# Read new, variable complete table ####
# Previous work rejoining tables is no longer obsolete; able to extract all fields
bja_tbl = read_csv("Mortality Data/Mortality Data from 2024 BJA Tables/full_bja_2024_data_all_fields.csv",
                   col_types = cols(.default = col_character())) |> clean_names()

#.####

# Cleaner data ####
cleaner_bja_tbl = bja_tbl

## age range ####
# Order the factor
cleaner_bja_tbl |> count(age_range)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(age_range = age_range |> ordered(levels = c("Under 18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+", "Unknown")))
cleaner_bja_tbl |> count(age_range)

## year_of_fiscal_year_of_death  ####
cleaner_bja_tbl |> count(year_of_fiscal_year_of_death)

## location_type  ####
# harmonize to title case
cleaner_bja_tbl |> count(location_type)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(location_type = location_type |> str_to_title())
cleaner_bja_tbl |> count(location_type)

## manner_of_death  ####
# Long manners of death (homicide, use of force) shortened for tabulation
cleaner_bja_tbl |> count(manner_of_death)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(manner_of_death = case_when(manner_of_death |> str_detect("Homicide") ~ "Homicide",
                                     manner_of_death |> str_detect("use of force") ~ "Use of Force",
                                     T ~ manner_of_death))
cleaner_bja_tbl |> count(manner_of_death)

## state ####
cleaner_bja_tbl |> count(state)

## concat id ####
# No changes; 
cleaner_bja_tbl |> count(concat_id) |> count(n) |> mutate(pct_nn = nn/sum(nn)*100)

## masked_count ####
cleaner_bja_tbl |> count(masked_count)

## age ####
cleaner_bja_tbl |> count(age) |> print(n=Inf)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(age = age |> as.integer())
cleaner_bja_tbl |> count(age) |> print(n=Inf)

## agency_name ####
cleaner_bja_tbl |> count(agency_name, sort = T) |> print(n=100)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(c_agency_name = case_when(agency_name |> str_to_upper() |> str_detect("^N/A$|^NA$|UNKNOWN|UKNOWN|NOT PROVIDED") ~ NA_character_,
                                   T ~ agency_name )) |> 
  mutate(c_agency_name = c_agency_name |> str_squish())
cleaner_bja_tbl |> count(agency_name, sort = T) |> print(n=100)
cleaner_bja_tbl |> count(c_agency_name, agency_name) |> filter(is.na(c_agency_name))
# NOTE: Could add lat-long for these.

## agency_end_date ####
cleaner_bja_tbl |> count(agency_end_date, sort = T) |> print(n=50)
# Yikes. Not cleaning for now.
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(c_agency_end_date = agency_end_date |> flexibly_clean_date())
cleaner_bja_tbl |> count(c_agency_end_date, sort = T)

## birth_year ####
cleaner_bja_tbl |> count(birth_year, sort = T) |> print(n=100)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(birth_year = case_when(birth_year == "9999" ~ NA_character_, T ~ birth_year)) |> 
  mutate(birth_year = birth_year |> ordered(levels = 1900:2030))
cleaner_bja_tbl |> count(birth_year) |> print(n=100)

## brief_circumstances ####
cleaner_bja_tbl |> count(brief_circumstances, sort = T) |> print(n=10)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(c_brief_circumstances = case_when(brief_circumstances |> str_to_lower() |> str_squish() |> str_detect("pending") ~ "Pending",
                                           brief_circumstances |> str_to_lower() |> str_squish() |> 
                                             str_detect("unknown|^na$|^n/a$|not available|unavailable|^\\*$|^none$|none provided|no additional information provided") ~ NA_character_,
                                           T ~ brief_circumstances))
cleaner_bja_tbl |> count(c_brief_circumstances, sort = T) |> print(n=100)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(c_has_circumstances = case_when(c_brief_circumstances == "Pending" ~ "Pending",
                                         c_brief_circumstances  |> is.na() ~ "No Circumstance Information",
                                         T ~ "Has Some Circumstance Narrative"))
cleaner_bja_tbl |> count(c_has_circumstances, sort = T) |> print(n=100)

## calendar_year_death ####
cleaner_bja_tbl |> count(calendar_year_death)

## city ####
cleaner_bja_tbl |> count(city, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(city = city |> str_to_title())
cleaner_bja_tbl |> count(city, sort = T)

## data_as_of ####
cleaner_bja_tbl |> count(data_as_of, sort = T)

## data_entry_status ####
cleaner_bja_tbl |> count(data_entry_status)

## data_entry_status ####
cleaner_bja_tbl |> count(date_of_death, sort = T) |> print(n=500)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(date_of_death = date_of_death |> mdy())
cleaner_bja_tbl |> count(date_of_death, sort = T)

## decedent ####
cleaner_bja_tbl |> count(decedent, sort = T) |> print(n=500)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(decedent = decedent |> str_to_title())
cleaner_bja_tbl |> count(decedent, sort = T) |> print(n=500)

## ethnicity ####
cleaner_bja_tbl |> count(ethnicity, sort = T)

## facility_type ####
cleaner_bja_tbl |> count(facility_type, sort = T)

## first_name ####
cleaner_bja_tbl |> count(first_name, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(first_name = first_name |> str_to_title())
cleaner_bja_tbl |> count(first_name, sort = T)

## fiscal_year_of_death ####
cleaner_bja_tbl |> count(fiscal_year_of_death, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(fiscal_year_of_death = str_sub(fiscal_year_of_death, 5, 8) |> as.integer())
cleaner_bja_tbl |> count(fiscal_year_of_death, sort = T)

## flag ####
cleaner_bja_tbl |> count(flag, sort = T) # Perhaps a Tableau variable

## flag ####
cleaner_bja_tbl |> count(flag_count, sort = T) # Perhaps a Tableau variable

## gender ####
cleaner_bja_tbl |> count(gender, sort = T)

## gender_open_text ####
cleaner_bja_tbl |> count(gender_open_text, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(c_gender_open_text = 
           case_when(gender_open_text |> str_to_title() |> str_detect("Unknown|Unkown|N/A") ~ "Unknown",
                     gender_open_text == "MALE" ~ NA_character_, # unnecessary given binary variable
                     gender_open_text |> str_detect("Transgender Male to Female|transgender woman") ~ "Transgender Woman", # unnecessary given binary variable
                     T ~ gender_open_text |> str_to_title()))
cleaner_bja_tbl |> count(c_gender_open_text, sort = T)

## grantee_legal_name ####
cleaner_bja_tbl |> count(grantee_legal_name, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(grantee_legal_name = grantee_legal_name |> str_to_title())
cleaner_bja_tbl |> count(grantee_legal_name, sort = T)

## location_of_death ####
cleaner_bja_tbl |> count(location_of_death, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(location_of_death = case_when(location_of_death |> str_detect("Unknown|^\\*$|^N/A$") ~ NA_character_,
                                       T ~ location_of_death))
cleaner_bja_tbl |> count(location_of_death, sort = T) |> print(n=20)

## location_of_death_state_postal_abbreviation ####
cleaner_bja_tbl |> count(location_of_death_state_postal_abbreviation, sort = T)
# TODO Read in our map file

## middle_name ####
cleaner_bja_tbl |> count(middle_name, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(middle_name = middle_name |> str_to_title())
cleaner_bja_tbl |> count(middle_name, sort = T)

## notes ####
cleaner_bja_tbl |> count(notes, sort = T)

## other_open_text ####
cleaner_bja_tbl |> count(other_open_text, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(other_open_text = other_open_text |> str_to_title())
cleaner_bja_tbl |> count(other_open_text, sort = T)

## pre_recode_type_of_death ####
cleaner_bja_tbl |> count(pre_recode_type_of_death, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> mutate(pre_recode_type_of_death = case_when(pre_recode_type_of_death |> str_to_title() == "Unknown" ~ "Unknown",
                                                                                 T ~ pre_recode_type_of_death))
cleaner_bja_tbl |> count(pre_recode_type_of_death, sort = T)

## race ####
cleaner_bja_tbl |> count(race, sort = T)

## recode ####
cleaner_bja_tbl |> count(recode, sort = T)

## status_please_use_this... ####
cleaner_bja_tbl |> count(status_please_use_this_column_to_track_any_updates_you_can_make_to_these_records, sort = T)

## street_address ####
cleaner_bja_tbl |> count(street_address, sort = T)

## time_of_death ####
cleaner_bja_tbl |> count(time_of_death, sort = T)
cleaner_bja_tbl = cleaner_bja_tbl |> 
  mutate(time_of_death = case_when(time_of_death |> str_detect("00:00|00:01") ~ NA_character_,
                                   T ~ time_of_death))
cleaner_bja_tbl |> count(time_of_death, sort = T)

## zip_code ####
cleaner_bja_tbl |> count(zip_code, sort = T)

## Write table ####
cleaner_bja_tbl |> write_csv("Mortality Data/Mortality Data from 2024 BJA Tables/cleaner_bja_tbl.csv")

## Write metadata table ####
field_meta_data_tbl = tibble(variable_name = names(cleaner_bja_tbl), 
                             type = map_chr(cleaner_bja_tbl, ~ .x |> class() |> first()))
field_meta_data_tbl
field_meta_data_tbl |> write_csv("Mortality Data/Mortality Data from 2024 BJA Tables/derived_bja_2024_field_meta_data_tbl.csv")


#.####
cleaner_bja_tbl
field_meta_data_tbl |> print(n=Inf)
field_meta_data_tbl |> pull(variable_name) |> paste(collapse = " ")
# "          "
# Demographic table: 
# age_range

# Submit table
# year_of_fiscal_year_of_death

## Tables ####

### Drop: ####
# concat_id masked_count agency_end_date data_as_of
# data_entry_status  notes status_please_use_this_column_to_track_any_updates_you_can_make_to_these_records
# decedent first_name flag flag_count middle_name 
# street_address
# c_agency_end_date

cleaner_bja_tbl |> count(recode)

### Demographics ####
bja_gt1 = cleaner_bja_tbl |> 
  select(age_range, # age, 
         # birth_year, 
         gender, # c_gender_open_text, 
         race, ethnicity) |> 
  tbl_summary(sort = list(gender ~ "frequency", race ~ "frequency", ethnicity ~ "frequency")) |> # sort = all_categorical(FALSE) ~ "frequency"
  as_gt() |> tab_header(title = "Decedent Demographics")
bja_gt1
bja_gt1 |> gt::gtsave("Mortality Data/Mortality Data from 2024 BJA Tables/bja_gt1.html")

### Death characteristics ####
bja_gt2 = cleaner_bja_tbl |> 
  select(manner_of_death, pre_recode_type_of_death, c_has_circumstances, 
         #brief_circumstances, # narrative information
         # city, 
         calendar_year_death, facility_type, 
         fiscal_year_of_death, 
         # location_of_death, # specific location
         # other_open_text, # specific
         # time_of_death, # specific time of death
         date_of_death) |> 
  tbl_summary(sort = all_categorical(FALSE) ~ "frequency") |> 
  as_gt() |> tab_header(title = "Death Characteristics")
bja_gt2
bja_gt2 |> gt::gtsave("Mortality Data/Mortality Data from 2024 BJA Tables/bja_gt2.html")

### Data characteristics ####
bja_gt3 = cleaner_bja_tbl |> 
  select(c_agency_name, agency_name, grantee_legal_name, location_of_death_state_postal_abbreviation, zip_code, 
         recode) |> # fct_lump some
  tbl_summary(sort = all_categorical(FALSE) ~ "frequency")
bja_gt3
bja_gt3 |> as_gt() |> gt::gtsave("Mortality Data/Mortality Data from 2024 BJA Tables/bja_gt3.html")


# bja_gt2 = cleaner_bja_tbl |> 
#   select(age_range, age, birth_year, gender, gender_open_text, c_gender_open_text, 
#          race, ethnicity, 
#          manner_of_death) |> # fct_lump some
#   tbl_summary(sort = all_categorical(FALSE) ~ "frequency")



bja_strat_gt = cleaner_bja_tbl |> 
  select(state, gender, race, ethnicity, manner_of_death, cid_dod_year_int, cid_dob_year_int, cid_calc_age) |> 
  tbl_summary(by = state, sort = all_categorical(FALSE) ~ "frequency")

bja_strat_gt |> as_gt() |> gtsave("Mortality Data/Mortality Data from 2024 BJA Tables/bja_strat_gt.html")

# DOD year seems to be not the same as FY, which seems to be calculated. e.g...
cleaner_bja_tbl |> count(year_of_fiscal_year_of_death, cid_dod_year_int, sort = T)
# ... and there are a handful of unclean data it seems, still.

# Chi square by manner of death
cleaner_bja_tbl |> count(state, sort = T) # May consider a filter on obs N
cleaner_bja_tbl |> count(state, manner_of_death, sort = T)
# bja_manner_chiqsq_model = cleaner_bja_tbl |> specify(manner_of_death ~ state) |> 
#   hypothesize(null = "independence")  |> calculate(stat = "Chisq")
bja_manner_chiqsq_model = cleaner_bja_tbl |> chisq_test(manner_of_death ~ state)
bja_manner_chiqsq_model

bja_manner_chiqsq_model = cleaner_bja_tbl |> 
  group_by(state) |> mutate(n_obs = n()) |> filter(n_obs>50) |> 
  chisq_test(manner_of_death ~ state)
# https://www.tidymodels.org/learn/statistics/xtabs/
