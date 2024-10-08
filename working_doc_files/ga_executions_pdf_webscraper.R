library(rbin)
library(tabulapdf)
library(dplyr)
library(tidyr)
library(janitor)
library(here)
library(stringr)
library(purrr)

pdf_file <- here('data', 'ga_executions_full.pdf')
result <- extract_tables(pdf_file, pages = c(12,13))


result_pg13 <- result[[2]]
result_pg13 <- result_pg13 |>
  rbind(colnames(result_pg13)) |>
  rename(name = "Mize, William",
         race_gender = "WM",
         age = "53",
         county_convic = "Oconee",
         crime = "Murder",
         ind_dod = "04/29/2009")

result_pg12 <- result[[1]]
result_pg12 <- result_pg12 |>
  rename(name = "Name Race",
         age = "Age",
         county_convic = "Conviction",
         crime = "Crime",
         ind_dod = "Date") |>
  select(-...2) |>
  mutate(race_gender = str_extract_all(name, '[BWM]{2,}'),
         name = str_replace(name, '[BWM]{2,}', ""))

result_pg13 <- result_pg13[, names(result_pg12)]

ga_uncleaned_data <- rbind(result_pg12, result_pg13)

ga_uncleaned_data <- ga_uncleaned_data |> 
  mutate(last_name = str_split_fixed(name, ",", 2)[, 1],
         first_and_middle = str_split_fixed(name, ",", 2)[, 2],
         first_name = map_chr(str_split(first_and_middle, " ", n=3), ~ .x[2]),
         middle_name = if_else(map_chr(str_split(first_and_middle, " ", n=3), ~ .x[3]) == "", NA, map_chr(str_split(first_and_middle, " ", n=3), ~ .x[3])),
         first_name = str_replace(first_name, " ", ""),
         last_name = str_replace(last_name, " ", ""),
         race = if_else(map_chr(str_split(race_gender, "", n=2), ~ .x[1]) == "B", "Black", "White"),
         gender = if_else(map_chr(str_split(race_gender, "", n=2), ~ .x[2]) == "M", "Male", "Female"),
         age = as.integer(str_replace(age, " ", ""))) |>
  select(-c(first_and_middle, name, race_gender)) 

#TO DO : 
#regex for name field -- split into first, middle, last -- eliminate aka nickname -- separate racegender classification into its own column
# reclassify NAs in age from unk to NA val
#only save values with dates 2000 onwards