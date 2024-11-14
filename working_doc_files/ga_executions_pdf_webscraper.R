library(rbin)
library(tabulapdf)
library(dplyr)
library(tidyr)
library(janitor)
library(here)
library(stringr)
library(purrr)
library(stringr)

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

ga_cleaned_data <- ga_uncleaned_data |> 
  mutate(last_name = str_split_fixed(name, ",", 2)[, 1],
         first_and_middle = str_split_fixed(name, ",", 2)[, 2],
         ind_first = map_chr(str_split(first_and_middle, " ", n=3), ~ .x[2]),
         ind_middle = if_else(map_chr(str_split(first_and_middle, " ", n=3), ~ .x[3]) == "", NA, map_chr(str_split(first_and_middle, " ", n=3), ~ .x[3])),
         ind_first = str_replace(ind_first, " ", ""),
         ind_last = str_replace(last_name, " ", ""),
         ind_race = if_else(map_chr(str_split(race_gender, "", n=2), ~ .x[1]) == "B", "Black", "White"),
         ind_gender = if_else(map_chr(str_split(race_gender, "", n=2), ~ .x[2]) == "M", "Male", "Female"),
         ind_age = as.integer(str_replace(age, " ", "")),
         system = "GA") |>
  select(-c(first_and_middle, name, race_gender, age, last_name, county_convic)) |>
  filter(grepl("^.*(200[0-9]|20(1|2)[0-9]).*$", ind_dod))  # Match years 2000-2024


write.csv(ga_cleaned_data, 'data/ga_cleaned_data.csv')
