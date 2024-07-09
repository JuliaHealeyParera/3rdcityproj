#Loading packages
library(tidyverse) 
library(here)

#Loading dataset
data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

#COD variable
cod_agg <- agg_data |> 
  mutate(c_system_abbr = case_when(
    startsWith(file, "FL") ~ "FL", 
    startsWith(file, "AZ") ~ "AZ",
    startsWith(file, "DE") ~ "DE", 
    startsWith(file, "IA") ~ "IA",
    TRUE ~ c_system_abbr
  )) |>
  group_by(c_system_abbr, c_ind_cod_type) |> 
  summarize(count = n()) |>
  mutate(c_ind_cod_type = case_when(
    c_ind_cod_type == "COVID-19" ~ "covid_19",
    c_ind_cod_type == "Drug / Alcohol" ~ "drug_alcohol",
    c_ind_cod_type == "Natural" ~ "natural",
    c_ind_cod_type == "Suicide" ~ "suicide",
    c_ind_cod_type == "Unknown" ~ "unknown",
    c_ind_cod_type == "Execution" ~ "execution",
    c_ind_cod_type == "Homicide" ~ "homicide",
    c_ind_cod_type == "Homicide by LEO" ~ "homicide_by_leo",
    c_ind_cod_type == "Unintentional non-Drug Injury" ~ "uninten_non_drug_inj",
    c_ind_cod_type == "Pending" ~ "pending",
  )) |>
  pivot_wider(names_from = c_ind_cod_type, values_from = count, values_fill = 0) |> 
  mutate(tot_cod = 
           ifelse(covid_19 > 0, 1, 0) +
           ifelse(drug_alcohol > 0, 1, 0) +
           ifelse(natural > 0, 1, 0) +
           ifelse(suicide > 0, 1, 0) +
           ifelse(unknown > 0, 1, 0) +
           ifelse(execution > 0, 1, 0) +
           ifelse(homicide > 0, 1, 0) +
           ifelse(homicide_by_leo > 0, 1, 0) +
           ifelse(uninten_non_drug_inj > 0, 1, 0) +
           ifelse(pending > 0, 1, 0)) |>
  mutate(four_cod = ifelse(tot_cod >=4, "Yes", "No"))