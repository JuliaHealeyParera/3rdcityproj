#Loading libraries
library(tidyverse)
library(here) 

#Loading datasets
data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

geo_file_path <- here('data', 'jurisdiction_sf.geojson')
geo_data <- st_read(geo_file_path)

#Cleaning source dataset
agg_data <- agg_data |>
  mutate(c_system_abbr = case_when(
    startsWith(file, "FL") ~ "FL", 
    startsWith(file, "AZ") ~ "AZ",
    startsWith(file, "DE") ~ "DE", 
    startsWith(file, "IA") ~ "IA",
    TRUE ~ c_system_abbr
  ))

#Dataframe with year of earliest and latest press release by system
min_year_sum <- agg_data |>
  separate(c_ind_dod_ymd, into = c("dod_year", "dod_month", "dod_day"), sep = "-") |>
  mutate(dod_year = case_when(
    is.na(dod_year) & endsWith(ind_dod, "2024") ~ "2024",
    is.na(dod_year) & endsWith(ind_dod, "2023") ~ "2023",
    is.na(dod_year) & endsWith(ind_dod, "1023") ~ "2023",
    is.na(dod_year) & endsWith(ind_dod, "2022") ~ "2022",
    TRUE ~ dod_year
  )) |>
  filter(dod_year != 1950 & !is.na(dod_year)) |>
  group_by(c_system_abbr) |>
  summarize(first_year = min(dod_year),
            last_year = max(dod_year))

#Dataframe with total press releases by system
total_counts_agg <- agg_data |>
  group_by(c_system_abbr) |>
  summarize(count = n()) 

#Dataframe with total COD by system
cod_agg <- agg_data |> 
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
           ifelse(execution > 0, 1, 0) +
           ifelse(homicide > 0, 1, 0) +
           ifelse(homicide_by_leo > 0, 1, 0) +
           ifelse(uninten_non_drug_inj > 0, 1, 0))

#Joining all above dataframes with with geospatial dataset
geo_data <- geo_data |> 
  left_join(min_year_sum, by = join_by(sys_abbr == c_system_abbr)) |>
  left_join(total_counts_agg, by = join_by(sys_abbr == c_system_abbr)) |>
  left_join(cod_agg, by = join_by(sys_abbr == c_system_abbr)) |>
  select(name, sys_abbr, first_year, last_year, count, tot_cod, geometry) |>
  mutate(first_year = as.integer(ifelse(is.na(first_year), 0, first_year)),
         last_year = as.integer(ifelse(is.na(last_year), 0, last_year)),
         num_years = last_year - first_year,
         count = ifelse(is.na(count), 0, count),
         tot_cod = ifelse(is.na(tot_cod), 0, tot_cod)) 

st_write(geo_data, "data/geo_data.geojson", delete_dsn = T)


quick_facts_table <- geo_data |>
  pivot_longer(cols = first_year:tot_cod, names_to = "measure_name", values_to = "values")

st_write(quick_facts_table, "data/quick_facts_table.geojson", delete_dsn = T)

