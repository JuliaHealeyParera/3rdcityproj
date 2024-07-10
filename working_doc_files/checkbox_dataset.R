##Loading packages
library(tidyverse) 
library(here)
library(dplyr)

##Loading dataset
data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

checklist_system_data_path <- here('data', 'checklist_system_data_check.csv')
checklist_system_data <- read_csv(checklist_system_data_path)

##Cleaning system abbreviations 
agg_data <- agg_data |> 
  mutate(c_system_abbr = case_when(
    startsWith(file, "FL") ~ "FL", 
    startsWith(file, "AZ") ~ "AZ",
    startsWith(file, "DE") ~ "DE", 
    startsWith(file, "IA") ~ "IA",
    TRUE ~ c_system_abbr
  ))

##COD variable
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
           ifelse(uninten_non_drug_inj > 0, 1, 0)) |>
  mutate(four_cod = ifelse(tot_cod >=4, "Yes", "No")) |>
  select(c_system_abbr, four_cod)


##DCRA variable
dcra_variables <- c("c_system_abbr", "c_ind_full_name", "c_ind_dob_year", "c_ind_gender", "c_ind_race", "c_ind_ethnicity", "c_ind_dod_ymd", "ind_tod", "ind_deathloc", "c_ind_fachoused", "c_ind_cod_avail")

data <- agg_data %>%  
  mutate(c_ind_first = 
           case_when(
             c_ind_first == "N/A" ~ NA, 
             TRUE ~ c_ind_first),
         c_ind_last = 
           case_when(
             c_ind_last == "N/A" ~ NA,
             TRUE ~ c_ind_last)
  )

dcra_dataset <- data %>% 
  mutate(c_ind_full_name = ifelse(!is.na(c_ind_first) & !is.na(c_ind_last),
                                  paste(c_ind_first, " ", c_ind_last), 
                                  NA),
         c_ind_cod_avail = ifelse(c_ind_cod_avail != "Listed", NA, c_ind_cod_avail)) %>% 
  select(all_of(dcra_variables))

#All other percentages
dcra_table_variable <- dcra_dataset %>%
  group_by(c_system_abbr) %>% 
  summarize(across(everything(), ~ round(mean(!is.na(.)) * 100, 2))) %>% 
  select(all_of(c(dcra_variables))) %>%
  rename(system = c_system_abbr) %>% 
  pivot_longer(!system, names_to = "variable", values_to = "percentages") 

dcra_table_almost_complete <- dcra_table_variable %>% 
  mutate(present_var = 
           case_when(
             percentages > 90 ~ "Present", 
             TRUE ~ "Missing"
             
           )
  ) %>% 
  group_by(system, present_var) %>%
  count() %>% 
  filter(present_var == "Present") %>% 
  mutate(almost_complete = 
           case_when(
             n >= 7 ~ "Yes", 
             TRUE ~ "No"
           ) 
  ) %>%
  ungroup() %>% 
  select(system, almost_complete) %>% 
  rename(c_system_abbr = system)

##Any press releases variable
total_counts_agg <- agg_data |>
  group_by(c_system_abbr) |>
  summarize(count = n()) |>
  mutate(any_pr = ifelse(count > 0, "Yes", "No")) |>
  select(c_system_abbr, any_pr)

##Press releases in the past five years variable
yearly_counts_agg <- agg_data |>
  separate(c_ind_dod_ymd, into = c("dod_year", "dod_month", "dod_day"), sep = "-") |>
  filter(dod_year %in% c("2023", "2022", "2021", "2020", "2019")) |>
  group_by(c_system_abbr, dod_year) |>
  summarize(pr_counts = n()) |>
  mutate(pass_pr_count = ifelse(pr_counts > 5, 1, 0)) |>
  group_by(c_system_abbr) |>
  summarize(five_yr_prs = ifelse(sum(pass_pr_count) == 5, "Yes", "No"))

##Joining variables
checkbox_dataset <- cod_agg %>% 
  left_join(dcra_table_almost_complete, by = "c_system_abbr") |> 
  left_join(total_counts_agg) |>
  left_join(yearly_counts_agg) |>
  right_join(checklist_system_data) |>
  mutate(four_cod = ifelse(is.na(four_cod), "No", four_cod),
         almost_complete = ifelse(is.na(almost_complete), "No", almost_complete),
         any_pr = ifelse(is.na(any_pr), "No", any_pr),
         five_yr_prs = ifelse(is.na(five_yr_prs), "No", five_yr_prs))
  
##Writing to CSV
write.csv(checkbox_dataset, "data/checkbox_dataset.csv")

##Tableau ver. 
checkbox_tab <- checkbox_dataset |> 
  pivot_longer(cols = four_cod:system_data, names_to = "pass_category", values_to = "pass_status")

write.csv(checkbox_tab, "data/checkbox_tab.csv")
