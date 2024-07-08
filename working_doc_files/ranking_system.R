library(tidyverse)
library(here)

data_file_path <- here('data', 'aggregate_data.csv')
agg_data <- read_csv(data_file_path)

populations_file_path <- here('data', 'cod_cutoffs.csv')
system_populations <- read_csv(populations_file_path)

##Press releases across years scores

system_populations <- system_populations |>
  select(system, system_pop)

yearly_aggregate <- agg_data |> 
  separate(c_ind_dod_ymd, into = c("dod_year", "dod_month", "dod_day"), sep = "-") |>
  filter(dod_year %in% c("2019", "2020", "2021", "2022", "2023")) |>
  group_by(c_system_abbr) |>
  summarize(count = n()) |> 
  right_join(system_populations, by = join_by(c_system_abbr == system)) |> 
  mutate(pr_quant_score = count / (system_pop))



##DCRA fields scores

data <- agg_data %>%
  separate(file, into = c("press_release_system", "press_release", "year"), sep = "_") %>%
  mutate(c_system_abbr = case_when(
    is.na(c_system_abbr) ~ press_release_system,
    TRUE ~ c_system_abbr
  ))

data %>% 
  filter(is.na(c_system_abbr))

dcra_variables <- c("c_system_abbr", "c_ind_full_name", "c_ind_dob_year", "c_ind_gender", "c_ind_race", "c_ind_ethnicity", "c_ind_dod_ymd", "ind_tod", "ind_deathloc", "c_ind_fachoused", "c_ind_cod_avail_listed")

#selected dataset variables based on DCRA required fields 
data <- data %>%  
  mutate(c_ind_first = 
           case_when(
             c_ind_first == "N/A" ~ NA, 
             TRUE ~ c_ind_first),
         c_ind_last = 
           case_when(
             c_ind_last == "N/A" ~ NA,
             TRUE ~ c_ind_last),
         c_ind_cod_avail_listed = ifelse(c_ind_cod_avail == "Listed", "Listed", NA))

dcra_dataset <- data %>% 
  mutate(c_ind_full_name = ifelse(!is.na(c_ind_first) & !is.na(c_ind_last),
                                  paste(c_ind_first, " ", c_ind_last), 
                                  NA)) |>
  select(all_of(dcra_variables))

dcra_table_variable <- dcra_dataset %>% 
  group_by(c_system_abbr) %>% 
  summarize(across(everything(), ~ round(mean(!is.na(.)) * 100, 2))) %>% 
  select(all_of(c(dcra_variables))) %>% 
  rename(system = c_system_abbr)
dcra_table_variable

dcra_table_variable_longer <- dcra_table_variable %>% 
  right_join(geo_data, join_by(system==system)) %>% 
  mutate_if(is.numeric, coalesce, 0) %>% 
  select(c("system", "c_ind_full_name", "c_ind_dob_year", "c_ind_gender", "c_ind_race", "c_ind_ethnicity", "c_ind_dod_ymd", "ind_tod", "ind_deathloc", "c_ind_fachoused", "c_ind_cod_avail_listed")) %>% 
  pivot_longer(!system, names_to = "variable", values_to = "percentages")

grouped_dcra_points <- dcra_table_variable_longer |>
  group_by(system) |>
  summarize(total_dcra_points = sum(percentages))

total_points <- right_join(yearly_aggregate, grouped_dcra_points, by = join_by(c_system_abbr == system)) |>
  mutate(final_points = pr_quant_score * total_dcra_points) |>
  mutate(press_release_rank = rank(-pr_quant_score),
         data_completeness_rank = round(rank(-total_dcra_points)),
         final_point_rank = rank(-final_points)) |>
  mutate(press_release_rank = ifelse(is.na(pr_quant_score), 54, press_release_rank),
         data_completeness_rank = ifelse(total_dcra_points == 0.00, 54, data_completeness_rank),
         final_point_rank = ifelse(is.na(final_points), 54, final_point_rank))


write.csv(total_points, "data/ranking_system.csv")
