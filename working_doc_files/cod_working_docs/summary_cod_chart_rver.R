library(tidyverse)
library(ggplot2)

data_file_path <- here('data', 'summary_cod_cutoffs.csv')
sum_cod <- read_csv(data_file_path)

sum_cod <- sum_cod |>
  mutate(label = case_when(
    cod_category == "unknown" ~ "Unknown",
    cod_category == "uninten_non_drug_inj" ~ "Uninten. Non-Drug Inj.",
    cod_category == "suicide" ~ "Suicide",
    cod_category == "pending" ~ "Pending",
    cod_category == "natural" ~ "Natural",
    cod_category == "homicide_by_leo" ~ "Homicide by LEO",
    cod_category == "homicide" ~ "Homicide",
    cod_category == "execution" ~ "Execution",
    cod_category == "drug_alcohol" ~ "Drug/Alcohol",
    cod_category == "covid_19" ~ "COVID-19"))

ggplot(sum_cod, aes(x = all_passing, y = label)) +
  geom_col(fill = "#a6bddb", width = 0.7, position = 'dodge') +
  geom_text(aes(label = all_passing), hjust = -.4, size = 3.3) +
  scale_x_continuous(limits = c(0, 54), expand = c(.01,0))  +
  scale_y_discrete(expand = c(.058, .058)) +
  theme_bw() +
  theme(plot.title = element_text(size = 15)) +
  labs(x = "States Reporting", y = "", title = "No cause of death is reported by all systems.") 
  
