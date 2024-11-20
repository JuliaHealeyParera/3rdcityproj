library(tidyverse)
library(ggplot2)

##Save R vis ggplot object, modify different ways for different uses

#Ex. manuscript -- text title, not in image itself (caption too)
#with title and caption, PNG, DPI 600

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

base <- ggplot(sum_cod, aes(x = all_passing, y = label)) +
  geom_col(fill = "#a6bddb", width = .9, position = 'dodge') +
  geom_text(aes(label = all_passing), hjust = -.4, size = 3.3) +
  scale_x_continuous(limits = c(0, 54), expand = c(.01,0))  +
  scale_y_discrete(expand = c(.058, .058)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15)) 

base +
  labs(x = "States Reporting", y = "", title = "No cause of death is reported by all systems.") 

