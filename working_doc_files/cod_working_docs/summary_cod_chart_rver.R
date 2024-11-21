library(tidyverse)
library(ggplot2)
library(here)

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
  geom_text(aes(label = all_passing), hjust = 1.75, size = 4, color = "white") +
  scale_x_continuous(limits = c(0, 54), expand = c(.01,0))  +
  scale_y_discrete(expand = c(.058, .058)) +
  theme_minimal() +
  theme(plot.title = element_text(size = 15)) +
  labs(x = "States Reporting", y = "")
base

base_title <- base +
  labs(title = "No cause of death is reported by all systems.") 
base_title

path_wo_title <- here("working_doc_files", "dashboard_visualizations", "summary_cod_chart_wo_title.png")
path_w_title <- here("working_doc_files", "dashboard_visualizations", "summary_cod_chart_w_title.png") 

ggsave(path_wo_title, plot = base, width = 10, height = 8, dpi = 300)
ggsave(path_w_title, plot = base_title, width = 10, height = 8, dpi = 300)
