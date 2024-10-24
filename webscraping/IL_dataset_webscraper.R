library(tidyverse)
library(chromote)
library(rvest)

il_dataset_url = 'https://icjia.illinois.gov/researchhub/datasets/death-in-custody-reports/'
  
il_rvest_live <- rvest::read_html_live(il_dataset_url)
il_rvest_live$view()

il_rvest_live |> html_elements("Button")
saved_obj <- il_rvest_live$click('Button[class="v-btn v-btn--is-elevated v-btn--has-bg theme--light v-size--default"', n_clicks = 1)

