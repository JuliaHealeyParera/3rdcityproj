library(tidyverse)
library(chromote)
library(rvest)
install.packages('sys')
library(sys)

mt_dataset_url = 'https://dataportal.mt.gov/t/COR/views/MortalityinCorrections_17005883769990/DatainListFormat?%3AshowAppBanner=false&%3Adisplay_count=n&%3AshowVizHome=n&%3Aorigin=viz_share_link&%3Aembed=y'

mt_rvest_live <- rvest::read_html_live(mt_dataset_url)
mt_rvest_live$view()

Sys.sleep(5)

mt_rvest_live |> html_elements("Button")
mt_rvest_live$click("Button[class='fppw03o low-density'", n_clicks = 1)