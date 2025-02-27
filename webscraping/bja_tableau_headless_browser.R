
library(chromote)
library(rvest)
library(sys)

mt_dataset_url = 'https://charts.ojp.usdoj.gov/t/public/views/DCRATransparencyTablesSeptVersion/Source?%3Aembed=y&%3AisGuestRedirectFromVizportal=y'

mt_rvest_live <- rvest::read_html_live(mt_dataset_url)
mt_rvest_live$view()

Sys.sleep(5)

mt_rvest_live |> html_elements("data-tb-test-id")
mt_rvest_live$click("Button[class='fppw03o low-density'", n_clicks = 1)