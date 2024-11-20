library(here)
library(tidyverse)

data_file_path <- here('data', 'qc_unfiltered_duplicate_records_tbl.csv')
duplicates <- read_csv(data_file_path)

duplicates_F_L_S_DOD<- unique(duplicates[,c('c_ind_first','c_ind_last','c_system_abbr','c_ind_dod_ymd')])

duplicates_F_L_S_TOD<- unique(duplicates[,c('c_ind_first','c_ind_last','c_system_abbr','c_ind_dod_ymd')])
