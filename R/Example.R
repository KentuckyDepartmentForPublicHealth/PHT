# library(tidyverse)
# library(REDCapR)
# 
# url <- "https://redcap.chfs.ky.gov/api/"
# # 
# # 
# data_from_redcap <- redcap_read_oneshot(url, my_token, raw_or_label = "label")$data
# # 
# data_from_redcap$lnayear %>% table()
# # 
# lhd_and_lna_year <- data_from_redcap %>%
#   select(hdname, lnayear) %>%
#   mutate(is_compliant = ifelse(is.na(lnayear), 'No', 'Yes'))
# # 
# lhd_and_lna_year$is_compliant %>% table()
