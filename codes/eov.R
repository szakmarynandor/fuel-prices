library(tidyverse)
library(rvest)

highway_df <- readxl::read_excel("data/20211125-ap-au_csomópontok.xls") %>% # FIXME
  janitor::row_to_names(1) %>%
  janitor::clean_names() %>%
  select(knev, x = keovx, y = keovy) %>%
  mutate_at(-1, as.numeric) %>%
  mutate_at(-1, round) %>%
  mutate_at(-1, as.character) %>%
  na.omit()

n_highway_done <- list.files("data/") %>% 
  keep(str_detect, "highway_eov") %>% 
  length()

highway_eov_done <- list.files("data/", full.names = TRUE) %>% 
  keep(str_detect, "highway_eov") %>% 
  map(read_rds) %>% 
  bind_rows()

if (nrow(highway_eov_done) == 0) {
  highway_eov_left <- highway_df
} else {
  highway_eov_left <- highway_df %>% 
    anti_join(highway_eov_done)
}

eov_l <- list()

for (i in 1:50) {
  
  eovy <- highway_eov_left$x[i]
  eovx <- highway_eov_left$y[i]

  j <- read_html(str_c("http://service.psoft.hu/eovwgs100.php?eovy=", eovy, "&eovx=", eovx, "&jsCallback=EOVWGS.callback"))
  if (str_detect(as.character(j), "Request limit exceeded")) break
  
  lat <- as.character(j) %>% 
    gsub(pattern = ".*lat", replacement = "") %>% 
    gsub(pattern = ",.*", replacement = "") %>% 
    gsub(pattern = '\":', replacement = "") %>% 
    as.numeric()
  
  lon <- as.character(j) %>% 
    gsub(pattern = ".*lon", replacement = "") %>% 
    gsub(pattern = ",.*", replacement = "") %>% 
    gsub(pattern = '\":', replacement = "") %>% 
    as.numeric()
  
  eov_l[[i]] <- tibble(lat, lon)

  if (i == nrow(highway_eov_left)) {
    message("job done!")
    break
  } 
}

if (length(eov_l) >= 1) {
eov_df <- eov_l %>% 
  bind_rows()

highway_eov_left %>% 
  head(nrow(eov_df)) %>% 
  bind_cols(eov_df) %>% 
  saveRDS(file = str_c("data/highway_eov", n_highway_done + 1, ".rds"))
}



