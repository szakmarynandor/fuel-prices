library(tidyverse)
library(rvest)

read_html("http://service.psoft.hu/eovwgs100.php?eovy=637254&eovx=235286&jsCallback=EOVWGS.callback")

eov <- function(eovy, eovx) {
j <- read_html(str_c("http://service.psoft.hu/eovwgs100.php?eovy=", eovy, "&eovx=", eovx, "&jsCallback=EOVWGS.callback"))
j
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

tibble(lat, lon)
}

df <- tibble::tribble(
                                                                                                                      ~knev,       ~x,       ~y,
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "637254", "235286",
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "638146", "233408",
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "638501", "231997",
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "643304", "229831",
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "644656", "228464",
                                                                             "M0 autóút körgyűrű /M1 - 11 sz. főút között/", "647242", "228193"
                                                                             )
# df <- readxl::read_excel("data/20211125-ap-au_csomópontok.xls") %>% # FIXME
#   janitor::row_to_names(1) %>% 
#   janitor::clean_names() %>% 
#   select(knev, x = keovx, y = keovy) %>% 
#   mutate_at(-1, as.numeric) %>% 
#   mutate_at(-1, round) %>% 
#   mutate_at(-1, as.character) %>% 
#   na.omit() %>% 
#   head() %>% 
df %>% 
  mutate(data = map2(x, y, eov)) %>% 
  unnest()


