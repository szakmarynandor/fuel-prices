library(tidyverse)
library(furrr)

NiceMonth <- function(x, keep_year = FALSE, label = FALSE) {
  .year <- gsub(na.omit(x), pattern = "-.*", replacement = "")
  
  .month <- str_remove(na.omit(x), "2021-|2022-") %>%
    gsub(pattern = "-.*", replacement = "") %>%
    as.numeric()
  
  out <- case_when(
    .month == 1 ~ "jan",
    .month == 2 ~ "feb",
    .month == 3 ~ "mar",
    .month == 4 ~ "apr",
    .month == 5 ~ "may",
    .month == 6 ~ "jun",
    .month == 7 ~ "jul",
    .month == 8 ~ "aug",
    .month == 9 ~ "sep",
    .month == 10 ~ "oct",
    .month == 11 ~ "nov",
    .month == 12 ~ "dec"
  )
  
  if (keep_year) {
    if (out[[1]] != "jan") out[[1]] <- str_c(.year[[1]],". ", out[[1]])
    out <- ifelse(out == "jan", paste(.year, out), out)
  }
  
  if (label) {
    out <- c(NA, out, NA)
  }
  
  out
}
