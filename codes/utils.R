library(tidyverse)
library(furrr)
library(sf)

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

count_competitives <- function(distance = 2e4, diff_brand = TRUE) {
  competitives_distance_df <- fuel_sf %>% 
    st_distance() %>% 
    data.frame() %>% 
    set_names(fuel_sf$address) %>% 
    mutate(neighbour = fuel_sf$address) %>% 
    pivot_longer(- neighbour, names_to = "address") %>% 
    filter(neighbour != address) %>% 
    left_join(
      fuel_df %>% 
        select(address, brand) %>% 
        distinct() %>% 
        rename(address_brand = brand),
      by = 'address'
    ) %>% 
    left_join(
      fuel_df %>% 
        select(address, brand) %>% 
        distinct() %>% 
        rename(neighbour_brand = brand, neighbour = address),
      by = "neighbour"
    )
  
  if (diff_brand) {
    competitives_distance_df <- competitives_distance_df %>% 
      filter(address_brand != neighbour_brand)
  }
  
  n_neighbour_l <- map(distance, function(d) {
    var_name <- str_c("competitives_in_", d, "m")
    
    competitives_distance_df %>% 
      mutate(value = as.numeric(value)) %>% 
      filter(value <= d) %>% 
      count(address) %>% 
      set_names("address", var_name)
  })
  
  n_neighbour_l %>% 
    reduce(full_join, by = "address") %>% 
    left_join(
      x = fuel_df %>% 
        select(address) %>% 
        distinct(),
      by = "address"
    ) %>% 
    mutate_at(-1, ~ ifelse(is.na(.), 0, .))
}
