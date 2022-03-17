in_distance <- function(.data, .dist) {
  # TODO azonos márkájú
.data %>% 
  filter(!duplicated(address)) %>% 
  column_to_rownames("address") %>% 
  select(Latitude, Longitude) %>% 
  na.omit() %>% 
  as.matrix() %>% 
  dist(diag = TRUE, upper = TRUE) %>% 
  as.matrix() %>% 
  data.frame() %>% 
  rownames_to_column("x") %>% 
  pivot_longer(-1) %>% 
  group_by(x) %>% 
  summarise(sum(value <= .dist * .01506) - 1) %>% # coordinate distance to km
  set_names("address", str_c("in_", .dist, "km"))
}

in_distance_df <- map(c(.5, 1, 2, 3, 5), ~ in_distance(fuel_df, .)) %>% 
    reduce(full_join, by = "address")

save(in_distance, file = "data/in_distance_df.RData")