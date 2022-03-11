raw_file <- function(file_name = NULL) {
  raw_folder <- getwd() %>% 
    str_remove("fuel-prices") %>% 
    str_c("benzin/data")
  if (!is.null(file_name)) {
    str_c(raw_folder, file_name, sep = "/")
  } else {
    raw_folder
  }
}

address <- readxl::read_excel(raw_file("address.xlsx"))
counties <- read_csv(raw_file("counties.csv"))

plan(multisession, workers = 8)

fuel_df <- list.files(raw_file(), full.names = TRUE) %>% 
  keep(str_detect, "csv") %>% 
  setdiff(raw_file("counties.csv")) %>% 
  future_map(function(file_name) {
    read_csv(file_name, show_col_types = FALSE) %>% 
      mutate(
        date = str_remove(file_name, ".csv"),
        date = str_remove(date, raw_file()),
        date = str_remove(date, "/benzin_"),
        date = lubridate::ymd(date)
      ) %>% 
      janitor::clean_names() %>% 
      select(type, price, address, city, brand, date)
  }, .progress = TRUE
  ) %>% 
  bind_rows()

fuel_df <- fuel_df %>% 
  mutate(
    price = gsub(",.*", "", price), # remove characters from price
    price = as.numeric(price),
    city = gsub(" .*", "", city),
    brand = str_remove_all(brand, '<img src="images/partner_logo/'),
    brand = str_remove_all(brand, '" style="float:left; margin-right:10px;"/>'),
    brand = gsub("\\d*_", "", brand),
    brand = str_remove_all(brand, "[.]png"),
    brand = case_when(
      brand == "bb1bd22248888770445410b0e36db4e1" ~ "MOL",
      brand == "omv" ~ "OMV",
      brand == "shell" ~ "Shell",
      brand == "lukoil" ~ "Lukoil",
      brand == "mobilpetrol" ~ "Mobilpetrol",
      brand == "oil!" ~ "OIL!",
      brand == "avia" ~ "Avia",
      brand == "auchan" ~ "Auchan",
      brand == "envi" ~ "ENVI",
      brand == "molpartner" ~ "MOL",
      brand == "edo" ~ "EDO",
      brand == "hunpetrol" ~ "HunPetrol",
      brand == "nkm" ~ "NKM",
      brand == "grovi" ~ "Grovi",
      T ~ "Egyéb"
    ),
    city = case_when(
      city == "Balatonfűred" ~ "Balatonfüred",
      city == "Balatonmáriafűrdő" ~ "Balatonmáriafürdő",
      city == "Fűle" ~ "Füle",
      city == "Fűlöpszállás" ~ "Fülöpszállás",
      city == "Fűzesabony" ~ "Füzesabony",
      city == "Pűspökladány" ~ "Püspökladány",
      city == "Révfűlöp" ~ "Révfülöp",
      city == "Sűkösd" ~ "Sükösd",
      city == "Sűmeg" ~ "Sümeg",
      city == "Tiszafűred" ~ "Tiszafüred",
      city == "Tolna-Mözs" ~ "Tolna",
      TRUE ~ city
    ),
    highway = str_detect(address, str_c("M", c(0:1, 3:7), collapse = "|"))
  ) %>%  
  left_join(address) %>% 
  left_join(select(counties, city, county)) %>% 
  filter(!is.na(city))

save(fuel_df, file = "data/fuel_df.RData")

city_df <- readxl::read_excel("data/Területi_adatok.xlsx") %>% 
  select(-2) %>% # all data are in 2020 >> irrelevant
  rename(city = 1) %>% 
  janitor::clean_names()

save(city_df, file = "data/city_df.RData")

highway_df <- readxl::read_excel("data/20211125-ap-au_csomópontok.xls") %>% # FIXME
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  select(x = keovx, y = keovy) %>% 
  mutate_all(as.numeric) %>% 
  na.omit() %>% 
  as.matrix() %>% 
  terra::vect(crs="+proj=utm +zone=10 +datum=WGS84  +units=m") %>% 
  terra::project("+proj=longlat +datum=WGS84") %>% 
  terra::geom()

