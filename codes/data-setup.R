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
    brand = str_remove_all(brand,  '<img src="images/partner_logo/'),
    brand = str_remove_all(brand, '" style="float:left; margin-right:10px;"/>'),
    brand = gsub("\\d*_", "", brand),
    brand = str_remove_all(brand, "[.]png"),
    brand = case_when(
      brand  == "bb1bd22248888770445410b0e36db4e1" ~ "MOL",
      brand  == "omv" ~ "OMV",
      brand  == "shell" ~ "Shell",
      brand  == "lukoil" ~ "Lukoil",
      brand  == "mobilpetrol" ~ "Mobilpetrol",
      brand  == "oil!" ~ "OIL!",
      brand  == "avia" ~ "Avia",
      brand  == "auchan" ~ "Auchan",
      brand  == "envi" ~ "ENVI",
      brand  == "molpartner" ~ "MOL",
      brand  == "edo" ~ "EDO",
      brand  == "hunpetrol" ~ "HunPetrol",
      brand  == "nkm" ~ "NKM",
      brand  == "grovi" ~ "Grovi",
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

eov_df <- list.files("data/", full.names = TRUE) %>% 
  keep(str_detect, "highway_eov\\d.rds") %>% 
  map_df(read_rds) %>% 
  rename(eovx = x, eovy = y, Latitude = lat, Longitude = lon) %>% 
  select(- knev)


highway_df <- readxl::read_excel("data/20211125-ap-au_csomópontok.xls") %>% # FIXME
  janitor::row_to_names(1) %>% 
  janitor::clean_names() %>% 
  select(-1) %>% 
  rename_all(str_remove, "k") %>% 
  mutate_at(vars(eovx:eovy), as.numeric) %>% 
  mutate_at(vars(eovx:eovy), round) %>% 
  mutate_at(vars(eovx:eovy), as.character) %>% 
  left_join(eov_df) %>% 
  na.omit()

save(highway_df, file = "data/highway_df.RData")

library(sf)

fuel_sf <- fuel_df %>% 
  select(address, Latitude, Longitude) %>% 
  unique() %>% 
  mutate(geometry = map2(Longitude, Latitude, ~ st_point(c(.x, .y)))) %>% 
  st_as_sf() %>% 
  st_set_crs(4326)

highway_sf <- highway_df %>% 
  select(nev, Latitude, Longitude) %>% 
  unique() %>% 
  mutate(geometry = map2(Longitude, Latitude, ~ st_point(c(.x, .y)))) %>% 
  st_as_sf() %>% 
  st_set_crs(4326)

highway_distance_df <- st_distance(fuel_sf, highway_sf) %>% 
  apply(1, min) %>% 
  enframe(value = "highway_distance", name = NULL) %>% 
  bind_cols(fuel_sf) %>% 
  select(address, highway_distance) %>% 
  mutate(
    highway_distance = ifelse(address %in% unique(filter(fuel_df, highway)$address), 0, highway_distance),
    highway_distance = highway_distance / 1e3  
  )

save(highway_distance_df, file = "data/highway_distance_df")

energy_agency_2022_df <- readxl::read_excel("data/hazai_koolajpiaci_informaciok_2012_2022.xls", sheet = "2022") %>% 
  set_names("time", "petrol", "gasoil") %>% 
  slice(-(1:which(petrol == "Ft/l"))) %>% 
  na.omit() %>% 
  mutate(time = as.Date(as.numeric(time), origin =  "1899-12-30")) %>% 
  mutate_at(-1, as.numeric)

energy_agency_2021_df <- readxl::read_excel("data/hazai_koolajpiaci_informaciok_2012_2022.xls", sheet = "2021") %>% 
  set_names("time", "petrol", "gasoil") %>% 
  slice(-(1:which(petrol == "Ft/l"))) %>% 
  na.omit() %>% 
  mutate(time = as.Date(as.numeric(time), origin =  "1899-12-30")) %>% 
  mutate_at(-1, as.numeric)

energy_agency_df <- bind_rows(energy_agency_2021_df, energy_agency_2022_df) %>% 
  filter(!duplicated(time))

save(energy_agency_df, file = "data/energy_agency_df.RData")

load("data/usdhuf.RData")

crude_df <- read_csv("data/Crude Oil Urals Europe CFR Spot Historical Data.csv") %>% 
  select(1:2) %>% 
  rename(time = 1, crude_price = 2) %>% 
  mutate(time = lubridate::mdy(time))

library(rvest)

Sys.setlocale("LC_TIME") # mac os specific language setup for Hungarian dates

crude_df <- read_html("https://www.mnb.hu/arfolyam-tablazat?deviza=rbCurrencySelect&devizaSelected=USD&datefrom=2021.01.01.&datetill=2022.03.17.&order=1") %>% 
  html_table() %>% 
  .[[1]] %>% 
  set_names("time", "usd_huf") %>% 
  slice(-(1:2)) %>% 
  mutate(
    time = lubridate::ymd(time),
    usd_huf = str_replace_all(usd_huf, ",", "."),
    usd_huf = as.numeric(usd_huf)
  ) %>% 
  full_join(x = crude_df) %>% 
  arrange(time)

save(crude_df, file = "data/crude_df.RData")

distance_from_szazhalom_df <- granatlib::Hungarian_map_admin8 %>%
  mutate(geometry = st_centroid(geometry)) %>% 
  st_transform(crs = 4326) %>% 
  filter(NAME == "Százhalombatta") %>% 
  st_distance(fuel_sf) %>% 
  as.numeric() %>% 
  enframe(value = "distance_from_szazhalom", name = NULL) %>% 
  mutate(distance_from_szazhalom = distance_from_szazhalom / 1e3) %>% 
  bind_cols(fuel_sf) %>% 
  select(address, distance_from_szazhalom)

save(distance_from_szazhalom_df, file = "data/distance_from_szazhalom_df.RData")

income_df <- readxl::read_excel("data/tp_jaras_jov.xlsx") %>% 
    mutate(szja_alap = as.numeric(szja_alap)) %>% 
    drop_na() %>% 
    rename(city = telepules, lau = jaras)

save(income_df, file = "data/income.RData")


    
