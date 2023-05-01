#===============================================================================
# 2023-03-18 -- never-in-union
# prepare data
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")

# the main prepared dataset
raw <- read.csv("out/D_all_minage35_edu2.csv") %>% 
  janitor::clean_names() 

# Human Development Index data
hdi <- read_excel(
  "out/HDR21-22_Statistical_Annex_HDI_Table.xlsx", sheet = "Table 1", skip = 5
) %>% 
  janitor::clean_names() %>% 
  transmute(
    country = case_when(
      country == "Congo (Democratic Republic of the)" ~ "Democratic Republic of the Congo",
      country == "Côte d'Ivoire" ~ "Ivory Coast",
      country == "Türkiye" ~ "Turkey",
      country == "Tanzania (United Republic of)" ~ "Tanzania",
      country == "Russian Federation" ~ "Russia",
      country == "Bolivia (Plurinational State of)" ~ "Bolivia",
      country == "Eswatini (Kingdom of)" ~ "Swaziland",
      country == "Moldova (Republic of)" ~ "Moldova",
      T ~ country
    ),
    hdi = value %>% as.numeric()
  ) %>% 
  drop_na() %>% 
  mutate(
    iso3 = country %>% countrycode(origin = "country.name", destination = "iso3c"),
    iso2 = country %>% countrycode(origin = "country.name", destination = "iso2c")
  )

# Gapminder regional classification of countries
# Use GDP and life_exp data from 30 years ago. 1992
library(gapminder)

gap <- gapminder_unfiltered %>% 
  clean_names() %>% 
  filter(year == 1992) %>% 
  mutate(
    iso3 = country %>% countrycode(origin = "country.name", destination = "iso3c"),
    continent = continent %>% paste
  )


# join and clean
never <- raw %>% 
  mutate(
    country = case_when(
      country == "Republic of Moldova" ~ "Moldova",
      country == "CzechRepublic" ~ "Czechia",
      T ~ country
    ),
    country = country %>% 
      str_replace("Republic of Moldova", "Moldova") %>% 
      str_replace("CzechRepublic", "Czechia"), 
    prop_childless = (childless_n/ total_n),
    prop_neverinunion = (never_in_union_n / childless_n)
  ) %>% 
  # join together
  left_join(hdi) %>% 
  left_join(gap %>% select(-country), by = "iso3") %>% 
  # final filtering
  filter(bc_cate == "1960-1969", education == "All", total_n >= 1) %>% 
  # drop NaN for `prop_neverinunion`
  drop_na(prop_neverinunion)  %>% 
  # UPD  2023-03-23 fix Kyrgyzstan
  mutate(
    continent = case_when(country == "Kyrgyzstan" ~ "FSU", TRUE ~ continent)
  ) %>% 
  # UPD  2023-04-26
  # filter out small 
  mutate(
    small_cases = case_when(childless_n < 10 ~ 0, TRUE ~ 1)
  ) %>% 
  group_by(country) %>% 
  mutate(
    small_cases_n = small_cases %>% sum
  ) %>% 
  ungroup() %>% 
  filter(small_cases_n == 2) %>% 
  # UPD  2023-03-23
  # arrange by HDI of the continent first and then within the continents
  group_by(continent) %>% 
  mutate(cont_hdi = hdi %>% mean) %>% 
  ungroup() %>% 
  arrange(cont_hdi, hdi) %>% 
  mutate(country = country %>% as_factor %>% fct_inorder()) 



# countries without male data 
no_male_cntr <- never %>% 
  select(iso3, sex, prop_childless) %>% 
  pivot_wider(names_from = sex, values_from = prop_childless) %>% 
  filter(is.na(Men)) %>% 
  pull(iso3)

# filter out countries without male data

never <- never %>% filter(! iso3 %in% no_male_cntr)


save(never, file = "out/never.rda")


# get world map outline (you might need to install the package)
world_outline <- spData::world %>% 
  st_as_sf() %>% 
  rmapshaper::ms_simplify(.25)

# let's use a fancy projection
world_outline_robinson <- world_outline %>% 
  #remove Antarctica
  filter(!iso_a2 == "AQ") %>% 
  st_transform(crs = "ESRI:54030") %>% 
  mutate(
    iso3 = name_long %>% countrycode(origin = "country.name", destination = "iso3c")
  ) %>% 
  select(-continent) %>% 
  left_join(never, by = "iso3")

country_borders <- world_outline %>% 
  rmapshaper::ms_innerlines() %>% 
  st_transform(crs = "ESRI:54030") 

save(world_outline_robinson, country_borders, file = "out/geodata.rda")
