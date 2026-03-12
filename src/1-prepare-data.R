#===============================================================================
# 2023-03-18 -- never-in-union
# prepare data
# Ryo Mogi, rymo@sdu.dk
# Ewa Batyra, ebatyra@ced.uab.es
# Ilya Kashnitsky, ilya.kashnitsky@gmail.com
#===============================================================================

source("src/0-prepare-session.R")

# the main prepared dataset
raw <- read.csv("out/d_all_minage35_edu2_.csv") |> 
  janitor::clean_names() |> 
  mutate(
    country = country |> 
      str_replace_all("The US", "United States") |> 
      str_replace_all("The UK", "United Kingdom")
  ) |>
  mutate(
    iso3 = country |> countrycode(origin = "country.name", destination = "iso3c")
  )

# # Human Development Index data
# hdi <- read_excel(
#   "out/HDR21-22_Statistical_Annex_HDI_Table.xlsx", sheet = "Table 1", skip = 5
# ) |> 
#   janitor::clean_names() |> 
#   transmute(
#     country = case_when(
#       country == "Congo (Democratic Republic of the)" ~ "Democratic Republic of the Congo",
#       country == "Côte d'Ivoire" ~ "Ivory Coast",
#       country == "Türkiye" ~ "Turkey",
#       country == "Tanzania (United Republic of)" ~ "Tanzania",
#       country == "Russian Federation" ~ "Russia",
#       country == "Bolivia (Plurinational State of)" ~ "Bolivia",
#       country == "Eswatini (Kingdom of)" ~ "Swaziland",
#       country == "Moldova (Republic of)" ~ "Moldova",
#       T ~ country
#     ),
#     hdi = value |> as.numeric()
#   ) |> 
#   drop_na() |> 
#   mutate(
#     iso3 = country |> countrycode(origin = "country.name", destination = "iso3c"),
#     iso2 = country |> countrycode(origin = "country.name", destination = "iso2c")
#   )

un <- read.dta13("out/un_data_900010.dta")

d_un <- un |> 
  filter(bc_cate %in% c("1960-1969", "1970-1979")) |> 
  group_by(country) |> 
  summarise(gii = mean(gii_)) |> 
  mutate(
    iso3 = country |> countrycode(origin = "country.name", destination = "iso3c")
  )


# Gapminder regional classification of countries
library(gapminder)

gap <- gapminder_unfiltered |> 
  clean_names() |> 
  distinct(country, continent) |>
  # filter(year == 1992) |> 
  transmute(
    iso3 = country |> countrycode(origin = "country.name", destination = "iso3c"),
    continent = continent |> paste()
  ) |> 
  drop_na() |> 
  add_row(
    iso3 = c("KGZ"),
    continent = c("FSU")
  ) |> 
  mutate(
    name = iso3 |> countrycode(origin = "iso3c", destination = "country.name.en")
  ) |> 
  # manual fixes
  mutate(
    continent = case_when(
      continent == "Europe" ~ "Europe & North America", 
      continent == "Americas" ~ "Latin America",
      T ~ continent
    )
  ) |> 
  mutate(
    continent = case_when(
      iso3 == "AZE" ~ "FSU",
      iso3 == "MDA" ~ "FSU",
      iso3 == "CAN" ~ "Europe & North America",
      iso3 == "USA" ~ "Europe & North America",
      T ~ continent
    )
  )



never <- raw |> 
  drop_na(sex) |> 
  replace_na(list(total_n = 0, childless_n = 0, never_in_union_n = 0)) |>
  filter(
    bc_cate %in% c("1960-1969", "1970-1979"),
    edu2 == "All",
    total_n |> is_weakly_greater_than(50)
  ) |> 
  group_by(iso3, sex, edu2) |> 
  summarise(
    n_total = sum(total_n),
    n_childless = sum(childless_n),
    n_niu = sum(never_in_union_n)
  ) |> 
  group_by(iso3) |> 
  mutate(n = n()) |>
  ungroup() |> 
  filter(n == 2) |> 
  left_join(gap, by = "iso3")  |> 
  # mutate(country = ifelse(country == "Democratic Republic of the Congo", "D.R.Congo", country),
  #        continent = case_when(continent == "Europe" ~ "Europe & North America", 
  #                              continent == "Americas" ~ "Latin America",
  #                              T ~ continent),
  #        continent = case_when(country == "Kyrgyzstan" ~ "FSU",
  #                              country == "Congo" ~ "Africa",
  #                              country == "Czechia" ~ "Europe & North America",
  #                              country == "D.R.Congo" ~ "Africa",
  #                              country == "Republic of Moldova" ~ "FSU",
  #                              country == "Slovakia" ~ "FSU",
  #                              country == "The UK" ~ "Europe & North America",
  #                              country == "The US" ~ "Europe & North America",
  #                              country == "Canada" ~ "Europe & North America",
  #                              country == "Ivory Coast" ~ "Africa",
  #                             T ~ continent)) |> 
  # filter(n_total >= 50) |>  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100) |> 
  left_join(d_un, by = "iso3") |> 
  filter(!is.na(gii)) |> 
  # arrange by GII of the continent first and then within the continents
  group_by(continent) |> 
  mutate(cont_gii = gii |> mean()) |> 
  ungroup() |> 
  arrange(cont_gii, gii) |> 
  mutate(
    country = country |> as_factor() |> fct_inorder() |> fct_rev(),
    continent = continent |> as_factor() |> fct_inorder() |> fct_rev()
  ) 

# # join and clean
# never <- raw |> 
#   mutate(
#     country = case_when(
#       country == "Republic of Moldova" ~ "Moldova",
#       country == "CzechRepublic" ~ "Czechia",
#       T ~ country
#     ),
#     country = country |> 
#       str_replace("Republic of Moldova", "Moldova") |> 
#       str_replace("CzechRepublic", "Czechia"), 
#     prop_childless = (childless_n/ total_n),
#     prop_neverinunion = (never_in_union_n / childless_n)
#   ) |> 
#   # join together
#   left_join(hdi) |> 
#   left_join(gap |> select(-country), by = "iso3") |> 
#   # final filtering
#   filter(bc_cate == "1960-1969", education == "All", total_n >= 1) |> 
#   # drop NaN for `prop_neverinunion`
#   drop_na(prop_neverinunion)  |> 
#   # UPD  2023-03-23 fix Kyrgyzstan
#   mutate(
#     continent = case_when(country == "Kyrgyzstan" ~ "FSU", TRUE ~ continent)
#   ) |> 
#   # UPD  2023-04-26
#   # filter out small 
#   mutate(
#     small_cases = case_when(childless_n < 10 ~ 0, TRUE ~ 1)
#   ) |> 
#   group_by(country) |> 
#   mutate(
#     small_cases_n = small_cases |> sum
#   ) |> 
#   ungroup() |> 
#   filter(small_cases_n == 2) |> 
#   # UPD  2023-03-23
#   # arrange by HDI of the continent first and then within the continents
#   group_by(continent) |> 
#   mutate(cont_hdi = hdi |> mean) |> 
#   ungroup() |> 
#   arrange(cont_hdi, hdi) |> 
#   mutate(country = country |> as_factor |> fct_inorder()) 


# 
# # countries without male data 
# no_male_cntr <- never |> 
#   select(iso3, sex, prop_childless) |> 
#   pivot_wider(names_from = sex, values_from = prop_childless) |> 
#   filter(is.na(Men)) |> 
#   pull(iso3)
# 
# # filter out countries without male data
# 
# never <- never |> filter(! iso3 %in% no_male_cntr)


save(never, file = "out/never.rda")


# never edu ---------------------------------------------------------------

never_edu2 <- raw |> 
  drop_na(sex) |> 
  replace_na(list(total_n = 0, childless_n = 0, never_in_union_n = 0)) |>
  filter(
    bc_cate %in% c("1960-1969", "1970-1979"),
    edu2 %in% c("Low", "High"),
    total_n |> is_weakly_greater_than(30) # arbitrary
  ) |> 
  group_by(iso3, sex, edu2) |> 
  summarise(
    n_total = sum(total_n),
    n_childless = sum(childless_n),
    n_niu = sum(never_in_union_n)
  ) |> 
  group_by(iso3) |> 
  mutate(n = n()) |>
  ungroup() |> 
  filter(n == 4) |> 
  left_join(gap, by = "iso3")  |> 
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100) |> 
  left_join(d_un, by = "iso3") |> 
  filter(!is.na(gii)) |> 
  # arrange by GII of the continent first and then within the continents
  group_by(continent) |> 
  mutate(cont_gii = gii |> mean()) |> 
  ungroup() |> 
  arrange(cont_gii, gii) |> 
  mutate(
    country = country |> as_factor() |> fct_inorder() |> fct_rev(),
    continent = continent |> as_factor() |> fct_inorder() |> fct_rev()
  ) 

save(never_edu2, file = "out/never_edu2.rda")

# get world map outline (you might need to install the package)
world_outline <- spData::world |> 
  st_as_sf() |> 
  rmapshaper::ms_simplify(.25)

# let's use a fancy projection
world_outline_robinson <- world_outline |> 
  #remove Antarctica
  filter(!iso_a2 == "AQ") |> 
  st_transform(crs = "ESRI:54030") |> 
  mutate(
    iso3 = name_long |> countrycode(origin = "country.name", destination = "iso3c")
  ) |> 
  select(-continent) |> 
  left_join(never, by = "iso3")

country_borders <- world_outline |> 
  rmapshaper::ms_innerlines() |> 
  st_transform(crs = "ESRI:54030") 

save(world_outline_robinson, country_borders, file = "out/geodata.rda")
