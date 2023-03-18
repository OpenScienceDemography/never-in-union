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
  left_join(hdi) %>% 
  # final filtering
  filter(bc_cate == "1960-1969", education == "All", total_n >= 1) %>% 
  # drop NaN for `prop_neverinunion`
  drop_na(prop_neverinunion)  %>% 
  # arrange by HDI
  mutate(country = country %>% as_factor %>% fct_reorder(hdi)) 


save(never, file = "out/never.rda")
