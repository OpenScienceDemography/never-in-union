names(hdi) <- c("country", "hdi")

hdi <- hdi %>% 
  mutate(country = case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "Türkiye" ~ "Turkey",
                             country == "Congo (Democratic Republic of the)" ~ "DR.Congo",
                             country == "Côte d'Ivoire" ~ "Ivory Coast",
                             country == "Moldova (Republic of)" ~ "Moldova",
                             country == "Russian Federation" ~ "Russia",
                             country == "Eswatini (Kingdom of)" ~ "Swaziland",
                             country == "Tanzania (United Republic of)" ~ "Tanzania",
                             T ~ country))
