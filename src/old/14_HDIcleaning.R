names(hdi) <- c("country", "hdi")

hdi <- hdi %>% 
  mutate(country = case_when(country == "Bolivia (Plurinational State of)" ~ "Bolivia",
                             country == "TÃ¼rkiye" ~ "Turkey",
                             country == "Congo (Democratic Republic of the)" ~ "DR.Congo",
                             country == "CÃ´te d'Ivoire" ~ "Ivory Coast",
                             country == "Moldova (Republic of)" ~ "Moldova",
                             country == "Russian Federation" ~ "Russia",
                             country == "Eswatini (Kingdom of)" ~ "Swaziland",
                             country == "Tanzania (United Republic of)" ~ "Tanzania",
                             T ~ country))
