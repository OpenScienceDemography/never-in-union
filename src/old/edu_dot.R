d_edu <- read.csv("out/old/D_all_minage35_edu2.csv")

col7 <- c("#332288", "#88CCEE", "#117733", "#999933", "#FD8D3C", "#882255", "#DDDDDD")

d_edu <- d_edu %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "D.R.Congo", 
                             country == "CzechRepublic" ~ "Czechia",
                             T~ country),
         continent = case_when(country %in% c("Sweden", "Germany", "Netherlands", "Belgium",
                                              "Austria", "France", "Italy", "Estonia", "Czechia",
                                              "Poland", "Hungary", "Romania", "Albania", "Bulgaria",
                                              "Republic of Moldova", "Turkey") ~ "Europe",
                               country %in% c("Lithuania", "Russia", "Kazakhstan", "Georgia",
                                              "Ukraine", "Armenia", "Kyrgyzstan") ~ "FSU",
                               country %in% c("Dominican Republic", "Peru", "Brazil", "Colombia",
                                              "Guyana", "Bolivia", "Guatemala", "Honduras", "Haiti",
                                              "Nicaragua", "Sao Tome and Principe") ~ "Americas",
                               country %in% c("Maldives", "Azerbaijan", "Indonesia", "Philippines", "India",
                                              "Timor-leste", "Cambodia", "Myanmar", "Nepal",
                                              "Timor-Leste") ~ "Asia",
                               country %in% c("South Africa", "Gabon", "Ghana", "Namibia", "Swaziland",
                                              "Zimbabwe", "Angola", "Cameroon", "Kenya", "Congo", "Comoros",
                                              "Mauritania", "Ivory Coast", "Togo", "Nigeria", "Lesotho",
                                              "Malawi", "Madagascar", "Ethiopia", "D.R.Congo",
                                              "Guinea", "Burkina Faso", "Mozambique", "Burundi", "Chad",
                                              "Benin", "Liberia", "Mali", "Niger", "Senegal",
                                              "Sierra Leone", "Tanzania", "Uganda", "Zambia") ~ "Africa")) %>% 
  filter(bc_cate == "1960-1969",
         education %in% c("Low", "High"),
         TotalN >= 1,
         country %out% c("Central African Republic", "Gambia", "Rwanda", "Uganda", "Liberia"))

# % of childless
level_country <- d_edu %>% 
  filter(sex == "Men",
         education == "High") %>% 
  arrange(prop_childless)

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

d_edu %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  ggplot(aes(x = prop_childless, y = country, group = education, colour = education)) +
  facet_wrap(~ sex) +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(col7[2], col7[6])) +
  labs(x = "% of childless population among total population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2))
ggsave("out/childless_sex_edu.png", width = 7.5, height = 8, bg = "white")



# two props
d_edu %>% 
  filter(country %out% "Italy") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent, shape = education)) +
  facet_wrap(~ sex) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_colour_manual(values = col7[1:5]) +
  scale_shape_manual(values = c(1, 17)) +
  #scale_fill_manual(values = col7[6:7]) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot.png", width = 7.5, height = 6, bg = "white")
