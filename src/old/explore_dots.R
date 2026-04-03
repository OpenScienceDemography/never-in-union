#### try to explore a scatter version to show our findings

# prepare the session
library(tidyverse)
library(countrycode)
library(readstata13)
library(readxl)
library(foreign)
library(egg)
library(ggrepel)

`%out%` = Negate(`%in%`)

col7 <- c("#332288", "#88CCEE", "#117733", "#999933", "#FD8D3C", "#882255", "#DDDDDD")
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# load the prepared dataset
d_all_sel <- read.csv("out/old/D_all_minage35_edu2.csv")

library(gapminder)

gap <- gapminder_unfiltered %>% 
  #clean_names() %>% 
  filter(year == 1992) %>% 
  mutate(
    iso3 = country %>% countrycode(origin = "country.name", destination = "iso3c"),
    continent = continent %>% paste
  ) %>% 
  select(country, continent)

never <- d_all_sel %>% 
  left_join(gap, by = "country") %>% 
  mutate(country = case_when(country == "Democratic Republic of the Congo" ~ "D.R.Congo", 
                             T ~ country),
         continent = case_when(continent == "Europe" ~ "Europe & North America", 
                               continent == "Americas" ~ "Latin America",
                               T ~ continent),
         continent = case_when(country == "Kyrgyzstan" ~ "FSU",
                               country == "Congo" ~ "Africa",
                               country == "Czechia" ~ "Europe & North America",
                               country == "D.R.Congo" ~ "Africa",
                               country == "Republic of Moldova" ~ "FSU",
                               country == "Slovakia" ~ "FSU",
                               country == "The UK" ~ "Europe & North America",
                               country == "The US" ~ "Europe & North America",
                               country == "Canada" ~ "Europe & North America",
                               country == "Ivory Coast" ~ "Africa",
                               T ~ continent)) %>% 
  filter(country != is.na(country),
         bc_cate == "1960-1969") %>% 
  ungroup()

# % of childless
level_country <- never %>% 
  filter(sex == "Men",
         education == "All") %>% 
  arrange(prop_childless)

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  filter(country != "NA") %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(col7[3], col7[5])) +
  labs(x = "% of childless population among total population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2))
ggsave("out/childless_sex.png", width = 7.5, height = 8, bg = "white")

# % of never-in-union
level_country <- never %>% 
  filter(sex == "Men",
         education == "All") %>% 
  arrange(prop_neverinunion)

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  filter(country != "NA") %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = sex, colour = sex)) +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(col7[3], col7[5])) +
  labs(x = "% of individuals who have never formed union") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2))
ggsave("out/neverunion_sex.png", width = 7.5, height = 8, bg = "white")

# two props
never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion)) +
  facet_wrap(~ sex) +
  geom_point() +
  #stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot.png", width = 7.5, height = 6, bg = "white")

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent)) +
  facet_wrap(~ sex) +
  geom_point() +
  scale_colour_manual(values = Mycol) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot.png", width = 7.5, height = 6, bg = "white")

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent, label = country)) +
  facet_wrap(~ sex) +
  geom_point() +
  geom_text_repel(size = 1.5,
                  nudge_x = 0.25,
                  nudge_y = 0.25,
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.3, "lines")) +
  scale_colour_manual(values = Mycol) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot.png", width = 7.5, height = 6, bg = "white")



never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent)) +
  facet_wrap(~ sex) +
  geom_point() +
  geom_smooth(method = lm, se = F) +
  scale_colour_manual(values = col7[1:5]) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot_fit.png", width = 7.5, height = 6, bg = "white")


# add a fitted line except Europe
fitline <- never %>% 
  filter(continent %out% "Europe") 

ols_m <- lm(prop_neverinunion ~ prop_childless, data = fitline %>% filter(sex == "Men"))
summary(ols_m)

ols_w <- lm(prop_neverinunion ~ prop_childless, data = fitline %>% filter(sex == "Women"))
summary(ols_w)

never %>% 
  filter(continent %out% "Europe") %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion)) +
  facet_wrap(~ sex) +
  geom_smooth(method = lm, se = F,
              colour = "#FFEDA0", size = 5, alpha = 0.8) +
  geom_point(aes(colour = continent)) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot_fit_excptEurope.png", width = 7.5, height = 6, bg = "white")
