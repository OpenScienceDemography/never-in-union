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
d_all_sel <- read.csv("out/d_all_minage35_edu2.csv")

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
         TotalN >= 50) %>% 
  ungroup()

# % of childless
level_country <- never %>% 
  filter(sex == "Men",
         education == "All",
         bc_cate == "1960-1969") %>% 
  arrange(prop_childless)

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  filter(country != "NA",
         bc_cate %in% c("1950-1959", "1960-1969", "1970-1979", "1980-1989")) %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  facet_wrap(~ bc_cate) +
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
ggsave("out/childless_sex_bc.png", width = 7.5, height = 8, bg = "white")

# % of never-in-union
level_country <- never %>% 
  filter(sex == "Men",
         education == "All",
         bc_cate == "1960-1969") %>% 
  arrange(prop_neverinunion)

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  filter(education == "All",
         sex != "NA") %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  filter(country != "NA",
         bc_cate %in% c("1950-1959", "1960-1969", "1970-1979", "1980-1989")) %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = sex, colour = sex)) +
  facet_wrap(~ bc_cate) +
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
ggsave("out/neverunion_sex_bc.png", width = 7.5, height = 8, bg = "white")

# two props
never %>% 
  filter(education == "All",
         sex != "NA",
         bc_cate %in% c("1950-1959", "1960-1969", "1970-1979", "1980-1989")) %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion)) +
  facet_grid(bc_cate ~ sex) +
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
         sex != "NA",
         bc_cate %in% c("1950-1959", "1960-1969", "1970-1979", "1980-1989")) %>% 
  ggplot(aes(x = prop_childless, y = prop_neverinunion, colour = continent)) +
  facet_grid(bc_cate ~ sex) +
  geom_point() +
  scale_colour_manual(values = Mycol) +
  labs(x = "% of childless population among total population",
       y = "% of individuals who have never formed a union among childless population") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/twoprops_dot_bc.png", width = 7.5, height = 6, bg = "white")

# cohort change within country
temp <- never %>% 
  filter(education == "All",
         sex != "NA",
         bc_cate %in% c("1950-1959", "1960-1969", "1970-1979", "1980-1989")) %>% 
  select(country, sex, bc_cate, prop_childless, prop_neverinunion, continent) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(prop_childless, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(diff_6050 = `bc1960-1969` - `bc1950-1959`,
         diff_7050 = `bc1970-1979` - `bc1950-1959`,
         diff_8050 = `bc1980-1989` - `bc1950-1959`) %>% 
  gather(key = bc, value = diffs, c(diff_6050, diff_7050, diff_8050)) %>% 
  select(country, sex, continent, key, bc, diffs)

never_childless <- temp %>% 
  filter(key == 'prop_childless') %>% 
  rename(diffs_childless = diffs) %>% 
  select(-key)

temp %>% 
  filter(key == 'prop_neverinunion') %>% 
  rename(diffs_neverinunion = diffs) %>% 
  select(-key) %>% 
  left_join(never_childless, by = c("country", "sex", "continent", "bc")) %>% 
  ggplot(aes(x = diffs_childless, y = diffs_neverinunion, group = country, colour = continent)) +
  facet_grid(continent ~ sex, scales = "free") +
  geom_line() + 
  scale_colour_manual(values = Mycol) +
  labs(x = "Change in % of childless population among total population compared to 1950s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1950s cohort") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent.png", width = 7.5, height = 6, bg = "white")
