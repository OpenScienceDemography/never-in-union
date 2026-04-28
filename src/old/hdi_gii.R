#### To explore Ewa's idea

# prepare the session
library(tidyverse)
library(countrycode)
library(readstata13)
library(readxl)
library(foreign)
library(egg)
library(ggrepel)
library(gapminder)
`%out%` = Negate(`%in%`)

col7 <- c("#332288", "#88CCEE", "#117733", "#999933", "#FD8D3C", "#882255", "#DDDDDD")
Mycol <- c("#08306B", "#238B45", "#FD8D3C", "#D4B9DA", "#FFEDA0")

# load the prepared dataset
d_all_sel <- read.csv("out/d_all_minage35_edu2.csv")
un <- read.dta13("out/un_data_90001020.dta", nonint.factors = T, convert.factors = T)

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
  ungroup() %>% 
  left_join(un, by = c("country", "bc_cate"))

# not separated by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         education == "All") %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(sex ~ bc_cate) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = Mycol) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.spacing = unit(2, "lines"))

# men by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         sex == "Men",
         education %in% c("Low", "High")) %>%
  mutate(education = factor(education, levels = c("Low", "High"))) %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(education ~ bc_cate) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = Mycol) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu_men.png", width = 7.5, height = 6, bg = "white")

# women by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         sex == "Women",
         education %in% c("Low", "High")) %>%
  mutate(education = factor(education, levels = c("Low", "High"))) %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(education ~ bc_cate) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = Mycol) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu_women.png", width = 7.5, height = 6, bg = "white")

#
never %>% 
  select(country, sex, bc_cate, education, prop_neverinunion, continent, gii_) %>% 
  filter(education %in% c("All", "Low", "High"),
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>% 
  spread(key = sex, value = prop_neverinunion) %>% 
  mutate(diff = Men / Women,
         education = factor(education, levels = c("All", "Low", "High"))) %>% 
  filter(Women > 0) %>% 
  ggplot(aes(x = gii_, y = diff)) +
  facet_grid(bc_cate ~ education) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = Mycol) +
  ylim(0, 5) + # there are some outlier
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu_genderdiff.png", width = 7.5, height = 6, bg = "white")
  
## cohort change within country
temp <- never %>% 
  filter(education == "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>% 
  select(country, sex, bc_cate, prop_childless, prop_neverinunion, continent) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(prop_childless, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  select(country, sex, continent, key, bc, ratio) %>% 
  filter(ratio >= 0, !is.infinite(ratio))

onlyf <- temp %>% 
  group_by(country) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  filter(sex == "Women")

all <- temp %>% 
  group_by(country) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  ungroup() %>% 
  count(country) %>% 
  filter(n == 2)
write.csv(all, "out/dataforall.csv")

never_childless <- temp %>% 
  right_join(all, by = "country") %>% 
  filter(key == 'prop_childless') %>% 
  rename(ratio_childless = ratio) %>% 
  select(-key)

fig_all <- temp %>% 
  right_join(all, by = "country") %>% 
  filter(key == 'prop_neverinunion') %>% 
  rename(ratio_neverinunion = ratio) %>% 
  select(-key) %>% 
  left_join(never_childless, by = c("country", "sex", "continent", "bc"))
write.csv(fig_all, "out/dataforall.csv")

fig_all %>% 
  ggplot(aes(x = ratio_childless, y = ratio_neverinunion, group = country, colour = continent)) +
  facet_wrap(~ sex, scales = "free") +
  geom_line(size = 1.05) + 
  xlim(0, 4) +
  scale_colour_manual(values = c(Mycol[1:4], col7[4])) +
  labs(x = "Change in % of childless population among total population compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_60s.png", width = 7.5, height = 6, bg = "white")

#
temp <- never %>% 
  filter(education == "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>% 
  select(country, sex, bc_cate, gii_, prop_neverinunion, continent) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(gii_, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  select(country, sex, continent, key, bc, ratio) %>% 
  filter(ratio >= 0, !is.infinite(ratio))

onlyf <- temp %>% 
  group_by(country) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  filter(sex == "Women")

all <- temp %>% 
  group_by(country) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  ungroup() %>% 
  count(country) %>% 
  filter(n == 2)
write.csv(all, "out/dataforall.csv")

never_childless <- temp %>% 
  right_join(all, by = "country") %>% 
  filter(key == "gii_") %>% 
  rename(ratio_gii = ratio) %>% 
  select(-key)

fig_all <- temp %>% 
  right_join(all, by = "country") %>% 
  filter(key == "prop_neverinunion") %>% 
  rename(ratio_neverinunion = ratio) %>% 
  select(-key) %>% 
  left_join(never_childless, by = c("country", "sex", "continent", "bc"))
write.csv(fig_all, "out/dataforall.csv")

fig_all %>% 
  ggplot(aes(x = ratio_gii, y = ratio_neverinunion, group = country, colour = continent, label = country)) +
  facet_wrap(~ sex, scales = "free") +
  geom_line(size = 1.05) + 
  geom_label() +
  scale_colour_manual(values = c(Mycol[1:4], col7[4])) +
  labs(x = "Change in % of Gender Inequality Index compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_gii.png", width = 7.5, height = 6, bg = "white")

###
never_childless <- temp %>% 
  right_join(onlyf, by = c("country", "sex")) %>% 
  filter(key == 'prop_childless') %>% 
  rename(ratio_childless = ratio) %>% 
  select(-key)

fig_women <- temp %>% 
  right_join(onlyf, by = c("country", "sex")) %>% 
  filter(key == 'prop_neverinunion') %>% 
  rename(ratio_neverinunion = ratio) %>% 
  select(-key) %>% 
  left_join(never_childless, by = c("country", "continent", "bc"))
write.csv(fig_women, "out/dataforwomen.csv")

fig_women %>% 
  ggplot(aes(x = ratio_childless, y = ratio_neverinunion, group = country, colour = continent)) +
  geom_line() + 
  scale_colour_manual(values = c(Mycol[1:4], col7[4])) +
  labs(x = "Ratio of childless pop among total pop",
       y = "Ratio of those who have never formed a union among childless pop") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
ggsave("out/changes_bc_continent_onlywomen.png", width = 7.5, height = 6, bg = "white")
