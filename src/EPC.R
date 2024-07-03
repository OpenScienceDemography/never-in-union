#### try to explore a scatter version to show our findings

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
d_all_sel <- read.csv("out/d_all_minage35_edu2_.csv")
un <- read.dta13("out/un_data_90001020.dta", nonint.factors = T, convert.factors = T)

d_un <- un %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979")) %>% 
  group_by(country) %>% 
  summarise(gii = mean(gii_))

gap <- gapminder_unfiltered %>% 
  #clean_names() %>% 
  filter(year == 1992) %>% 
  mutate(
    iso3 = country %>% countrycode(origin = "country.name", destination = "iso3c"),
    continent = continent %>% paste
  ) %>% 
  select(country, continent)

never <- d_all_sel %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979"),
         edu2 == "All",
         !is.na(sex)) %>% 
  mutate(TotalN = ifelse(is.na(TotalN), 0, TotalN),
         ChildlessN = ifelse(is.na(ChildlessN), 0, ChildlessN),
         NeverInUnionN = ifelse(is.na(NeverInUnionN), 0, NeverInUnionN)) %>% 
  filter(TotalN >= 25) %>% # arbitrary
  group_by(country, sex, edu2) %>% 
  summarise(n_total = sum(TotalN),
            n_childless = sum(ChildlessN),
            n_niu = sum(NeverInUnionN)) %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 2) %>% 
  left_join(gap, by = "country")  %>% 
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "D.R.Congo", country),
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
  filter(n_total >= 50) %>%  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100) %>% 
  left_join(d_un, by = "country") %>% 
  filter(!is.na(gii))
write.csv(never, "out/dataset_forEPC_alledu.csv")

never_edu2 <- d_all_sel %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979"),
         edu2 %in% c("Low", "High"),
         !is.na(sex)) %>% 
  mutate(TotalN = ifelse(is.na(TotalN), 0, TotalN),
         ChildlessN = ifelse(is.na(ChildlessN), 0, ChildlessN),
         NeverInUnionN = ifelse(is.na(NeverInUnionN), 0, NeverInUnionN)) %>% 
  filter(TotalN >= 15) %>% # arbitrary
  group_by(country, sex, edu2) %>% 
  summarise(n_total = sum(TotalN),
            n_childless = sum(ChildlessN),
            n_niu = sum(NeverInUnionN)) %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 4) %>% 
  left_join(gap, by = "country")  %>% 
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "D.R.Congo", country),
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
  filter(n_total >= 30) %>%  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100,
         edu2 = factor(edu2, levels = c("Low", "High"))) %>% 
  left_join(d_un, by = "country") %>% 
  filter(!is.na(gii))
write.csv(never_edu2, "out/dataset_forEPC_byedu2.csv")
  
# % of childless
level_country <- never %>% 
  filter(sex == "Men") %>% 
  arrange(desc(gii))

country_num <- level_country %>% 
  select(country) %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

never %>% 
  mutate(country = factor(country, levels = level_country$country)) %>% 
  ggplot(aes(x = p_childless, y = country, group = sex, colour = sex)) +
  geom_hline(yintercept = country_num$country, linewidth = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c(col7[3], col7[5])) +
  labs(x = "% of childless population among total population") +
  theme_minimal() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 7, face = 2))
ggsave("out/prop-childless_sex.png", width = 9, height = 7.5, bg = "white")

# x: GII, y: % of never-in-union among childless population
never %>% 
  filter(edu2 == "All") %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_wrap(~ sex) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = Mycol) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15, face = 2),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_p-niu.png", width = 9, height = 6.5, bg = "white")

# x: GII, y: ratio of % never-in-union
never %>% 
  select(country, sex, edu2, p_niu, continent, gii) %>% 
  filter(edu2 == "All") %>% 
  spread(key = sex, value = p_niu) %>% 
  mutate(diff = Men / Women) %>% 
  filter(Women > 0) %>% 
  ggplot(aes(x = gii, y = diff)) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
  labs(x = "Gender Inequality Index", y = "Ratio of Men / Women") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15, face = 2),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_p-niu_genderdiff.png", width = 7.5, height = 5, bg = "white")

# x: GII, y: ratio of % never-in-union by education
ratio_edu <- d_all_sel %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979"),
         edu2 %in% c("Low", "High"),
         !is.na(sex)) %>% 
  mutate(TotalN = ifelse(is.na(TotalN), 0, TotalN),
         ChildlessN = ifelse(is.na(ChildlessN), 0, ChildlessN),
         NeverInUnionN = ifelse(is.na(NeverInUnionN), 0, NeverInUnionN)) %>% 
  filter(TotalN >= 15) %>% # arbitrary
  group_by(country, sex, edu2) %>% 
  summarise(n_total = sum(TotalN),
            n_childless = sum(ChildlessN),
            n_niu = sum(NeverInUnionN)) %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 4) %>% 
  left_join(gap, by = "country")  %>% 
  mutate(country = ifelse(country == "Democratic Republic of the Congo", "D.R.Congo", country),
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
  filter(n_total >= 30) %>%  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100,
         edu2 = factor(edu2, levels = c("Low", "High"))) %>% 
  left_join(d_un, by = "country") %>% 
  filter(!is.na(gii)) %>% 
  select(country, sex, edu2, p_niu, continent, gii) %>% 
  spread(key = sex, value = p_niu) %>% 
  mutate(diff = Men / Women) %>% 
  filter(Women > 0)
write.csv(ratio_edu, "out/ratio_forEPC_byedu2.csv")

ratio_edu %>% 
  ggplot(aes(x = gii, y = diff)) +
  facet_wrap(~ edu2) + 
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
  ylim(0, 10) +
  labs(x = "Gender Inequality Index", y = "Ratio of Men / Women") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15, face = 2),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_p-niu_genderdiff_edu.png", width = 9, height = 7.5, bg = "white")
