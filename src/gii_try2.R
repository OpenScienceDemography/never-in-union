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
d_all_sel <- read.csv("out/d_all_minage35_edu2_.csv")
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
  left_join(un, by = c("country", "bc_cate")) %>% 
  mutate(gii_check = ifelse(gii_ < 0.4, 1, 0))

# check the sample size
doya <- never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979"),
         edu2 %in% c("Low", "High")) %>% 
  select("country", "sex", "bc_cate", "edu2", "TotalN", "NeverInUnionN") %>% 
  mutate(non0 = ifelse(NeverInUnionN >= 10, 1, 0)) %>% 
  group_by(country) %>% 
  mutate(check = sum(non0, na.rm = T)) %>% 
  filter(check == 8)

# not separated by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         edu2 == "All") %>% 
  drop_na() %>% 
  group_by(country, sex) %>% 
  mutate(gii_sum = sum(gii_check),
         gii_level = case_when(gii_sum == 0 ~ "GII >= 0.4",
                               gii_sum == 3 ~ "GII < 0.4",
                               gii_sum %in% c(1, 2) ~ "GII >= 0.4 -> GII < 0.4"),
         gii_level = factor(gii_level, levels = c("GII < 0.4", 
                                                  "GII >= 0.4 -> GII < 0.4",
                                                  "GII >= 0.4"))) %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(sex ~ bc_cate) +
  geom_point(aes(group = country, colour = gii_level)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[2], Mycol[3], Mycol[4])) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu.png", width = 7.5, height = 6, bg = "white")

# men by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         sex == "Men",
         edu2 %in% c("Low", "High")) %>% 
  mutate(edu2 = factor(edu2, levels = c("Low", "High"))) %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(edu2 ~ bc_cate) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu_men_eduy.png", width = 7.5, height = 6, bg = "white")

# women by education
never %>% 
  filter(bc_cate %in% c("1960-1969", "1970-1979", "1980-1989"),
         sex == "Women",
         edu2 %in% c("Low", "High")) %>% 
  mutate(edu2 = factor(edu2, levels = c("Low", "High"))) %>% 
  ggplot(aes(x = gii_, y = prop_neverinunion)) +
  facet_grid(edu2 ~ bc_cate) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
  labs(x = "Gender Inequality Index", y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 13),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 10, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(2, "lines"))
ggsave("out/gii_propniu_women_eduy.png", width = 7.5, height = 6, bg = "white")

#
never %>% 
  select(country, sex, bc_cate, edu2, prop_neverinunion, continent, gii_) %>% 
  filter(edu2 %in% c("All", "Low", "High"),
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>% 
  spread(key = sex, value = prop_neverinunion) %>% 
  mutate(diff = Men / Women,
         edu2 = factor(edu2, levels = c("All", "Low", "High"))) %>% 
  filter(Women > 0) %>% 
  ggplot(aes(x = gii_, y = diff)) +
  facet_grid(bc_cate ~ edu2) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam") +
  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
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
  filter(edu2 == "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>% 
  group_by(country, sex) %>% 
  mutate(gii_sum = sum(gii_check),
         gii_level = case_when(gii_sum == 0 ~ "GII >= 0.4",
                               gii_sum == 3 ~ "GII < 0.4",
                               gii_sum %in% c(1, 2) ~ "GII >= 0.4 -> < 0.4"),
         gii_level = factor(gii_level, levels = c("GII < 0.4", 
                                                  "GII >= 0.4 -> GII < 0.4",
                                                  "GII >= 0.4"))) %>% 
  ungroup() %>% 
  select(country, sex, bc_cate, prop_childless, prop_neverinunion, gii_level) %>% 
  drop_na() %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(prop_childless, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  select(country, sex, gii_level, key, bc, ratio) %>% 
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
  left_join(never_childless, by = c("country", "sex", "gii_level", "bc"))
write.csv(fig_all, "out/dataforall.csv")

fig_all %>% 
  ggplot(aes(x = ratio_childless, y = ratio_neverinunion, group = country, colour = gii_level)) +
  facet_wrap(~ sex, scales = "free") +
  geom_line(size = 1.05) + 
  xlim(0, 4) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3], Mycol[4])) +
  labs(x = "Change in % of childless population among total population compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_60s.png", width = 7.5, height = 6, bg = "white")

#
temp <- never %>% 
  filter(edu2 == "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>%
  group_by(country, sex) %>% 
  mutate(gii_sum = sum(gii_check),
         gii_level = case_when(gii_sum == 0 ~ "GII >= 0.4",
                               gii_sum == 3 ~ "GII < 0.4",
                               gii_sum %in% c(1, 2) ~ "GII >= 0.4 -> GII < 0.4"),
         gii_level = factor(gii_level, levels = c("GII < 0.4", 
                                                  "GII >= 0.4 -> GII < 0.4",
                                                  "GII >= 0.4"))) %>% 
  ungroup() %>% 
  select(country, sex, bc_cate, gii_, prop_neverinunion, gii_level) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(gii_, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  select(country, sex, gii_level, key, bc, ratio) %>% 
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
  left_join(never_childless, by = c("country", "sex", "gii_level", "bc"))
write.csv(fig_all, "out/dataforall.csv")

fig_all %>% 
  ggplot(aes(x = ratio_gii, y = ratio_neverinunion, group = country, colour = gii_level)) +
  facet_wrap(~ sex, scales = "free") +
  geom_line(size = 1.05) + 
  scale_colour_manual(values = c(Mycol[2], Mycol[3], Mycol[4])) +
  labs(x = "Change in % of Gender Inequality Index compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_gii.png", width = 7.5, height = 6, bg = "white")

# different categorisation
fig_all %>% 
  ggplot(aes(x = ratio_gii, y = ratio_neverinunion, group = country)) +
  facet_grid(gii_level ~ sex, scales = "free") +
  geom_line() + 
  #scale_colour_manual(values = c(Mycol[2], Mycol[3], Mycol[4])) +
  labs(x = "Change in % of Gender Inequality Index compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_gii_sep.png", width = 7.5, height = 6, bg = "white")


# by education years
temp <- never %>% 
  filter(edu2 != "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>%
  group_by(country, sex, edu2) %>% 
  mutate(gii_sum = sum(gii_check),
         gii_level = case_when(gii_sum == 0 ~ "GII >= 0.4",
                               gii_sum == 3 ~ "GII < 0.4",
                               gii_sum %in% c(1, 2) ~ "GII >= 0.4 -> GII < 0.4"),
         gii_level = factor(gii_level, levels = c("GII < 0.4", 
                                                  "GII >= 0.4 -> GII < 0.4",
                                                  "GII >= 0.4"))) %>% 
  ungroup() %>% 
  select(country, sex, edu2, bc_cate, gii_, prop_neverinunion, gii_level) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(gii_, prop_neverinunion)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  select(country, sex, edu2, gii_level, key, bc, ratio) %>% 
  filter(ratio >= 0, !is.infinite(ratio))

onlyf <- temp %>% 
  group_by(country, edu2) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  filter(sex == "Women")

all <- temp %>% 
  group_by(country, edu2) %>% 
  count(sex) %>% 
  filter(n == 6) %>% 
  ungroup() %>% 
  count(country, edu2) %>% 
  filter(n == 2)
write.csv(all, "out/dataforall.csv")

never_childless <- temp %>% 
  right_join(all, by = c("country", "edu2")) %>% 
  filter(key == "gii_") %>% 
  rename(ratio_gii = ratio) %>% 
  select(-key)

fig_all <- temp %>% 
  right_join(all, by = c("country", "edu2")) %>% 
  filter(key == "prop_neverinunion") %>% 
  rename(ratio_neverinunion = ratio) %>% 
  select(-key) %>% 
  left_join(never_childless, by = c("country", "sex", "edu2", "gii_level", "bc"))
write.csv(fig_all, "out/dataforall.csv")

fig_all %>% 
  ggplot(aes(x = ratio_gii, y = ratio_neverinunion, group = country, colour = gii_level)) +
  facet_grid(edu2 ~ sex, scales = "free") +
  geom_line(size = 1.05) + 
  scale_colour_manual(values = c(Mycol[2], Mycol[3], Mycol[4])) +
  labs(x = "Change in % of Gender Inequality Index compared to 1960s cohort",
       y = "Change in % of individuals who have never formed a union among childless population compared to 1960s cohort") +
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave("out/changes_bc_continent_gii_eduy.png", width = 7.5, height = 6, bg = "white")

### ratio of men / women
never_ratioMW <- never %>% 
  filter(edu2 != "All",
         sex != "NA",
         bc_cate %in% c("1960-1969", "1970-1979", "1980-1989")) %>%
  group_by(country, sex, edu2) %>% 
  mutate(gii_sum = sum(gii_check),
         gii_level = case_when(gii_sum == 0 ~ "GII >= 0.4",
                               gii_sum == 3 ~ "GII < 0.4",
                               gii_sum %in% c(1, 2) ~ "GII >= 0.4 -> GII < 0.4"),
         gii_level = factor(gii_level, levels = c("GII < 0.4", 
                                                  "GII >= 0.4 -> GII < 0.4",
                                                  "GII >= 0.4"))) %>% 
  ungroup() %>% 
  select(country, sex, edu2, bc_cate, gii_, prop_childless, prop_neverinunion, gii_level, continent) %>% 
  mutate(bc_cate = paste0("bc", bc_cate)) %>% 
  gather(key = key, value = props, c(prop_childless, prop_neverinunion)) %>% 
  spread(key = sex, value = props) %>% 
  mutate(ratioMW = Men / Women) %>% 
  select(-Men, -Women)

ratioratio_childless <- never_ratioMW %>% 
  filter(key == "prop_childless") %>% 
  gather(key = key, value = props, c(gii_, ratioMW)) %>% 
  spread(key = bc_cate, value = props) %>% 
  mutate(set_60s = 1,
         ratio_7060 = `bc1970-1979` / `bc1960-1969`,
         ratio_8060 = `bc1980-1989` / `bc1960-1969`) %>% 
  select(-c("bc1960-1969", "bc1970-1979", "bc1980-1989")) %>% 
  gather(key = bc, value = ratio, c(set_60s, ratio_7060, ratio_8060)) %>% 
  spread(key = key, value = ratio) %>% 
  filter(ratioMW >= 0, !is.infinite(ratioMW), !is.na(gii_level))

ratioratio_childless %>% 
  ggplot(aes(x = gii_, y = ratioMW, group = country)) +
  facet_grid(edu2 ~ gii_level) +
  geom_point() +
  geom_line() + 
  labs(x = "Ratios of GII", y = "Ratios of % of childless") +
  theme_minimal()
ggsave("out/ratios-ratios-propchildless.png", width = 7.5, height = 6, bg = "white")

#
never_ratioMW %>% 
  filter(key == "prop_neverinunion",
         !is.na(gii_level),
         !is.na(ratioMW)) %>% 
  group_by(country, edu2) %>% 
  mutate(check = n()) %>% 
  filter(check == 3) %>% 
  mutate(bc_cate = factor(bc_cate, levels = c("bc1980-1989", "bc1970-1979", "bc1960-1969"))) %>% 
  ggplot(aes(x = bc_cate, y = ratioMW, group = country)) +
  facet_grid(edu2 ~ gii_level) +
  geom_point() +
  geom_line() +
  labs(x = "Birth cohort", y = "Ratios of % of never-in-union") +
  theme_minimal()
ggsave("out/ratiosMW-propniu.png", width = 7.5, height = 6, bg = "white")
