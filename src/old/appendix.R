# load the prepared dataset
d_all_sel <- read.csv("out/d_all_minage35_edu2_.csv")

# ---- Robustness check 1: using GII 1980s
d_un_80s <- un %>% 
  filter(bc_cate == "1980-1989") %>% 
  distinct(country, gii_)

gap <- gapminder_unfiltered %>% 
  filter(year == 1992) %>% 
  mutate(
    iso3 = country %>% countrycode(origin = "country.name", destination = "iso3c"),
    continent = continent %>% paste
  ) %>% 
  select(country, continent)

never_80s <- d_all_sel %>% 
  filter(
    bc_cate %in% c("1960-1969", "1970-1979"),
    edu2 == "All",
    !is.na(sex)
  ) %>% 
  mutate(
    TotalN = ifelse(is.na(TotalN), 0, TotalN),
    ChildlessN = ifelse(is.na(ChildlessN), 0, ChildlessN),
    NeverInUnionN = ifelse(is.na(NeverInUnionN), 0, NeverInUnionN)
  ) %>% 
  filter(TotalN >= 25) %>% 
  group_by(country, sex, edu2) %>% 
  summarise(
    n_total = sum(TotalN),
    n_childless = sum(ChildlessN),
    n_niu = sum(NeverInUnionN),
    .groups = "drop"
  ) %>% 
  group_by(country) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 2) %>% 
  left_join(gap, by = "country") %>% 
  mutate(
    country = ifelse(country == "Democratic Republic of the Congo", "D.R.Congo", country),
    continent = case_when(continent == "Europe" ~ "Europe & North America", 
                          continent == "Americas" ~ "Latin America",
                          T ~ continent),
    continent = case_when(country %in% c("Lithuania", "Ukraine", "Czechia",
                                         "Republic of Moldova", "Slovakia",
                                         "Russia", "Belarus",
                                         "The UK", "The US", "Canada") ~ "Europe & North America",
                          country %in% c("Kazakhstan", "Armenia", "Georgia",
                                         "Kyrgyzstan", "Turkey") ~ "Asia",
                          country %in% c("D.R.Congo", "Ivory Coast", "Congo") ~ "Africa",
                          T ~ continent)
  ) %>% 
  filter(n_total >= 50) %>% 
  mutate(
    p_childless = n_childless / n_total * 100,
    p_niu = n_niu / n_childless * 100
  ) %>% 
  left_join(d_un_80s, by = "country") %>% 
  filter(!is.na(gii_))

# Figure 2 robustness check: x = 1980s GII, y = % never-in-union among childless
p_gii80 <- never_80s %>% 
  ggplot(aes(x = gii_, y = p_niu)) +
  facet_wrap(~ sex) +
  geom_point(aes(colour = continent), size = 2) +
  geom_smooth(
    method = "gam",
    se = FALSE,
    linewidth = 2,
    colour = scales::alpha(col7[2], 0.7)
  ) +
  scale_colour_manual(values = Mycol) +
  coord_cartesian(xlim = c(0, 0.8)) +
  labs(
    x = "Gender Inequality Index (year 2020)",
    y = "% never-formed-union among childless population"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 18),
    legend.title = element_blank(),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 15, face = 2),
    strip.text = element_text(size = 18),
    panel.spacing = unit(2, "lines")
  )

p_gii80

ggsave("out/gii_p-niu_1980s.png", p_gii80, width = 9, height = 6.5, bg = "white")

# ---- Robustness check 2: Min age 40
ESS39 <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/ESS39.rds")
d_hh <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/d_hh.rds")
d_dhs <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/dhs_sel.rds")

d_all <- d_hh %>% 
  bind_rows(d_dhs) %>% 
  bind_rows(ESS39)

Minage = 45

d_all_sel_ap <- func_makedata2(oridata = d_all, minage = Minage)
d_all_sel_ap <- d_all_sel_ap %>% 
  mutate(prop_childless = round((ChildlessN / TotalN) * 100, 1),
         prop_neverinunion = round((NeverInUnionN / ChildlessN) * 100, 1),
         edu2 = factor(edu2, levels = c("All", "Low", "High")))

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

never <- d_all_sel_ap %>% 
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
         continent = case_when(country %in% c("Lithuania", "Ukraine", "Czechia",
                                              "Republic of Moldova", "Slovakia",
                                              "Russia", "Belarus", "Latvia",
                                              "The UK", "The US", "Canada") ~ "Europe & North America",
                               country %in% c("Kazakhstan", "Armenia", "Georgia",
                                              "Kyrgyzstan", "Turkey") ~ "Asia",
                               country %in% c("D.R.Congo", "Ivory Coast", "Congo") ~ "Africa",
                               T ~ continent)) %>% 
  filter(n_total >= 50) %>%  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = n_niu / n_childless * 100) %>% 
  left_join(d_un, by = "country") %>% 
  filter(!is.na(gii))

never %>% 
  filter(edu2 == "All") %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_wrap(~ sex) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam", se = F, linewidth = 2, 
              colour = scales::alpha(col7[2], 0.7)) +
  scale_colour_manual(values = Mycol) +
  xlim(0, 0.8) +
  labs(x = "Gender Inequality Index", y = paste0("% never-formed-union among childless population (", Minage, "+)")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 18),
        legend.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 15, face = 2),
        strip.text = element_text(size = 18),
        panel.spacing = unit(2, "lines"))
ggsave(paste0("out/gii_p-niu_", Minage, "p.png"), width = 9, height = 6.5, bg = "white")
