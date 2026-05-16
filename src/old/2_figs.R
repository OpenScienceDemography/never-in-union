# load the prepared dataset
d_all_sel <- read.csv("out/d_all_minage35_edu2_.csv")

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
         continent = case_when(country %in% c("Lithuania", "Ukraine", "Czechia",
                                              "Republic of Moldova", "Slovakia",
                                              "Russia", "Belarus",
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
write.csv(never, "out/dataset_alledu.csv")

unique(d_all_sel$country)
unique(never$country)

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
         continent = case_when(country %in% c("Lithuania", "Ukraine", "Czechia",
                                              "Republic of Moldova", "Slovakia",
                                              "Russia", "Belarus",
                                              "The UK", "The US", "Canada") ~ "Europe & North America",
                               country %in% c("Kazakhstan", "Armenia", "Georgia",
                                              "Kyrgyzstan", "Turkey") ~ "Asia",
                               country %in% c("D.R.Congo", "Ivory Coast", "Congo") ~ "Africa",
                               T ~ continent)) %>% 
  filter(n_total >= 30) %>%  # arbitrary
  mutate(p_childless = n_childless / n_total * 100,
         p_niu = if_else(n_childless > 0, n_niu / n_childless * 100, NA_real_),
         edu2 = factor(edu2, levels = c("Low", "High"))) %>% 
  left_join(d_un, by = "country") %>% 
  filter(!is.na(gii))
write.csv(never_edu2, "out/dataset_byedu2.csv")

# ---- Tab 1
func_table1_samplesize <- function(oridata, minage){
  out <- oridata %>%
    filter(age >= minage,
           bc_cate %in% c("1960-1969", "1970-1979")) %>%
    group_by(datasetname, country) %>%
    summarise(
      sample_n = n(),
      .groups = "drop"
    )
  
  return(out)
}

table1_n <- func_table1_samplesize(d_all, minage = 35)

source_info <- table1_n %>%
  group_by(country) %>%
  summarise(
    `Data source` = paste(unique(datasetname), collapse = "; "),
    `Sample size` = sum(sample_n),
    .groups = "drop"
  )

table1 <- never %>%
  # Table 1 は全体（All education）のみ
  filter(edu2 == "All") %>%
  
  # country × sex で1行に集約
  group_by(country, sex, continent) %>%
  summarise(
    n_total = sum(n_total),
    p_childless = mean(p_childless),
    p_niu = mean(p_niu),
    gii = mean(gii),
    .groups = "drop"
  ) %>%
  
  # 男女を列に展開
  pivot_wider(
    names_from = sex,
    values_from = c(n_total, p_childless, p_niu),
    names_sep = "_"
  ) %>%
  
  # data source + sample size を追加
  left_join(source_info, by = "country") %>%
  
  # 表示用
  transmute(
    Country = country,
    `Data source`,
    continent,
    `Sample size`,
    GII = round(gii, 3),
    `% childless (men)` = round(p_childless_Men, 1),
    `% childless (women)` = round(p_childless_Women, 1),
    `% never-in-union among childless (men)` = round(p_niu_Men, 1),
    `% never-in-union among childless (women)` = round(p_niu_Women, 1)
  ) %>%
  arrange(GII)
write.csv(table1, "out/table1.csv", row.names = F)

table1_summary_region <- table1 %>%
  group_by(continent) %>%
  summarise(
    Countries = n(),
    `GII (min–max)` =
      paste0(round(min(GII), 3), "–", round(max(GII), 3)),
    
    `Childless men (%)` =
      paste0(
        round(mean(`% childless (men)`), 1),
        " (", round(min(`% childless (men)`), 1),
        "–", round(max(`% childless (men)`), 1), ")"
      ),
    
    `Childless women (%)` =
      paste0(
        round(mean(`% childless (women)`), 1),
        " (", round(min(`% childless (women)`), 1),
        "–", round(max(`% childless (women)`), 1), ")"
      ),
    
    `Never-union men (%)` =
      paste0(
        round(mean(`% never-in-union (men)`), 1),
        " (", round(min(`% never-in-union (men)`), 1),
        "–", round(max(`% never-in-union (men)`), 1), ")"
      ),
    
    `Never-union women (%)` =
      paste0(
        round(mean(`% never-in-union (women)`), 1),
        " (", round(min(`% never-in-union (women)`), 1),
        "–", round(max(`% never-in-union (women)`), 1), ")"
      ),
    .groups = "drop"
  ) %>%
  arrange(continent)
write.csv(table1_summary_region, "out/table1_summary_region.csv")

# ---- Figure 1
# % of childless
abbr <- c(
  "Dominican Republic"    = "DR",
  "Sao Tome and Principe" = "STP",
  "Republic of Moldova"   = "Moldova"
)

country_order <- never %>%
  filter(sex == "Men") %>%
  arrange(desc(gii)) %>%
  pull(country)

plot_df <- never %>%
  mutate(country_plot = recode(country, !!!abbr),
         country_plot = factor(country_plot, levels = recode(country_order, !!!abbr)),
         y = as.numeric(country_plot))

hline_y <- tibble(country = country_order) %>%
  mutate(country_plot = recode(country, !!!abbr),
         y = as.numeric(factor(country_plot, levels = levels(plot_df$country_plot)))) %>%
  arrange(y) %>%
  mutate(rank = row_number()) %>%
  filter(rank %% 5 == 0) %>%
  pull(y)

hline_y <- seq(5, max(plot_df$y, na.rm = TRUE), by = 5)

plot_df |> 
  ggplot(aes(x = p_childless, y = y, colour = sex)) +
  geom_hline(yintercept = hline_y, linewidth = 1.0, colour = "grey85") +
  #geom_hline(yintercept = hline_y, linewidth = 0.6, colour = "grey92") +
  geom_line(aes(group = country_plot), colour = "grey78", linewidth = 0.5) +
  geom_point(size = 2.8) +
  scale_colour_manual(values = c(col7[3], col7[5])) +
  scale_y_continuous(breaks = seq_along(levels(plot_df$country_plot)),
                     labels = levels(plot_df$country_plot),
                     expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "% childless (among population aged 35+)",
       y = NULL,
       colour = NULL,
    #caption = "Abbreviations: DR = Dominican Republic; STP = Sao Tome and Principe; Moldova = Republic of Moldova."
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.86, 0.12),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill = "white", colour = "grey80"),
        legend.key = element_rect(fill = "white", colour = NA),
        legend.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.text.y  = element_text(size = 10, hjust = 1),
        axis.text.x  = element_text(size = 11),
        plot.caption = element_text(size = 10, hjust = 0),
        panel.grid.minor.y = element_blank())
ggsave("out/prop-childless_sex.png",
       width = 8.27, height = 13, units = "in", dpi = 300, bg = "white")

# sex gap for the manuscript
sex_gap <- plot_df %>%
  filter(edu2 == "All") %>%
  select(country_plot, sex, p_childless) %>%
  distinct() %>%
  pivot_wider(
    names_from = sex,
    values_from = p_childless
  ) %>%
  mutate(
    gap = Men - Women,
    gap_abs = abs(gap)
  )

min_gap <- sex_gap %>% slice_min(gap_abs, n = 1, with_ties = TRUE)
max_gap <- sex_gap %>% slice_max(gap_abs, n = 1, with_ties = TRUE)

min_gap
max_gap

# top 2 countries of % of childless
top2_childless_by_sex <- plot_df %>%
  filter(edu2 == "All") %>%
  group_by(sex) %>%
  arrange(desc(p_childless), .by_group = TRUE) %>%
  slice_head(n = 2) %>%
  ungroup() %>%
  transmute(
    sex,
    country = country_plot,
    childless_pct = round(p_childless, 1)
  )

# ---- Figure 2
# x: GII, y: % of never-in-union among childless population
never %>% 
  filter(edu2 == "All") %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_wrap(~ sex) +
  geom_point(aes(group = country, colour = continent)) +
  geom_smooth(method = "gam", se = F, linewidth = 2, 
              colour = scales::alpha(col7[2], 0.7)) +
  scale_colour_manual(values = Mycol) +
  xlim(0, 0.8) +
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


# ---- Figure 3
# x: GII, y: % of never-in-union among childless population by education
y_max <- 80

outliers <- never_edu2 %>%
  filter(!is.na(p_niu), p_niu > y_max) %>%
  transmute(sex, edu2, country, p_niu = round(p_niu, 1)) %>%
  arrange(desc(p_niu))

never_edu2 %>%
  filter(sex %in% c("Men", "Women")) %>%
  mutate(sex = factor(sex, levels = c("Women", "Men")),
         edu2 = factor(edu2, levels = c("Low", "High"))) %>%
  ggplot(aes(x = gii, y = p_niu)) +
  facet_grid(sex ~ edu2) +
  geom_point(aes(colour = continent), size = 2) +
  geom_smooth(method = "gam", se = F,
              colour = scales::alpha(col7[2], 0.7),
              linewidth = 2) +
  scale_colour_manual(values = Mycol) +
  coord_cartesian(xlim = c(0, 0.8), ylim = c(0, y_max)) +
  labs(x = "Gender Inequality Index",
       y = "% never-formed-union among childless population") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11, face = 2),
        strip.text = element_text(size = 13),
        panel.spacing = unit(1.2, "lines"),
        plot.caption = element_text(size = 10, hjust = 0),
        plot.margin = margin(8, 14, 8, 10))
ggsave("out/gii_p-niu_edu.png", width = 9, height = 6.5, bg = "white")

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
  spread(key = edu2, value = p_niu) %>% 
  mutate(diff = Low / High)
write.csv(ratio_edu, "out/ratio_niu_byedu2.csv")

check <- never_edu2 %>%
  mutate(p_niu_prop = p_niu / 100)

fig3_test_f <- glm(
  p_niu_prop ~ edu2 * gii,
  data = check %>% filter(sex == "Women"),
  weights = n_childless,
  family = quasibinomial
)

fig3_test_m <- glm(
  p_niu_prop ~ edu2 * gii,
  data = check %>% filter(sex == "Men"),
  weights = n_childless,
  family = quasibinomial
)

summary(fig3_test_f)
summary(fig3_test_m)

## x: GII, y: ratio of % never-in-union
#never %>% 
#  select(country, sex, edu2, p_niu, continent, gii) %>% 
#  filter(edu2 == "All") %>% 
#  spread(key = sex, value = p_niu) %>% 
#  mutate(diff = Women / Men) %>% 
#  filter(Women > 0) %>% 
#  ggplot(aes(x = gii, y = diff)) +
#  geom_point(aes(group = country, colour = continent)) +
#  geom_smooth(method = "gam") +
#  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
#  xlim(0, 0.8) +
#  ylim(0, 3) +
#  labs(x = "Gender Inequality Index", y = "Ratio of Women / Men") +
#  theme_minimal() +
#  theme(legend.position = "bottom",
#        legend.text = element_text(size = 18),
#        legend.title = element_blank(),
#        axis.title = element_text(size = 18),
#        axis.text = element_text(size = 15, face = 2),
#        strip.text = element_text(size = 18),
#        panel.spacing = unit(2, "lines"))
#ggsave("out/gii_p-niu_genderdiff.png", width = 7.5, height = 5, bg = "white")

#ratio_edu %>% 
#  ggplot(aes(x = gii, y = diff)) +
#  facet_wrap(~ sex) + 
#  geom_point(aes(group = country, colour = continent)) +
#  geom_smooth(method = "gam") +
#  scale_colour_manual(values = c(Mycol[1:4], col7[6])) +
#  xlim(0, 0.8) +
#  labs(x = "Gender Inequality Index", y = "Ratio of Low / High") +
#  theme_minimal() +
#  theme(legend.position = "bottom",
#        legend.text = element_text(size = 18),
#        legend.title = element_blank(),
#        axis.title = element_text(size = 18),
#        axis.text = element_text(size = 15, face = 2),
#        strip.text = element_text(size = 18),
#        panel.spacing = unit(2, "lines"))
#ggsave("out/gii_p-niu_edudiff.png", width = 9, height = 7.5, bg = "white")
