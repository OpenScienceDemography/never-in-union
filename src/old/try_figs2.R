D_all_sel <- read.csv("out/D_all_minage35_edu2.csv")

D_all_sel <- D_all_sel %>% 
  mutate(prop_childless = (ChildlessN / TotalN) * 100,
         prop_neverinunion = (NeverInUnionN / ChildlessN) * 100,
         country = case_when(country == "Democratic Republic of the Congo" ~ "DR.Congo",
                             country == "CzechRepublic" ~ "Czechia",
                             country == "Republic of Moldova" ~ "Moldova",
                             T ~ country)) %>% 
  left_join(hdi, by = "country")

## sorted country by HDI
level_country <- D_all_sel %>% 
  filter(sex == "Women", bc_cate == "1960-1969", education == "All") %>% 
  arrange(hdi)

D_childless_all <- D_all_sel %>% 
  filter(bc_cate == "1960-1969", education == "All", TotalN >= 100) %>% 
  mutate(country = factor(country, levels = level_country$country),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")))

# for making gray shaded lines
plt <- D_childless_all %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range), rev(ggplot_build(plt)$layout$panel_scales_y[[2]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

#D_childless_all %>% 
#  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
#  #facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
#  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
#  geom_line(aes(group = country), colour = "grey") +
#  geom_point(size = 3) +
#  geom_text(aes(x = 23, y = 65), label = "Men", size = 6, col = Mycol[2]) +
#  geom_text(aes(x = 10, y = 77), label = "Women", size = 6, col = Mycol[3]) +
#  scale_x_continuous(expand = c(0, 0), limits = c(0, 36), sec.axis = dup_axis()) +
#  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
#  theme_minimal() +
#  theme(axis.title = element_blank(),
#        axis.text = element_text(size = 8, face = 2),
#        panel.grid.major.y = element_blank(),
#        panel.grid.minor.y = element_blank(),
#        panel.spacing.x = unit(2, "lines"),
#        strip.text = element_blank(),
#        legend.position = "none")
#ggsave("out/propchildless_35+_hdi.png", width = 7, height = 8, bg = "white")


D_childless_all %>% 
  select(country, sex, prop_childless, prop_neverinunion) %>% 
  gather(key = index, value = prop, c(prop_childless, prop_neverinunion)) %>% 
  ggplot(aes(x = prop, y = country, group = sex, colour = sex)) +
  facet_grid(cols = vars(index), scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 3) +
  scale_x_continuous(sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_blank(),
        legend.position = "none") +
  labs(subtitle = "% of childless                           % of never-in-union among childless population")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(15, NA), y = c(20, NA), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6, colour = Mycol[3],
                   tag_pool = c("Women", NA))

tag_facet(p_sex, 
          x = c(22, NA), y = c(63, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 6, colour = Mycol[2],
          tag_pool = c("Men", NA))
ggsave("out/prop35+_hdi.png", width = 7, height = 8, bg = "white")

## sorted country by % of childless pop
level_country <- D_all_sel %>% 
  filter(sex == "Women", bc_cate == "1960-1969", education == "All") %>% 
  arrange(prop_childless)

D_childless_all <- D_all_sel %>% 
  filter(bc_cate == "1960-1969", education == "All", TotalN >= 100) %>% 
  mutate(country = factor(country, levels = level_country$country),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")))

# for making gray shaded lines
plt <- D_childless_all %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))


D_childless_all %>% 
  select(country, sex, prop_childless, prop_neverinunion) %>% 
  gather(key = index, value = prop, c(prop_childless, prop_neverinunion)) %>% 
  ggplot(aes(x = prop, y = country, group = sex, colour = sex)) +
  facet_grid(cols = vars(index), scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 3) +
  scale_x_continuous(sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(1, "lines"),
        strip.text = element_blank(),
        legend.position = "none") +
  labs(subtitle = "% of childless                           % of never-in-union among childless population")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(10, NA), y = c(39, NA), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6, colour = Mycol[3],
                   tag_pool = c("Women", NA))

tag_facet(p_sex, 
          x = c(22, NA), y = c(54, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 6, colour = Mycol[2],
          tag_pool = c("Men", NA))
ggsave("out/prop35+_%ofchildlss.png", width = 7, height = 8, bg = "white")
