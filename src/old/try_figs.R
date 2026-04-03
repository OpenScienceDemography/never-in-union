D_all_sel <- read.csv("out/old/D_all_minage35_edu2.csv")

names(hdi)[1] <- "country"

D_all_sel <- D_all_sel %>% 
  mutate(prop_childless = (ChildlessN / TotalN) * 100,
         prop_neverinunion = (NeverInUnionN / ChildlessN) * 100,
         country = ifelse(country == "Democratic Republic of the Congo", "DR. Congo", country)) %>% 
  left_join(hdi, by = "country")


## % of childless
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
  facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range), rev(ggplot_build(plt)$layout$panel_scales_y[[2]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

D_childless_all %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 36), sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        legend.position = "none")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(6, NA), y = c(7, NA), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6, colour = Mycol[3],
                   tag_pool = c("Women", NA))

tag_facet(p_sex, 
          x = c(22, NA), y = c(2, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 6, colour = Mycol[2],
          tag_pool = c("Men", NA))
ggsave("out/propchildless_35+.png", width = 7, height = 8, bg = "white")


## % of never in union
level_country <- D_all_sel %>% 
  filter(sex == "Women", bc_cate == "1960-1969", education == "All") %>% 
  arrange(prop_neverinunion)

D_neverunion_all <- D_all_sel %>% 
  filter(bc_cate == "1960-1969", education == "All", TotalN >= 100) %>% 
  mutate(country = factor(country, levels = level_country$country),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")))

# for making gray shaded lines
plt <- D_neverunion_all %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = sex, colour = sex)) +
  facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range), rev(ggplot_build(plt)$layout$panel_scales_y[[2]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = n():1) %>% 
  filter(country_num %in% seq(5, n(), 5))

D_neverunion_all %>% 
  mutate(level_childless = case_when(prop_childless <= 5 ~ "1",
                                     prop_childless > 5 & prop_childless <= 10 ~ "2",
                                     prop_childless > 10 & prop_childless < 20 ~ "3",
                                     prop_childless >= 20 ~ "4")) %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = sex, colour = sex, shape = level_childless)) +
  facet_grid(rows = vars(datasetname), scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(aes(size = level_childless)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 85), sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  scale_shape_manual(values = c(43, 18, 17, 16)) +
  scale_size_manual(values = c(5, 4, 3, 3)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        legend.position = "none")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(NA, 32), y = c(NA, 54), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6, colour = Mycol[2],
                   tag_pool = c(NA, "Men"))

p_sex <- tag_facet(p_sex, 
                   x = c(NA, 45), y = c(NA, 50), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6, colour = Mycol[3],
                   tag_pool = c(NA, "Women"))

p <- tag_facet(p_sex, 
               x = c(0, NA), y = c(12, NA), 
               vjust = -1, hjust = -0,
               open = "", close = "",
               size = 5, colour = "black",
               tag_pool = c("% of childless", NA))

p <- tag_facet(p, 
               x = c(10, NA), y = c(8, NA), 
               vjust = -1, hjust = -0,
               open = "", close = "",
               size = 5, colour = "black",
               tag_pool = c("<= 5%", NA))

p <- tag_facet(p, 
               x = c(0, NA), y = c(6, NA), 
               vjust = -1, hjust = -0,
               open = "", close = "",
               size = 5, colour = "black",
               tag_pool = c("5% <     <= 10%", NA))

p <- tag_facet(p, 
               x = c(0, NA), y = c(4, NA), 
               vjust = -1, hjust = -0,
               open = "", close = "",
               size = 5, colour = "black",
               tag_pool = c("10% <     <= 20%", NA))

tag_facet(p, 
          x = c(0, NA), y = c(2, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 5, colour = "black",
          tag_pool = c("20% <", NA))
ggsave("out/propneverunion_35+.png", width = 7, height = 8, bg = "white")

## % of childless by edu
level_country <- D_all_sel %>% 
  filter(sex == "Women", bc_cate == "1960-1969", education == "High") %>% 
  arrange(prop_childless)

D_childless_edu <- D_all_sel %>% 
  filter(bc_cate == "1960-1969", education %in% c("Low", "High")) %>% 
  mutate(country = factor(country, levels = level_country$country),
         education = factor(education, levels = c("Low", "Medium", "High")),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")),
         sex = factor(sex, levels = c("Women", "Men")))

# for making gray shaded lines
plt <- D_childless_edu %>% 
  ggplot(aes(x = prop_childless, y = country, group = sex, colour = sex)) +
  facet_grid(datasetname ~ sex, scales = "free", space = "free") +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range), rev(ggplot_build(plt)$layout$panel_scales_y[[2]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = 1:n()) %>% 
  filter(country_num %in% seq(5, n(), 5))

D_all_sel %>% 
  filter(bc_cate == "1960-1969", education %in% c("Low", "High")) %>% 
  mutate(prop_childless = (ChildlessN / TotalN) * 100,
         prop_neverinunion = (NeverInUnionN / ChildlessN) * 100,
         education = factor(education, levels = c("Low", "Medium", "High")),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")),
         sex = factor(sex, levels = c("Women", "Men")),
         country = factor(country, levels = level_country$country)) %>%
  ggplot(aes(x = prop_childless, y = country, group = education, colour = education)) +
  facet_grid(datasetname ~ sex, scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(size = 3) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 45), sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        legend.position = "none")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(NA, NA, 30, 30), y = c(NA, NA, -Inf, -Inf), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6,
                   tag_pool = c(NA, NA, "Women", "Men"))

p_edu1 <- tag_facet(p_sex, 
                    x = c(NA, NA, 15, NA), y = c(NA, NA, 25, NA), 
                    vjust = -1, hjust = -0,
                    open = "", close = "",
                    size = 5, colour = Mycol[2],
                    tag_pool = c(NA, NA, "Low educated", NA))

tag_facet(p_edu1, 
          x = c(NA, NA, 15, NA), y = c(NA, NA, 22, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 5, colour = Mycol[3],
          tag_pool = c(NA, NA, "High educated", NA))
ggsave("out/propchildless_35+_edu.png", width = 8, height = 10, bg = "white")


## % of never in union by edu
level_country <- D_all_sel %>% 
  filter(sex == "Women", bc_cate == "1960-1969", education == "High") %>% 
  arrange(prop_neverinunion)

D_neverunion_edu <- D_all_sel %>% 
  filter(bc_cate == "1960-1969", education %in% c("Low", "High")) %>% 
  mutate(country = factor(country, levels = level_country$country),
         education = factor(education, levels = c("Low", "Medium", "High")),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")),
         sex = factor(sex, levels = c("Women", "Men")))

# for making gray shaded lines
plt <- D_neverunion_edu %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = sex, colour = sex)) +
  facet_grid(datasetname ~ sex, scales = "free", space = "free") +
  geom_line(aes(group = country), colour = "grey")

country_order <- c(rev(ggplot_build(plt)$layout$panel_scales_y[[1]]$range$range), rev(ggplot_build(plt)$layout$panel_scales_y[[2]]$range$range))

country_num <- country_order %>% 
  as.data.frame %>% 
  rename(country = ".") %>% 
  mutate(country_num = 1:n()) %>% 
  filter(country_num %in% seq(5, n(), 5))

D_all_sel %>% 
  filter(bc_cate == "1960-1969", education %in% c("Low", "High")) %>% 
  mutate(education = factor(education, levels = c("Low", "Medium", "High")),
         datasetname = factor(datasetname, levels = c("GGS", "DHS")),
         sex = factor(sex, levels = c("Women", "Men")),
         country = factor(country, levels = level_country$country),
         level_childless = case_when(prop_childless <= 5 ~ "1",
                                     prop_childless > 5 & prop_childless <= 10 ~ "2",
                                     prop_childless > 10 & prop_childless < 20 ~ "3",
                                     prop_childless >= 20 ~ "4")) %>% 
  ggplot(aes(x = prop_neverinunion, y = country, group = education, colour = education, shape = level_childless)) +
  facet_grid(datasetname ~ sex, scales = "free", space = "free") +
  geom_hline(yintercept = country_num$country, size = 2, color = "#eaeaea") +
  geom_line(aes(group = country), colour = "grey") +
  geom_point(aes(size = level_childless)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 101), sec.axis = dup_axis()) +
  scale_colour_manual(values = c(Mycol[2], Mycol[3])) +
  scale_shape_manual(values = c(43, 18, 17, 16)) +
  scale_size_manual(values = c(5, 4, 3, 3)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_text(size = 8, face = 2),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.spacing.x = unit(2, "lines"),
        strip.text = element_blank(),
        legend.position = "none")
p <- last_plot()

p_sex <- tag_facet(p, 
                   x = c(NA, NA, 70, 80), y = c(NA, NA, -Inf, -Inf), 
                   vjust = -1, hjust = -0,
                   open = "", close = "",
                   size = 6,
                   tag_pool = c(NA, NA, "Women", "Men"))

p_edu1 <- tag_facet(p_sex, 
                    x = c(NA, NA, 55, NA), y = c(NA, NA, 60, NA), 
                    vjust = -1, hjust = -0,
                    open = "", close = "",
                    size = 5, colour = Mycol[2],
                    tag_pool = c(NA, NA, "Low educated", NA))

p_edu2 <- tag_facet(p_edu1, 
                    x = c(NA, NA, 0, NA), y = c(NA, NA, 28, NA), 
                    vjust = -1, hjust = -0,
                    open = "", close = "",
                    size = 5, colour = Mycol[3],
                    tag_pool = c(NA, NA, "High educated", NA))

p <- tag_facet(p_edu2, 
          x = c(0, NA, NA, NA), y = c(12, NA, NA, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 5, colour = "black",
          tag_pool = c("% of childless", NA, NA, NA))

p <- tag_facet(p, 
          x = c(10, NA, NA, NA), y = c(8, NA, NA, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 5, colour = "black",
          tag_pool = c("<= 10%", NA, NA, NA))

p <- tag_facet(p, 
               x = c(0, NA, NA, NA), y = c(4, NA, NA, NA), 
               vjust = -1, hjust = -0,
               open = "", close = "",
               size = 5, colour = "black",
               tag_pool = c("10% <     <= 20%", NA, NA, NA))

tag_facet(p, 
          x = c(0, NA, NA, NA), y = c(0, NA, NA, NA), 
          vjust = -1, hjust = -0,
          open = "", close = "",
          size = 5, colour = "black",
          tag_pool = c("20% <", NA, NA, NA))
ggsave("out/propneverunion_35+_edu.png", p_edu2, width = 8, height = 10, bg = "white")

##
D_all_sel %>% 
  mutate(prop_childless = round((ChildlessN / TotalN) * 100, 1),
         prop_neverinunion = round((NeverInUnionN / ChildlessN) * 100, 1),
         education = factor(education, levels = c("Low", "Medium", "High"))) %>%
  filter(bc_cate == "1960-1969", education %in% c("Low", "High")) %>% View()
