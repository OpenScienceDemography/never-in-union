ESS3sel <- ESS3 %>% 
  select(cntry, agea, pspwght, pweight, evlvptn, gndr, yrbrn, edulvla, bthcld) %>% 
  mutate(dataset = "ESS3")

ESS9sel <- ESS9 %>% 
  select(cntry, agea, pspwght, pweight, evlvptn, gndr, yrbrn, edulvla = edulvlb, bthcld) %>% 
  mutate(dataset = "ESS9")

ESS39 <- ESS3sel %>% 
  bind_rows(ESS9sel) %>% 
  mutate(agea = as.numeric(as.character(agea)),
         sex = ifelse(gndr == 1, 1, 0),
         bc_cate = cut(yrbrn, seq(min(yrbrn, na.rm = T), max(yrbrn, na.rm = T), 5)),
         country = countrycode(cntry, origin = "genc2c", destination = "country.name"),
         # educational level
         edulvla2 = case_when(edulvla %in% c(0, 1, 113, 129) ~ 1,
                              edulvla %in% c(2, 212, 213, 221, 222, 223, 229) ~ 2,
                              edulvla %in% c(3, 311, 312, 313, 321, 322, 323) ~ 3,
                              edulvla %in% c(4, 412, 413, 421, 422, 423) ~ 4,
                              edulvla %in% c(5, 510, 520, 610, 620, 710, 720, 800) ~ 5,
                              edulvla %in% c(55, 555) ~ 0),
         edu_cate = case_when(edulvla2 == 0 ~ "uncategorised",
                              edulvla2 %in% c(1, 2) ~ "low",
                              edulvla2 %in% c(3, 4) ~ "secondary",
                              edulvla2 == 5 ~ "tertiary"),
         # weight
         anweight = pspwght * pweight)

ESS_total <- ESS39 %>%
  filter(agea >= 35) %>% 
  group_by(Country, sex, bc_cate, edu_cate) %>% 
  tally(name = "TotalN")

ESS_childless <- ESS39 %>%
  filter(agea >= 35) %>% 
  group_by(Country, sex, bc_cate, edu_cate) %>% 
  count(bthcld, name = "ChildlessN") %>% 
  filter(bthcld == 2) %>% 
  select(-bthcld)

ESS_union <- ESS39 %>%
  filter(agea >= 35, bthcld == 2) %>% 
  group_by(Country, sex, bc_cate, edu_cate) %>% 
  count(evlvptn, name = "NeverInUnionN") %>% 
  filter(evlvptn == 2) %>% 
  select(-evlvptn)

ESS_total %>% 
  left_join(ESS_childless, by = c("Country", "sex", "bc_cate", "edu_cate")) %>% 
  left_join(ESS_union, by = c("Country", "sex", "bc_cate", "edu_cate")) %>% View()
