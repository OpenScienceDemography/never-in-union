ESS3sel <- ESS3 %>% 
  select(idno, cntry, agea, pspwght, pweight, evlvptn, gndr, yrbrn, edulvla, bthcld) %>% 
  mutate(dataset = "ESS3")

ESS9sel <- ESS9 %>% 
  select(idno, cntry, agea, pspwght, pweight, evlvptn, gndr, yrbrn, edulvla = edulvlb, bthcld) %>% 
  mutate(dataset = "ESS9")

ESS39 <- ESS3sel %>% 
  bind_rows(ESS9sel) %>% 
  mutate(agea = as.numeric(as.character(agea)),
         sex = ifelse(gndr == 1, "Men", "Women"),
         bc_cate = case_when(yrbrn %in% 1905:1909 ~ "1905-1909",
                             yrbrn %in% 1910:1919 ~ "1910-1919",
                             yrbrn %in% 1920:1929 ~ "1920-1929",
                             yrbrn %in% 1930:1939 ~ "1930-1939",
                             yrbrn %in% 1940:1949 ~ "1940-1949",
                             yrbrn %in% 1950:1959 ~ "1950-1959",
                             yrbrn %in% 1960:1969 ~ "1960-1969",
                             yrbrn %in% 1970:1979 ~ "1970-1979",
                             yrbrn %in% 1980:1989 ~ "1980-1989",
                             yrbrn %in% 1990:1999 ~ "1990-1999",
                             yrbrn %in% 1910:1919 ~ "2000-2009"),
         country = countrycode(cntry, origin = "genc2c", destination = "country.name"),
         country = ifelse(country == "United Kingdom", "The UK", country),
         everbirth = ifelse(bthcld == 1, 1, 0),
         everunion = ifelse(evlvptn == 1, 1, 0),
         # educational level
         edulvla2 = case_when(edulvla %in% c(0, 1, 113, 129) ~ 1,
                              edulvla %in% c(2, 212, 213, 221, 222, 223, 229) ~ 2,
                              edulvla %in% c(3, 311, 312, 313, 321, 322, 323) ~ 3,
                              edulvla %in% c(4, 412, 413, 421, 422, 423) ~ 4,
                              edulvla %in% c(5, 510, 520, 610, 620, 710, 720, 800) ~ 5,
                              edulvla %in% c(55, 555) ~ 0),
         edu_cate = case_when(edulvla2 %in% c(0, 1, 2) ~ "Low",
                              edulvla2 %in% c(3, 4) ~ "Medium",
                              edulvla2 == 5 ~ "High"),
         # weight
         anweight = pspwght * pweight,
         datasetname = "ESS",
         id = paste(idno, idno, "-")) %>% 
select(id, country, sex, age = agea, bc_cate, birthyear = yrbrn, education = edu_cate, 
       everbirth, everunion, dataset, datasetname, weight = anweight)
saveRDS(ESS39, file = "../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/tempo/ESS39.rds")

##
ESS_total <- ESS39 %>%
  filter(agea >= 35) %>% 
  group_by(country, sex, bc_cate, edu_cate) %>% 
  tally(name = "TotalN")

ESS_childless <- ESS39 %>%
  filter(agea >= 35) %>% 
  group_by(country, sex, bc_cate, edu_cate) %>% 
  count(bthcld, name = "ChildlessN") %>% 
  filter(bthcld == 2) %>% 
  select(-bthcld)

ESS_union <- ESS39 %>%
  filter(agea >= 35, bthcld == 2) %>% 
  group_by(country, sex, bc_cate, edu_cate) %>% 
  count(evlvptn, name = "NeverInUnionN") %>% 
  filter(evlvptn == 2) %>% 
  select(-evlvptn)

ESS_total <- ESS_total %>% 
  left_join(ESS_childless, by = c("country", "sex", "bc_cate", "edu_cate")) %>% 
  left_join(ESS_union, by = c("country", "sex", "bc_cate", "edu_cate"))
saveRDS(ESS_total, file = "../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/tempo/ESS_total.rds")

