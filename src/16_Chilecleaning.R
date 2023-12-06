chile %>% 
  mutate(sex = ifelse(gender == 1, "Men", "Women"),
         bc_cate = case_when(cohort == 1910 ~ "1910-1919",
                             cohort == 1920 ~ "1920-1929",
                             cohort == 1930 ~ "1930-1939",
                             cohort == 1940 ~ "1940-1949",
                             cohort == 1950 ~ "1950-1959",
                             cohort == 1960 ~ "1960-1969",
                             cohort == 1970 ~ "1970-1979",
                             cohort == 1980 ~ "1980-1989",
                             cohort == 1990 ~ "1990-1999")) %>% 
  select(id = RESPID, country = country_name, sex = SEX, age = v012, bc_cate, birthyear = v010, education = EDU_3, 
         everbirth, everunion, dataset = COUNTRY, datasetname)