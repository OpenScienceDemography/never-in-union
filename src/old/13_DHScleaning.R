#dhs_sel <- dhs %>% 
#  rename(id = caseid, country = country_name, birthyear = v010, age = v012) %>% 
#  group_by(country) %>% 
#  # select only the recent survey year
#  filter(syear == max(syear)) %>% 
#  mutate(sex = ifelse(gender == 1, "Women", "Men"),
#         bc_cate = case_when(birthyear %in% 1910:1914 ~ "1910-1914",
#                             birthyear %in% 1915:1919 ~ "1915-1919",
#                             birthyear %in% 1920:1924 ~ "1920-1924",
#                             birthyear %in% 1925:1929 ~ "1925-1929",
#                             birthyear %in% 1930:1934 ~ "1930-1934",
#                             birthyear %in% 1935:1939 ~ "1935-1939",
#                             birthyear %in% 1940:1944 ~ "1940-1944",
#                             birthyear %in% 1945:1949 ~ "1945-1949",
#                             birthyear %in% 1950:1954 ~ "1950-1954",
#                             birthyear %in% 1955:1959 ~ "1955-1959",
#                             birthyear %in% 1960:1964 ~ "1960-1964",
#                             birthyear %in% 1965:1969 ~ "1965-1969",
#                             birthyear %in% 1970:1974 ~ "1970-1974",
#                             birthyear %in% 1975:1979 ~ "1975-1979",
#                             birthyear %in% 1980:1984 ~ "1980-1984",
#                             birthyear %in% 1985:1989 ~ "1985-1989",
#                             birthyear %in% 1990:1994 ~ "1990-1994",
#                             birthyear %in% 1995:1999 ~ "1995-1999",
#                             birthyear %in% 2000:2004 ~ "2000-2004"),
#         education = case_when(education == 0 ~ "Low",
#                               education == 1 ~ "Medium",
#                               education == 2 ~ "High"),
#         education = factor(education, levels = c("Low", "Medium", "High")),
#         dataset = paste0("DHS", country_survey),
#         datasetname = "DHS") %>% 
#  select(id, country, sex, age, bc_cate, birthyear, education, everbirth, everunion, dataset, datasetname, weight = weigr)

## edu 2 category
d_dhs <- dhs %>% 
  select(-education) %>% 
  rename(id = caseid, country = country_name, birthyear = v010, age = v012, education = pc50, bc_cate = cohort) %>% 
  group_by(country) %>% 
  # select only the recent survey year
  filter(syear == max(syear)) %>% 
  mutate(sex = ifelse(gender == 1, "Women", "Men"),
         bc_cate = case_when(birthyear %in% 1910:1919 ~ "1910-1919",
                             birthyear %in% 1920:1929 ~ "1920-1929",
                             birthyear %in% 1930:1939 ~ "1930-1939",
                             birthyear %in% 1940:1949 ~ "1940-1949",
                             birthyear %in% 1950:1959 ~ "1950-1959",
                             birthyear %in% 1960:1969 ~ "1960-1969",
                             birthyear %in% 1970:1979 ~ "1970-1979",
                             birthyear %in% 1980:1989 ~ "1980-1989",
                             birthyear %in% 1990:1999 ~ "1990-1999",
                             birthyear %in% 2000:2004 ~ "2000-2009"),
         education = ifelse(education == 1, "High", "Low"),
         education = factor(education, levels = c("Low", "High")),
         dataset = paste0("DHS", country_survey),
         datasetname = "DHS") %>% 
  group_by(country, sex, bc_cate) %>% 
  mutate(mean_eduy = mean(yearss, na.rm = T),
         edu2 = ifelse(yearss >= mean_eduy, "High", "Low")) %>% 
  ungroup() %>% 
  select(id, country, sex, age, bc_cate, birthyear, education, everbirth, everunion, dataset, datasetname, weight = weigr, edu2)
saveRDS(d_dhs, file = "../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/dhs_sel.rds")
