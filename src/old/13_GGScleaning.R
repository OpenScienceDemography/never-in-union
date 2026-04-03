#D_ggs_w1 <- ggs_w1 %>% 
#  select(arid, ayear, country = acountry, age = aage, asex, numbiol, aparstat, a333, a148, birthyear = abyear) %>% 
#  mutate(id = paste(arid, ayear, country, sep = "-"),
#         sex = ifelse(asex == "female", "Women", "Men"),
#         bc_cate = case_when(birthyear %in% 1910:1919 ~ "1910-1919",
#                             birthyear %in% 1920:1929 ~ "1920-1929",
#                             birthyear %in% 1930:1939 ~ "1930-1939",
#                             birthyear %in% 1940:1949 ~ "1940-1949",
#                             birthyear %in% 1950:1959 ~ "1950-1959",
#                             birthyear %in% 1960:1969 ~ "1960-1969",
#                             birthyear %in% 1970:1979 ~ "1970-1979",
#                             birthyear %in% 1980:1989 ~ "1980-1989",
#                             birthyear %in% 1990:1999 ~ "1990-1999",
#                             birthyear %in% 2000:2004 ~ "2000-2009"),
#         everbirth = ifelse(numbiol > 0, 1, 0),
#         everunion = ifelse(aparstat == "co-resident partner" | a333 == "yes", 1, 0),
#         education = case_when(a148 %in% c("isced 0 - pre-primary education", "isced 1 - primary level",
#                                     "isced 2 - lower secondary level", "has not studied in school, incl. illiterate",
#                                     "0 - isced97", "1-2 - isced97") ~ "Low",
#                         a148 %in% c("isced 3 - upper secondary level", "isced 4 - post secondary non-tertiary",
#                                     "3A - isced97", "3B - isced97", "3C - isced97") ~ "Medium",
#                         a148 %in% c("isced 5 - first stage of tertiary", "isced 6 - second stage of tertiary",
#                                     "5A-6 - isced97", "5B - isced97", "isced 5A-6", "isced 5A",
#                                     "isced 5A-5B") ~ "High"),
#         education = factor(education, levels = c("Low", "Medium", "High")),
#         dataset = "GGS1",
#         datasetname = "GGS") %>% 
#  select(id, country, sex, age, bc_cate, birthyear, education, everbirth, everunion, dataset, datasetname)
#saveRDS(D_ggs_w1, file = "../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/tempo/D_ggs_w1.rds")
