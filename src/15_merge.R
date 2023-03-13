D_all <- D_ggs_w1 %>% 
  bind_rows(dhs_sel)

D_all_sel <- func_makedata(oridata = D_all, minage = 35)
D_all_sel <- D_all_sel %>% 
  mutate(prop_childless = round((ChildlessN / TotalN) * 100, 1),
         prop_neverinunion = round((NeverInUnionN / ChildlessN) * 100, 1),
         education = factor(education, levels = c("All", "Low", "Medium", "High")))

D_all_sel %>% 
  filter(education == "All") %>% View()


write.csv(D_all_sel, "out/D_all_minage35_edu2.csv")

##
try <- D_all %>% 
  filter(country == "Albania", sex == "Women",
         bc_cate %in% c("1965-1969", "1970-1974", "1975-1979"))
table(try$bc_cate)
table(try$bc_cate, try$everbirth)

D_childless <- try %>%
  filter(age >= 35) %>% 
  group_by(country, sex, bc_cate, education, datasetname) %>% 
  count(everbirth, name = "ChildlessN") %>% 
  filter(everbirth == 0) %>% 
  select(-everbirth)

D_alledu <- D_childless %>% 
  filter(education %in% c("Low", "Medium", "High")) %>% 
  group_by(country, sex, bc_cate, datasetname) %>% 
  summarise(ChildlessN = sum(ChildlessN, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(education = "All")
