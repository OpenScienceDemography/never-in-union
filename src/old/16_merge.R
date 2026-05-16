ESS39 <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/ESS39.rds")
d_hh <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/d_hh.rds")
d_dhs <- readRDS("../../../Library/CloudStorage/GoogleDrive-ryohei.mogi@upf.edu/My Drive/BigData/tempo/dhs_sel.rds")

d_all <- d_hh %>% 
  bind_rows(d_dhs) %>% 
  bind_rows(ESS39)

d_all_sel <- func_makedata2(oridata = d_all, minage = 35)
d_all_sel <- d_all_sel %>% 
  mutate(prop_childless = round((ChildlessN / TotalN) * 100, 1),
         prop_neverinunion = round((NeverInUnionN / ChildlessN) * 100, 1),
         edu2 = factor(edu2, levels = c("All", "Low", "High")))
write.csv(d_all_sel, "out/d_all_minage35_edu2_.csv")

d_all_sel %>% 
  filter(edu2 == "All") %>% View()


###
#try <- D_all %>% 
#  filter(country == "Albania", sex == "Women",
#         bc_cate %in% c("1965-1969", "1970-1974", "1975-1979"))
#table(try$bc_cate)
#table(try$bc_cate, try$everbirth)
#
#D_childless <- try %>%
#  filter(age >= 35) %>% 
#  group_by(country, sex, bc_cate, education, datasetname) %>% 
#  count(everbirth, name = "ChildlessN") %>% 
#  filter(everbirth == 0) %>% 
#  select(-everbirth)
#
#D_alledu <- D_childless %>% 
#  filter(education %in% c("Low", "Medium", "High")) %>% 
#  group_by(country, sex, bc_cate, datasetname) %>% 
#  summarise(ChildlessN = sum(ChildlessN, na.rm = T)) %>% 
#  ungroup() %>% 
#  mutate(education = "All")
