func_makedata <- function(oridata, minage){
  D_total <- oridata %>%
    filter(age >= minage) %>% 
    group_by(country, sex, bc_cate, education, datasetname) %>% 
    tally(name = "TotalN")
  
  D_childless <- oridata %>%
    filter(age >= minage) %>% 
    group_by(country, sex, bc_cate, education, datasetname) %>% 
    count(everbirth, name = "ChildlessN") %>% 
    filter(everbirth == 0) %>% 
    select(-everbirth)
  
  D_union <- oridata %>%
    filter(age >= minage, everbirth == 0) %>% 
    group_by(country, sex, bc_cate, education, datasetname) %>% 
    count(everunion, name = "NeverInUnionN") %>% 
    filter(everunion == 0) %>% 
    select(-everunion)
  
  D <- D_total %>% 
    left_join(D_childless, by = c("country", "sex", "bc_cate", "education", "datasetname")) %>% 
    left_join(D_union, by = c("country", "sex", "bc_cate", "education", "datasetname"))
  
  D_alledu <- D %>% 
    filter(education %in% c("Low", "Medium", "High")) %>% 
    group_by(country, sex, bc_cate, datasetname) %>% 
    summarise(TotalN = sum(TotalN, na.rm = T), ChildlessN = sum(ChildlessN, na.rm = T), 
              NeverInUnionN = sum(NeverInUnionN, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(education = "All")
  
  outcome <- D %>% 
    bind_rows(D_alledu)
  
  return(outcome)
  
}
