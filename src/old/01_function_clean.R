func_makedata <- function(oridata, minage){
  D_total <- oridata %>%
    filter(age >= minage) %>% 
    group_by(country, sex, bc_cate, education) %>% 
    tally(name = "TotalN")
  
  D_childless <- oridata %>%
    filter(age >= minage) %>% 
    group_by(country, sex, bc_cate, education) %>% 
    count(everbirth, name = "ChildlessN") %>% 
    filter(everbirth == 0) %>% 
    select(-everbirth)
  
  D_union <- oridata %>%
    filter(age >= minage, everbirth == 0) %>% 
    group_by(country, sex, bc_cate, education) %>% 
    count(everunion, name = "NeverInUnionN") %>% 
    filter(everunion == 0) %>% 
    select(-everunion)
  
  D <- D_total %>% 
    left_join(D_childless, by = c("country", "sex", "bc_cate", "education")) %>% 
    left_join(D_union, by = c("country", "sex", "bc_cate", "education"))
  
  D_alledu <- D %>% 
    filter(education %in% c("Low", "Medium", "High")) %>% 
    group_by(country, sex, bc_cate) %>% 
    summarise(TotalN = sum(TotalN, na.rm = T), 
              ChildlessN = sum(ChildlessN, na.rm = T), 
              NeverInUnionN = sum(NeverInUnionN, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(education = "All")
  
  outcome <- D %>% 
    bind_rows(D_alledu)
  
  return(outcome)
  
}

func_makedata2 <- function(oridata, minage){
  D_total <- oridata %>%
    filter(age >= minage) %>% 
    group_by(datasetname, country, sex, bc_cate, edu2) %>% 
    tally(name = "TotalN")
  
  D_childless <- oridata %>%
    filter(age >= minage) %>% 
    group_by(datasetname, country, sex, bc_cate, edu2) %>% 
    count(everbirth, name = "ChildlessN", wt = weight) %>% 
    filter(everbirth == 0) %>% 
    select(-everbirth)
  
  D_union <- oridata %>%
    filter(age >= minage, everbirth == 0) %>% 
    group_by(datasetname, country, sex, bc_cate, edu2) %>% 
    count(everunion, name = "NeverInUnionN", wt = weight) %>% 
    filter(everunion == 0) %>% 
    select(-everunion)
  
  D <- D_total %>% 
    left_join(D_childless, by = c("datasetname", "country", "sex", "bc_cate", "edu2")) %>% 
    left_join(D_union, by = c("datasetname", "country", "sex", "bc_cate", "edu2"))
  
  D_alledu <- D %>% 
    filter(edu2 %in% c("High", "Low")) %>% 
    group_by(datasetname, country, sex, bc_cate) %>% 
    summarise(TotalN = sum(TotalN, na.rm = T), 
              ChildlessN = sum(ChildlessN, na.rm = T), 
              NeverInUnionN = sum(NeverInUnionN, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(edu2 = "All")
  
  outcome <- D %>% 
    bind_rows(D_alledu)
  
  return(outcome)
  
}
