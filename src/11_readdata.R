# ESS
# downloaded on June 5 2022 except ESS 10
#ESS1 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS1e06_6.sav", 
#                  to.data.frame = T, use.value.labels = F)
#ESS2 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS2e03_6.sav", 
#                  to.data.frame = T, use.value.labels = F)
ESS3 <- read.spss("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/ESS/ESS3e03_7.sav", 
                  to.data.frame = T, use.value.labels = F)
#ESS4 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS4e04_5.sav", 
#                  to.data.frame = T, use.value.labels = F)
#ESS5 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS5e03_4.sav", 
#                  to.data.frame = T, use.value.labels = F)
#ESS6 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS6e02_4.sav", 
#                  to.data.frame = T, use.value.labels = F)
#ESS7 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS7e02_2.sav", 
#                  to.data.frame = T, use.value.labels = F)
#ESS8 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS8e02_2.sav", 
#                  to.data.frame = T, use.value.labels = F)
ESS9 <- read.spss("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/ESS/ESS9e03_2.sav", 
                  to.data.frame = T, use.value.labels = F)
#ESS10 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS10.sav", 
#                  to.data.frame = T, use.value.labels = F)

#IT2 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS2IT.sav", 
#                 to.data.frame = T, use.value.labels = F)
#AT4 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS4AT.sav", 
#                 to.data.frame = T, use.value.labels = F)
#LT4 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS4LT.sav", 
#                 to.data.frame = T, use.value.labels = F)
#AT5 <- read.spss("../../../../OneDrive - Syddansk Universitet/BigData/ESS/ESS5ATe1_1.sav", 
#                 to.data.frame = T, use.value.labels = F)

# GGS
#ggs_w1 <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/GGS/GGS_Wave1_V.4.4.dta", 
#                     nonint.factors = T, convert.factors = T)

# Harmonized Histories
#hh <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/GGS_Harmonized/HARMONIZED#-HISTORIES_ALL_GGSaccess.dta", 
#                     nonint.factors = T, convert.factors = T)

hh0 <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/GGS_Harmonized/HH/HARMONIZED-HISTORIES_ALL_GGSaccess.dta", 
                  nonint.factors = T, convert.factors = T)

hh1 <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/GGS_Harmonized/HH1/HARMONIZED-HISTORIES_I.dta", 
                  nonint.factors = T, convert.factors = T)

hh2 <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/GGS_Harmonized/HH2/HarmonizedHistoriesII_2023_07_10.dta", 
                  nonint.factors = T, convert.factors = T)

# DHS
dhs <- read.dta13("../../../Users/rymo/Dropbox (Personal)/Proj_Partnership/Research_UCP/Analysis/data/DHS/ucp_red_edu.dta", 
                  nonint.factors = T, convert.factors = T)

# Chile
chile <- read.dta13("../../../Users/rymo/OneDrive - Syddansk Universitet/BigData/Chile/Chile_2017.dta", 
                    nonint.factors = T, convert.factors = T)


# HDI
hdi <- read_excel("../../../Users/rymo/Dropbox (Personal)/Proj_Partnership/Research_UCP/Analysis/data/HDR21-22_Statistical_Annex_HDI_Table.xlsx", sheet = "Table 1",
                  skip = 4)[-c(1:3), c(2, 3)]
