#Turn Wales IMD 2019 into MSOA level average ranks

library(tidyverse)
library(readxl)
library(nomisr)

WalesIMD19 <- read_excel("welsh-index-multiple-deprivation-2019-index-and-domain-ranks-by-small-area.xlsx", 
                         sheet = "WIMD_2019_ranks", range = "A3:D1912")


#add in lsoa populations for 2019
lsoa_pops <- nomis_get_data(id = "NM_2010_1", geography = "TYPE298", sex = "Total", date = "latest", measures = "20100", C_AGE = "0")
lsoa_pops <- lsoa_pops %>% select("GEOGRAPHY_CODE", "OBS_VALUE") %>% rename(Pop2018 = "OBS_VALUE")
#lsoa to msoa lookup
lookup <- read_csv("os lsoa msoa lookup.csv")
lookup <- lookup %>%select(MSOA11CD, LSOA11CD)
lookup2 <- lookup[!duplicated(lookup$LSOA11CD),]

#merge MSOA codes and pops to IMD
WalesIMD19 <- merge(WalesIMD19, lsoa_pops, by.x = "LSOA Code", by.y = "GEOGRAPHY_CODE", all.x = T)
WalesIMD19 <- merge(WalesIMD19, lookup2, by.x = "LSOA Code", by.y = "LSOA11CD")

#calculate population weighted MSOA IMD rank
wimd_msoa <- WalesIMD19 %>% group_by(MSOA11CD) %>% summarise_at(vars(starts_with("WIMD")),funs(weighted.mean(.,Pop2018)))
wimd_msoa <- wimd_msoa %>% mutate(`IMD MSOA Deciles` = ntile(`WIMD 2019`,10))
wimd_msoa$`IMD MSOA Deciles` <- factor(wimd_msoa$`IMD MSOA Deciles`, labels = c("1st most deprived", "2", "3", "4", "5", "6", "7", "8", "9", "10 least deprived"))

write.csv(wimd_msoa, file = "wimd19MSOA.csv", row.names = F)
