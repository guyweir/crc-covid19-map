#Turn Scottish IMD 2020 into MSOA level average ranks

library(tidyverse)
library(readxl)
library(nomisr)

ScotIMD20 <- read_excel("SIMD+2020v2+-+ranks.xlsx", 
                         sheet = "SIMD 2020v2 ranks")


#add in lsoa populations for 2019 https://www.opendata.nhs.scot/tl/dataset/population-estimates/resource/c505f490-c201-44bd-abd1-1bd7a64285ee
library(httr)
library(jsonlite)
call <- "https://www.opendata.nhs.scot/tl/api/3/action/datastore_search?resource_id=c505f490-c201-44bd-abd1-1bd7a64285ee&q=2018&limit=30000"
GETresult <- GET(call, type = "basic")
foo <- content(GETresult, "text")
foo_json <- fromJSON(foo, flatten = T)
scotpops2018 <- foo_json[["result"]][["records"]]
scotpops2018 <- scotpopsage %>% filter(Sex == "All") %>% select(DataZone, AllAges) %>% rename(Population2018 = AllAges)


#lsoa to msoa lookup
lookup <- read_csv("Datazone2011lookup.csv")
lookup <- lookup %>%select(DZ2011_Code, IZ2011_Code)
lookup2 <- lookup[!duplicated(lookup$DZ2011_Code),]



#merge MSOA codes and pops to IMD
ScotIMD20 <- merge(ScotIMD20, scotpops2018, by.x = "Data_Zone", by.y = "DataZone", all.x = T)
ScotIMD20 <- merge(ScotIMD20, lookup2, by.x = "Data_Zone", by.y = "DZ2011_Code")

#calculate population weighted MSOA IMD rank
simd_iz <- ScotIMD20 %>% group_by(IZ2011_Code) %>% summarise_at(vars(starts_with("SIMD2020v2_Rank")),funs(weighted.mean(.,Total_population)))
simd_iz <- simd_iz %>% mutate(`IMD IZ Deciles` = ntile(`SIMD2020v2_Rank`,10))
simd_iz$`IMD IZ Deciles` <- factor(simd_iz$`IMD IZ Deciles`, labels = c("1st most deprived", "2", "3", "4", "5", "6", "7", "8", "9", "10 least deprived"))

write.csv(simd_iz, file = "simd20IZ.csv", row.names = F)
