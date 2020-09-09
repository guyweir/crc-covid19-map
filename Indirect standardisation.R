#indirect age standardised COVID-19 mortality rates
library(nomisr)
library(readxl)

#read in England and Wales COVID-19 mortality data by age band (manual sum in the sheet) covering March to June
COVID19_byage <- read_excel("referencetables.xlsx", sheet = "Table 2", range = "AH13:AI37")
COVID19_byage <- na.omit(COVID19_byage)
ages <- as.vector(COVID19_byage$C_AGE_NAME)

EandWpops <- nomis_get_data(id = "NM_2010_1", geography = "TYPE439", sex = "Total", date = "latest", measures = "20100")
EandWpops2 <- EandWpops %>% filter(C_AGE_NAME %in% ages)


EandWpops2 <- EandWpops2 %>% select(C_AGE_NAME, OBS_VALUE) %>%  group_by(C_AGE_NAME) %>%  summarise(Population = sum(OBS_VALUE))


#msoa_pops_allages <- nomis_get_data(id = "NM_2010_1", geography = "TYPE297", sex = "Total", date = "latest", measures = "20100", C_AGE_NAME %in% ages)
msoa_pops_allages2 <- msoa_pops_allages %>% filter(C_AGE_NAME %in% ages)

msoa_covid <- read_excel("referencetables1.xlsx", sheet = "Table 5", range = "A13:J7214")
msoa_covid <- msoa_covid %>% select(1,10) %>% rename(`GEOGRAPHY_CODE` = 1, `COVID-19` = 2)

#add in scotland data (to be standardised by the E&W age profiles)
scotcovid <- read_excel("covid-deaths-extra-tables-week-32.xlsx", sheet = "Table S8", range = "A3:F1283")
scotcovid <- na.omit(scotcovid)
scotcovid <- scotcovid %>% select(1,4,5) %>% rename(`GEOGRAPHY_CODE` = 1, `COVID-19` = 2)
scotpops <- scotcovid %>% select(GEOGRAPHY_CODE, `Population (2018 based)`) #split out the pop data for merging with E&W

scotcovid <- select(scotcovid,1,2)
msoa_covid <- rbind(msoa_covid,scotcovid) #E&W and scotland covid deaths by MSOA and intermediate zones

#merge

COVID19_byage <- merge(COVID19_byage,EandWpops2, by = "C_AGE_NAME")
COVID19_byage$`Age specific COVID19 mortality rate` <- (COVID19_byage$`COVID-19 March to May E&W`/COVID19_byage$Population)


####now calculate expected mortality

#add scot pops into the MSOA pops
#######################################################################################
######################## GOT TO GET SCOT POPS BY 5 YR AGE BANDS########################
#######################################################################################

library(httr)
library(jsonlite)
call <- "https://www.opendata.nhs.scot/api/3/action/datastore_search?resource_id=93df4c88-f74b-4630-abd8-459a19b12f47&q=2019&limit=10000"
#GETresult <- GET(call, type = "basic")
foo <- content(GETresult, "text")
foo_json <- fromJSON(foo, flatten = T)
scotpopsage <- foo_json[["result"]][["records"]]
scotpopsage <- filter(scotpopsage,Year == 2019 & Sex == "All")

#group into age bands
scotpopsage <- scotpopsage %>% mutate(`Age 0 - 4` = select(.,`Age0`:`Age4`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 5-9` = select(.,`Age5`:`Age9`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 10-14` = select(.,`Age10`:`Age14`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 15-19` = select(.,`Age15`:`Age19`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 20-24` = select(.,`Age20`:`Age24`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 25-29` = select(.,`Age25`:`Age29`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 30-34` = select(.,`Age30`:`Age34`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 35-39` = select(.,`Age35`:`Age39`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 40-44` = select(.,`Age40`:`Age44`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 45-49` = select(.,`Age45`:`Age49`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 50-54` = select(.,`Age50`:`Age54`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 55-59` = select(.,`Age55`:`Age59`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 60-64` = select(.,`Age60`:`Age64`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 65-69` = select(.,`Age65`:`Age69`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 70-74` = select(.,`Age70`:`Age74`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 75-79` = select(.,`Age75`:`Age79`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 80-84` = select(.,`Age80`:`Age84`) %>% apply(1, sum, na.rm=TRUE))
scotpopsage <- scotpopsage %>% mutate(`Aged 85+` = select(.,`Age85`:`Age90plus`) %>% apply(1, sum, na.rm=TRUE))

#scotpopsage <- scotpopsage %>% mutate(`SUMCHECK` = select(.,`Age 0 - 4`:`Age 85+`) %>% apply(1, sum, na.rm=TRUE))
  
#select reshape and rename variables to match the MSOA data.
scotpopsage <- scotpopsage %>%  select(IntZone, SexQF, `Age 0 - 4`:`Aged 85+`) %>% rename("GEOGRAPHY_CODE" = IntZone, "GEOGRAPHY_NAME" = SexQF)
scotpopsage <- pivot_longer(scotpopsage,cols = (3:20),names_to = "C_AGE_NAME", values_to = "OBS_VALUE" )

msoa_pops_allages2 <- msoa_pops_allages2 %>% select(GEOGRAPHY_CODE, GEOGRAPHY_NAME,C_AGE_NAME,OBS_VALUE)
#rbind the scottish data
msoa_pops_allages2 <- bind_rows(msoa_pops_allages2, scotpopsage)

#add in the expected rates
msoa_pops_allages2 <- merge(msoa_pops_allages2, COVID19_byage[,c(1,4)],by = "C_AGE_NAME" )


msoa_pops_allages2$expected <- msoa_pops_allages2$OBS_VALUE * msoa_pops_allages2$`Age specific COVID19 mortality rate`
#now add up all the ages
msoa_pops_total <- msoa_pops_allages2 %>% group_by(GEOGRAPHY_CODE) %>% summarise(expected = sum(expected))

msoa_covid <- merge(msoa_pops_total, msoa_covid, by = "GEOGRAPHY_CODE")

msoa_covid$SMR <- msoa_covid$`COVID-19`/msoa_covid$expected
#overall mortality E&W
sumdeaths <- sum(COVID19_byage$`COVID-19 March to May E&W`)
sumpop <- sum(EandWpops2$Population)
cruderate <- sumdeaths/sumpop

msoa_covid$`COVID-19 AMR` <-  (cruderate * msoa_covid$SMR)*100000

write.csv(msoa_covid,file = "msoa_IZ_covid_Indirect_results.csv", row.names = F )

# selected_data <- merge(selected_data,msoa_covid,by = "GEOGRAPHY_CODE")
# 
# saveRDS(selected_data,file = "selected_data_AMR.rds")

