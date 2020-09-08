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


msoa_pops_allages <- nomis_get_data(id = "NM_2010_1", geography = "TYPE297", sex = "Total", date = "latest", measures = "20100", C_AGE_NAME %in% ages)
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

msoa_pops_allages2 <- msoa_pops_allages2 %>% select(GEOGRAPHY_CODE, GEOGRAPHY_NAME,C_AGE_NAME,OBS_VALUE)

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

selected_data <- merge(selected_data,msoa_covid,by = "GEOGRAPHY_CODE")

saveRDS(selected_data,file = "selected_data_AMR.rds")

