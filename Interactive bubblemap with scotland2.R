#######################
##### LOAD DATA #######
#######################
library(tidyverse)
library(reactable)
library(crosstalk)

selected_data <- readRDS("selected_data_AMR.rds") #AMRs pre-calculated in "Indirect standardisation.R" original data used in LPP covid regression
AMRs <- read.csv("msoa_IZ_covid_Indirect_results.csv", stringsAsFactors = F) #new data covering March to July including scotland
selected_data$Laname <- gsub('[0-9]+', '', selected_data$GEOGRAPHY_NAME)#add lanames
selected_data$Laname <- trimws(selected_data$Laname, "r")
selected_data <- selected_data[,c(1,6,85,42,46,48)]

#create nice new dataset with the variables from original data and the new COVID mortality with SMR AMRs etc.
selected_data <- merge(selected_data, AMRs, by = "GEOGRAPHY_CODE", all.y = T)
selected_data$scotflag <- NA
selected_data$scotflag[str_detect(selected_data$GEOGRAPHY_CODE,"S0")] <- "Scotland"
selected_data$scotflag[str_detect(selected_data$GEOGRAPHY_CODE,"E0|W0")] <- "England&Wales"


selected_data <-  selected_data  %>% group_by(scotflag) %>% mutate(`IMD quintiles` = ntile(`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`, 5))

#######################
##### add welsh IMD average MSOA ranks. #######
#######################

wimd <- read.csv("wimd19MSOA.csv", stringsAsFactors = F)

#filter Wales only from selected data and remove IMD vars
W_selected_data <- selected_data %>% filter(`Region: `== "(pseudo) Wales") %>% 
  select(-`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`,
         -`IMD MSOA Deciles`)


#add in Wales IMD data
W_selected_data <- merge(W_selected_data, wimd, by.x = "GEOGRAPHY_CODE", by.y = "MSOA11CD")
W_selected_data <- W_selected_data %>% rename("Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x" = WIMD.2019,
                                              "IMD MSOA Deciles" = IMD.MSOA.Deciles)
#remove old Wales
selected_data <- selected_data %>% filter(!str_detect(GEOGRAPHY_CODE, "W0"))
selected_data <- bind_rows(selected_data, W_selected_data)

selected_data$`IMD MSOA Deciles` <- factor(selected_data$`IMD MSOA Deciles`, 
                                           levels = c("1st most deprived", "2","3","4","5","6","7","8","9","10 least deprived" )) #make sure IMD deciles loaded as factor in the correct order
rm(W_selected_data, wimd)

#######################
##### add Scottish IMD average MSOA ranks. #######
#######################


simd <- read.csv("simd20IZ.csv", stringsAsFactors = F)

#####AND MERGE THEM!#####

#filter Wales only from selected data and remove IMD vars
S_selected_data <- selected_data %>% filter(str_detect(GEOGRAPHY_CODE, "S0")) %>% 
  select(-`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`,
         -`IMD MSOA Deciles`, -`House of Commons Library MSOA Names`, -Laname)


#add in Scotland IMD data
S_selected_data <- merge(S_selected_data, simd, by.x = "GEOGRAPHY_CODE", by.y = "IZ2011_Code")
S_selected_data <- S_selected_data %>% rename("Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x" = SIMD2020v2_Rank,
                                              "IMD MSOA Deciles" = IMD.IZ.Deciles)

#add msoa and LA place names
msoanames <- read_csv("MSOA-Names-v1.1.0.csv")
#keep just the LA and msoa name columns
msoanames <- msoanames %>% select(msoa11cd, msoa11hclnm, Laname)
#add scotland intermediate zones
#lsoa to msoa lookup
izcodes <- read_csv("Datazone2011lookup.csv")
izcodes <- izcodes %>% select(IZ2011_Code, IZ2011_Name,LA_Name) %>% rename("Laname" = LA_Name)
izcodes <- izcodes[!duplicated(izcodes$IZ2011_Code),]
izcodes <- izcodes %>% rename("msoa11cd" =IZ2011_Code, "msoa11hclnm" = IZ2011_Name)
msoanames <- bind_rows(msoanames, izcodes)
msoanames <- msoanames %>% rename(`House of Commons Library MSOA Names` = "msoa11hclnm")

#merge 'em
S_selected_data <- merge(S_selected_data, msoanames, by.x = "GEOGRAPHY_CODE", by.y = "msoa11cd")

#remove old Scotland
selected_data <- selected_data %>% filter(str_detect(GEOGRAPHY_CODE, "S0", negate = T))
selected_data <- bind_rows(selected_data, S_selected_data)
selected_data <- select(selected_data, -`Region: `) #remove region as Scotland NA.

selected_data$`IMD MSOA Deciles` <- factor(selected_data$`IMD MSOA Deciles`, 
                                           levels = c("1st most deprived", "2","3","4","5","6","7","8","9","10 least deprived" )) #make sure IMD deciles loaded as factor in the correct order

# make sure all the variable names match in the crosstalk data
selected_data <- selected_data %>% rename("Neighbourhood name" = `House of Commons Library MSOA Names`,
                                          "Local Authority" = Laname, 
                                          "IMD MSOA Deciles" = `IMD MSOA Deciles`, "COVID-19 deaths unadjusted" = `COVID.19`,
                                          "COVID-19 deaths per 100,000" = covid_per100k_pop,
                                          "COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`)

rm(S_selected_data, msoanames, izcodes, AMRs,simd)

#Make table for the widget
table1 <- selected_data  %>% ungroup() %>% mutate_at(.vars = "IMD MSOA Deciles", .funs = gsub, pattern = "1st ", replacement = "1 ") %>%
  select("Neighbourhood name",
         "Local Authority",
         "IMD MSOA Deciles" , 
         "COVID-19 deaths unadjusted" ,
         "COVID-19 deaths per 100,000" ,
         "COVID-19 Adjusted Mortality Rate per 100,000" ) %>%
  mutate_if(is.numeric,round,0)


table2 <- SharedData$new(table1, group = "1") #make it a shared object for the widget.
#, table1$`Neighbourhood name`


##########the reactable table

tbl <- reactable(table2, selection = "multiple",
                 onClick = "select",
                 rowStyle = list(cursor = "pointer"),
                 minRows = 10,filterable = F,searchable = F, wrap = T , defaultPageSize = 15, striped = T, highlight = T,
                 defaultSorted = list("COVID-19 Adjusted Mortality Rate per 100,000" = "desc"),
                 columns = list(`IMD MSOA Deciles` = colDef(filterable = T),
                                `Local Authority` = colDef(filterable = T),
                                `Neighbourhood name` = colDef(filterable = T),
                                `COVID-19 deaths unadjusted` = colDef(aggregate = "sum",format = colFormat(digits = 0)),
                                `COVID-19 Adjusted Mortality Rate per 100,000` = colDef(aggregate = "sum",format = colFormat(digits = 0))),
                 #`COVID-19 deaths per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0)),
                 #`COVID-19 deaths age adjusted per 100,000` = colDef(aggregate = "mean",format = colFormat(digits = 0))),
                 theme = reactableTheme(
                   stripedColor = "#faf8f1",
                   highlightColor = "#e5dec4",
                   cellPadding = "6px 10px",
                   style = list(fontFamily = "Arial", fontSize = "12px"),
                   #searchInputStyle = list(width = "100%", fontWeight = "400"),
                   headerStyle = list(color = "white",background = "#2A2A2A",
                                      "&:hover[aria-sort]" = list(background = "#8c8c8c "),
                                      "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "#8c8c8c"),
                                      borderColor = "#555"
                   )
                 )) 
tbl


###############################################################
################################ ADD MAP ######################
###############################################################

suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))
library(spdplyr)
library(maptools)
library(rgdal)
library(rgeos)

#function for adding circle sizes to the legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-12,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}




msoa.centroids <- geojson_sf("https://opendata.arcgis.com/datasets/b0a6d8a3dc5d4718b3fd62c548d60f81_0.geojson")
#add in the IZ centroids
#get the centroids
izcentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/8/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")

izcentroids <- izcentroids %>% rename(objectid = OBJECTID, msoa11cd = InterZone, msoa11nm = Name)


izcentroids <- izcentroids[,c(1,2,3,9)]
izcentroids <- as(izcentroids, "Spatial")
izcentroids <- st_as_sf(izcentroids)


#EWS.centroids <- raster::union(lsoa.centroids, dzcentroids)
# dzcentroids2 <- as_tibble(dzcentroids)
# lsoa.centroids2 <- as_tibble(lsoa.centroids)

EWS.centroids <- bind_rows(izcentroids, msoa.centroids)
rm(izcentroids, msoa.centroids)



#merge in the cc data
#EWS.centroids <- st_as_sf(EWS.centroids)
EWS.centroids.df <- merge(selected_data,EWS.centroids,by.x = "GEOGRAPHY_CODE", by.y = "msoa11cd", all.x = T)
EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.df$`COVID-19sq_per100k_pop` <- sqrt(EWS.centroids.df$`COVID-19 deaths per 100,000`)*0.3 #create square root metrics for circle radius's
EWS.centroids.df$`COVID-19sq_ageadjusted_per100k_pop` <- sqrt(as.numeric(EWS.centroids.df$`COVID-19 Adjusted Mortality Rate per 100,000`))*0.3


factpal <- colorFactor("RdBu",levels = levels(EWS.centroids.df$`IMD MSOA Deciles`), 
                       ordered = TRUE, reverse = T )


labels <- sprintf("<strong>%s</strong><br/>%g ppt change<sup></sup>",
                  EWS.centroids.df$`Neighbourhood name`, round(EWS.centroids.df$`COVID-19 Adjusted Mortality Rate per 100,000`,1)) %>% 
  lapply(htmltools::HTML)





#function for adding circle sizes to the legend
addLegendCustom <- function(map, colors, labels, sizes, opacity = 0.5, position){
  
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:",
                           sizes, "px", "; position: relative; left: ",max(sizes)-(sizes/2)-12,"px")
  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", 
                           sizes,";position:relative; left: ",max(sizes)-(sizes),"px","; bottom: ",
                           10,"px",";margin-top: 12px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity, position = position))
}

#add title to page
library(htmltools)

#page element title
title <- tags$div(HTML("COVID-19 deaths per 100,000 and deprivation,<br> March to May 2020, England and Wales</br>"), 
                  style = "font-family: Open Sans;color: #2A2A2A;font-weight: bold; font-size: 22px; text-align: center"
)

#page element data sources
sources <- tags$div(HTML("Sources: Indices of Multiple Deprivation, MHCLG; Welsh Index of Multiple Deprivation 2019; Deaths involving COVID-19 by local area and deprivation, ONS<br> Analysis: WPI Economics on behalf of CRC, originally produced for Trust for London<br>Note: Deprivation ranks are relative to England and Wales separately"), 
                    style = "font-family: Open Sans;color: #2A2A2A;font-style: italic; font-size: 12px; text-align: left"
)

#remove NA
#EWS.centroids.df <- EWS.centroids.df[!is.na(EWS.centroids.df$`Change decile (1 = low)`),]
#EWS.centroids.df <- st_as_sf(EWS.centroids.df)

EWS.centroids.dfXT <- SharedData$new(EWS.centroids.df, group = "1") 

#map element
m2 <- leaflet(EWS.centroids.dfXT, height = "580px", options = list(padding = 100)) %>% setView(-3.5,53.2, 5.5) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  
  
  addCircleMarkers( group = "circlegw",
                    radius = ~`COVID-19sq_ageadjusted_per100k_pop`,
                    stroke = F,
                    color = ~factpal(`IMD MSOA Deciles`), opacity = 0.85, fillOpacity = 0.85,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto"))   %>% 
  
  addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("-3ppts","+5ppts","+15ppts"),
                  
                  sizes = c(1.4,3,5)*2, position = "bottomright" ) %>% 
  
  addLegend(pal = factpal, values = EWS.centroids.df$`IMD MSOA Deciles`, 
            labels = levels(EWS.centroids.df$`IMD MSOA Deciles`), position = "bottomright", title = "Deciles <br>(1 = low)") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2





combo <- htmltools::tagList(m2, tbl,sources) #I think this makes a combined html object
#browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index.html", background = "#FFFCF1") #this saves it as an HTML page in the default folder.

