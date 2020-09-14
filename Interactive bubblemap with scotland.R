#### Creates an HTML page with bubble map and MSOA table selectable boxes for the map.

#'NOTE ON DATA COVERAGE
#'Scotland produce mortality data by intermediate zones (equivalent to MSOAs) over the same time period as ONS do for E&W - 01 March to 31st July
#'NI produce Local Government District which look much larger than MSOAs so NOT extending to NI

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

#######################
##### add Scottish IMD average MSOA ranks. #######
#######################


simd <- read.csv("simd20IZ.csv", stringsAsFactors = F)

#####AND MERGE THEM!#####

#filter Wales only from selected data and remove IMD vars
S_selected_data <- selected_data %>% filter(str_detect(GEOGRAPHY_CODE, "S0")) %>% 
  select(-`Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`,
         -`IMD MSOA Deciles`, -`House of Commons Library MSOA Names`, -Laname)


#add in Wales IMD data
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



#


#final table selected cols and bit of a tidy up.
# table1 <- selected_data  %>% mutate_at(.vars = "IMD MSOA Deciles", .funs = gsub, pattern = "1st ", replacement = "1 ") %>% 
#   select(`House of Commons Library MSOA Names`, 
#          Laname, 
#          `IMD MSOA Deciles`,
#          `COVID.19`, 
#          covid_per100k_pop, 
#          `COVID.19.AMR`) %>% 
#   rename("Neighbourhood name" = `House of Commons Library MSOA Names`,
#          "Local Authority" = Laname, 
#          "IMD MSOA Deciles" = `IMD MSOA Deciles`, "COVID-19 deaths unadjusted" = `COVID.19`,
#          "COVID-19 deaths per 100,000" = covid_per100k_pop,
#          "COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`) %>% 
#   mutate_if(is.numeric,round,0) 

table1 <- selected_data  %>% mutate_at(.vars = "IMD MSOA Deciles", .funs = gsub, pattern = "1st ", replacement = "1 ") %>%
  select("Neighbourhood name",
         "Local Authority",
         "IMD MSOA Deciles" , "COVID-19 deaths unadjusted" ,
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
#cleanup
#rm(S_selected_data, AMRs, simd, wimd, W_selected_data, table1, table2, izcodes, msoanames)

###### ADD MAP #########
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))
library(spdplyr)
library(maptools)

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


#selected_data <- readRDS("selected_data.rds")
# selected_data <- selected_data %>% select(GEOGRAPHY_CODE, `House of Commons Library MSOA Names`,`Region: `,
#                                           `IMD MSOA Deciles`, covid_per100k_pop,
#                                           `COVID.19`,`COVID.19.AMR`,
#                                           `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`) %>% 
#   rename("COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`)

#selected_data <- selected_data %>% rename("COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`)

##########merge scotland IZs into msoas

#merge data and MSOA/iz boundaries

#MSOA/IZ boundaries
msoas <- geojson_sp("https://opendata.arcgis.com/datasets/87aa4eb6393644768a5f85929cc704c2_0.geojson") #super generalised (20m) - clipped to the coastline (Mean High Water mark); 
msoas <- rmapshaper::ms_simplify(msoas, keep = 0.05)
msoas <- select(msoas,"OBJECTID","MSOA11CD","MSOA11NM" )
izbounds <- geojson_sp("izboundaries_simplified.geojson")
izbounds <- izbounds %>% rename(MSOA11CD = "InterZone", MSOA11NM = "Name", OBJECTID = "rmapshaperid")
msoas <- rbind(izbounds, msoas, makeUniqueIDs = T)



msoas_eandw <- merge(msoas, selected_data, by.x = "MSOA11CD", by.y = "GEOGRAPHY_CODE", all.y = T) #merge in our data
#izbounds <- merge(izbounds, selected_data, by.x = "InterZone", by.y = "GEOGRAPHY_CODE", all.x = T)



#get the centroids
izcentroids <- read_sf("http://sedsh127.sedsh.gov.uk/arcgis/rest/services/ScotGov/StatisticalUnits/MapServer/8/query?where=1%3D1&text=&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelWithin&relationParam=&outFields=*&returnGeometry=true&returnTrueCurves=false&maxAllowableOffset=&geometryPrecision=&outSR=&returnIdsOnly=false&returnCountOnly=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&returnZ=false&returnM=false&gdbVersion=&returnDistinctValues=false&resultOffset=&resultRecordCount=&f=geojson")

izcentroids <- izcentroids %>% rename(objectid = OBJECTID, msoa11cd = InterZone, msoa11nm = Name)
#izcentroids <- as(izcentroids, "Spatial")
#izcentroids <- st_as_sf(izcentroids)

izcentroids <- izcentroids %>%  select(msoa11cd, msoa11nm, geometry)

izcentroids$objectid <- 7202:8480 #change object id so not duplicated when rbind

#izcentroids <- merge(izcentroids, selected_data, by.x = "msoa11cd", by.y = "GEOGRAPHY_CODE", all.x = T)
#izcentroids$`COVID-19sq_per100k_pop` <- sqrt(izcentroids$covid_per100k_pop)*0.3 #create square root metrics for circle radius's
#izcentroids$`COVID-19sq_ageadjusted_per100k_pop` <- sqrt(as.numeric(izcentroids$`COVID-19 Adjusted Mortality Rate per 100,000`))*0.3


msoa.centroids <- geojson_sp("https://opendata.arcgis.com/datasets/b0a6d8a3dc5d4718b3fd62c548d60f81_0.geojson")
#add in the IZ centroids
msoa.centroids <- st_as_sf(msoa.centroids)

msoa.centroids <-  rbind(izcentroids, msoa.centroids)

msoa.centroids_eandw <- merge(msoa.centroids, selected_data, by.x = "msoa11cd", by.y = "GEOGRAPHY_CODE", all.y = T) #merge in our data

msoa.centroids_eandw$`COVID-19sq_per100k_pop` <- sqrt(msoa.centroids_eandw$`COVID-19 deaths per 100,000`)*0.3 #create square root metrics for circle radius's
msoa.centroids_eandw$`COVID-19sq_ageadjusted_per100k_pop` <- sqrt(as.numeric(msoa.centroids_eandw$`COVID-19 Adjusted Mortality Rate per 100,000`))*0.3

#msoa.centroids_eandw <- as(msoa.centroids_eandw, "Spatial")

#get region layer to make clip layer
#NUTS1 <- geojson_sf("https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_3.geojson")
#NUTS1 <- NUTS1 %>% filter(nuts118nm != "London")


factpal <- colorFactor("RdBu",levels = levels(msoa.centroids_eandw$`IMD MSOA Deciles`), ordered = TRUE )
#factpal2 <- colorFactor("RdBu",levels = levels(izcentroids$`IMD MSOA Deciles`), ordered = TRUE )
#with age adjusted data

labels2 <- sprintf("<strong>%s</strong><br/>%g COVID-19 AMR per 100k<sup></sup>",
                   msoas_eandw$`Neighbourhood name`, round(msoas_eandw$`COVID-19 Adjusted Mortality Rate per 100,000`,0)) %>% lapply(htmltools::HTML)


# msoas_eandw <- msoas_eandw %>% rename("Neighbourhood name" = "House of Commons Library MSOA Names")
# msoa.centroids_eandw <- msoa.centroids_eandw %>% rename("Neighbourhood name" = "House of Commons Library MSOA Names")
# izbounds <- izbounds %>% rename("Neighbourhood name" = "MSOA11NM")
# izcentroids <- izcentroids %>%  rename("Neighbourhood name" = "msoa11nm")

# msoas_eandw <- rename(msoas_eandw,"Neighbourhood name" = `House of Commons Library MSOA Names`)
# msoa.centroids_eandw <- rename(msoa.centroids_eandw,"Neighbourhood name" = `House of Commons Library MSOA Names`)

msoas_eandw_db <- SharedData$new(msoas_eandw, group = "1") #sets the link to the interactive table , msoas_eandw$`House of Commons Library MSOA Names

msoa.centroids_eandw <- msoa.centroids_eandw %>% select(-msoa11cd) #drop msoa code for consistency with the shared data
msoa.centroids_eandw_db <- SharedData$new(msoa.centroids_eandw, group = "1") #, msoa.centroids_eandw$`House of Commons Library MSOA Names`
#izbounds_db <- SharedData$new(izbounds, izbounds$MSOA11NM, group = "1")
#izcentroids_db <- SharedData$new(izcentroids, izcentroids$`House of Commons Library MSOA Names`, group = "1")
#####################

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

#map element
m2 <- leaflet(msoas_eandw_db, height = "580px", options = list(padding = 100)) %>% setView(-3.5,53.2, 5.5) %>% 
  setMapWidgetStyle(list(background = "white")) %>% addProviderTiles(providers$CartoDB.Positron, providerTileOptions(opacity = 1) ) %>% 
  addMapPane(name = "toplayer", zIndex = 420) %>% 
  addMapPane(name = "nottoplayer", zIndex = 410) %>% 
  #addPolygons(data = NUTS1, color = "white",opacity = 1, fillColor = "white", fillOpacity = 1)  %>%        #add london clip
  #labels layer
  addPolygons(fillOpacity = 0, opacity = 0, 
              label = labels2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"),
              options = leafletOptions(pane = "toplayer")) %>% 
  #boundary line layer not adding much so not rendering
  # addPolygons(weight = 0.5, color = "white",
  #             fillColor = "#D3D3D3",
  # 
  #             label = labels2,
  #             labelOptions = labelOptions(
  #               style = list("font-weight" = "normal", padding = "3px 8px"),
  #               textsize = "15px",
  #               direction = "auto"),
  #             options = leafletOptions(pane = "nottoplayer")) %>%
  
#E&W MARKERS
addCircleMarkers(data = msoa.centroids_eandw_db, group = "circlegw",
                 radius = ~`COVID-19sq_ageadjusted_per100k_pop`,
                 stroke = F,
                 color = ~factpal(`IMD MSOA Deciles`), opacity = 0.85, fillOpacity = 0.85,
                 #color = "#0d2e5b",
                 options = leafletOptions(pane = "nottoplayer")) %>% 
  
    addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("10 deaths per 100k","100 deaths per 100k","500 deaths per 100k"),
                  
                  sizes = c(0.94,3.00,6.71)*2, position = "bottomright" ) %>% 
  
  addLegend(pal = factpal, values = msoas_eandw$`IMD MSOA Deciles`, 
            labels = levels(msoas_eandw$`IMD MSOA Deciles`), position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
  addResetMapButton() 
m2


#####################



combo <- htmltools::tagList(title,m2, tbl,sources) #I think this makes a combined html object
browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index.html", background = "#FFFCF1") #this saves it as an HTML page in the default folder.


