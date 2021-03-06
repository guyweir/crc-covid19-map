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
selected_data <- selected_data[,c(1,6,42,46,48)]

#create nice new dataset with the variables from original data and the new COVID mortality with SMR AMRs etc.
selected_data <- merge(selected_data, AMRs, by = "GEOGRAPHY_CODE", all.y = T)


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


#final table selected cols and bit of a tidy up.
table1 <- selected_data  %>% mutate_at(.vars = "IMD MSOA Deciles", .funs = gsub, pattern = "1st ", replacement = "1 ") %>% 
  select(`House of Commons Library MSOA Names`, Laname, `IMD MSOA Deciles`,`COVID.19`, covid_per100k_pop, `COVID.19.AMR`) %>% 
  rename("Neighbourhood name" = `House of Commons Library MSOA Names`,"Local Authority" = Laname, "IMD MSOA Deciles" = `IMD MSOA Deciles`, "COVID-19 deaths unadjusted" = `COVID.19`,
         "COVID-19 deaths per 100,000" = covid_per100k_pop,
         "COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`) %>% 
  mutate_if(is.numeric,round,0) 

table2 <- SharedData$new(table1, group = "Neighbourhood name") #make it a shared object for the widget.



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


###### ADD MAP #########
suppressPackageStartupMessages(library(sp))
suppressPackageStartupMessages(library(sf))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggiraph))
suppressPackageStartupMessages(library(geojsonio))
suppressPackageStartupMessages(library(leaflet))
suppressPackageStartupMessages(library(leaflet.extras))

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
selected_data <- selected_data %>% select(GEOGRAPHY_CODE, `House of Commons Library MSOA Names`,`Region: `,
                                          `IMD MSOA Deciles`, covid_per100k_pop,
                                          `COVID.19`,`COVID.19.AMR`,
                                          `Index of Multiple Deprivation (IMD) Rank (where 1 is most deprived).x`) %>% 
  rename("COVID-19 Adjusted Mortality Rate per 100,000" = `COVID.19.AMR`)

msoas <- geojson_sp("https://opendata.arcgis.com/datasets/87aa4eb6393644768a5f85929cc704c2_0.geojson") #super generalised (20m) - clipped to the coastline (Mean High Water mark); 


#merge data and MSOA/iz boundaries
msoas_eandw <- merge(msoas, selected_data, by.x = "MSOA11CD", by.y = "GEOGRAPHY_CODE", all.y = T) #merge in our data

msoa.centroids <- geojson_sp("https://opendata.arcgis.com/datasets/b0a6d8a3dc5d4718b3fd62c548d60f81_0.geojson")
msoa.centroids_eandw <- merge(msoa.centroids, selected_data, by.x = "msoa11cd", by.y = "GEOGRAPHY_CODE", all.y = T) #merge in our data
msoa.centroids_eandw$`COVID-19sq_per100k_pop` <- sqrt(msoa.centroids_eandw$covid_per100k_pop)*0.3 #create square root metrics for circle radius's
msoa.centroids_eandw$`COVID-19sq_ageadjusted_per100k_pop` <- sqrt(as.numeric(msoa.centroids_eandw$`COVID-19 Adjusted Mortality Rate per 100,000`))*0.3



#get region layer to make clip layer
#NUTS1 <- geojson_sf("https://opendata.arcgis.com/datasets/01fd6b2d7600446d8af768005992f76a_3.geojson")
#NUTS1 <- NUTS1 %>% filter(nuts118nm != "London")


factpal <- colorFactor("RdBu",levels = levels(msoa.centroids_eandw$`IMD MSOA Deciles`), ordered = TRUE )

#with age adjusted data

labels2 <- sprintf("<strong>%s</strong><br/>%g COVID-19 AMR per 100k<sup></sup>",
                   msoas_eandw$`House of Commons Library MSOA Names`, round(msoas_eandw$`COVID-19 Adjusted Mortality Rate per 100,000`,0)) %>% lapply(htmltools::HTML)


factpal2 <- colorFactor("RdBu",domain = levels(msoa.centroids_eandw$`IMD MSOA Deciles`), ordered = TRUE )



msoas_eandw_db <- SharedData$new(msoas_eandw, group = "Neighbourhood name") #sets the link to the interactive table
msoa.centroids_eandw_db <- SharedData$new(msoa.centroids_eandw, group = "Neighbourhood name")

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
  
  
  addCircleMarkers(data = msoa.centroids_eandw_db, group = "circlegw",
                   radius = ~`COVID-19sq_ageadjusted_per100k_pop`,
                   stroke = F,
                   color = ~factpal(`IMD MSOA Deciles`), opacity = 0.85, fillOpacity = 0.85,
                   #color = "#0d2e5b",
                   options = leafletOptions(pane = "nottoplayer")) %>% 
  
  #addLegend(pal = pal,values = levels(msoas_eandw$colgroups), opacity = 0.8, title = "COVID-19 <br>deaths per 100,000 people",  position = "bottomright") %>%
    addLegendCustom(colors = c("grey", "grey", "grey"), 
                  labels = c("10 deaths per 100k","100 deaths per 100k","500 deaths per 100k"),
                  
                  sizes = c(0.94,3.00,6.71)*2, position = "bottomright" ) %>% 
    addLegend(pal = factpal, values = msoas_eandw$`IMD MSOA Deciles`, labels = levels(msoas_eandw$`IMD MSOA Deciles`), position = "bottomright") %>% 
  removeDrawToolbar(clearFeatures = T) %>% 
 addResetMapButton() 
m2


#####################



combo <- htmltools::tagList(title,m2, tbl,sources) #I think this makes a combined html object
browsable(combo)

############# Move index.html and lib folder manually into /docs htmltools doesn't support detailed file paths :( )
htmltools::save_html(combo, "index.html", background = "#FFFCF1") #this saves it as an HTML page in the default folder.


