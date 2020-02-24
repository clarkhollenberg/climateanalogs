##2/18/20
##Clark Hollenberg

libraries = c("knitr", "shiny", "leaflet","ncdf4", "lubridate", "dplyr",
              "zoo", "ggplot2", "scales", "raster", "leafem", "leaflet.extras")
lapply(libraries, library, character.only = TRUE)

setwd("~/ClimateAnalogs/ShinyApp")


#Read in rasters and LUT
#############
ecorast_now = raster("ecoregion_raster_current_ver2.tif")
ecorast_2C = raster("ecorast_2C_mapped.tif")
ecorast_4C = raster("ecorast_4C_mapped.tif")
ecorast_now_pr = raster("eco_rast_now_reproj.tif")
ecorast_2C_pr<-raster('ecorast_2C_reproj.tif')
ecorast_4C_pr<-raster('ecorast_4C_reproj.tif')
LUT<-read.csv('LUT_plus.csv')

uSAstatelines<-readOGR(dsn="C:/Users/clark/Documents/ClimateAnalogs/analysis/USA_state_shp",layer='cb_2018_us_state_5m')
nationalBorders<-readOGR(dsn="C:/Users/clark/Documents/ClimateAnalogs/analysis/countries_shp",layer='countries')
###############

#Define colors and plotting functions
#################

ecoColor<-function(rasterName)
{
  rangeValues<-c((minValue(rasterName)+1):(maxValue(rasterName)+1))
  col_plot <- as.character(LUT$color[rangeValues])
  return(col_plot)
}

ecoLegend<-function(rasterName1, rasterName2)
{
  df <- data.frame(unique(c(unique(rasterName1), unique(rasterName2))))
  colnames(df)<- "ECO_ID"
  df<-merge(df, LUT, by="ECO_ID")
  color_palate_leg <- as.character(df$color)
  legend_names <- as.character(df$econame)
  df<-data.frame('Names' = legend_names, 'Color' = color_palate_leg)
  return(df)
}

col_now <- ecoColor(ecorast_now)
col_2C <- ecoColor(ecorast_2C)
col_4C <-ecoColor(ecorast_4C)

leg <- ecoLegend(ecorast_now, ecorast_4C)

mapClicks<-function(x, y)
{
  df <- data.frame('x' = x, 'y' = y)
  ecoid <- extract(ecorast_now, df)
  tempy = subset(LUT, ECO_ID == ecoid)
  tempnow = as.character(tempy$econame)
  
  ecoid <- extract(ecorast_2C, df)
  temp = subset(LUT, ECO_ID == ecoid)
  temp2C = as.character(temp$econame)
  
  ecoid <- extract(ecorast_4C, df)
  temp = subset(LUT, ECO_ID == ecoid)
  temp4C = as.character(temp$econame)
  
  return(c(tempnow, temp2C, temp4C))
}
############

#####################
library(htmlwidgets)

    leaflet() %>% 
      addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
      addProviderTiles("Stamen.TonerLines") %>%
      addRasterImage(ecorast_now_pr, colors = col_now, project = F, opacity = 0.8, group = 'now') %>% 
      addRasterImage(ecorast_2C_pr, colors = col_2C, project = F, opacity = 0.8, group = '+2C') %>%
      addRasterImage(ecorast_4C_pr, colors = col_4C, project = F, opacity = 0.8, group = '+4C') %>%
    addLayersControl(
       baseGroups = c('now', '+2C', '+4C'),
       options = layersControlOptions(collapsed = FALSE)
          ) %>%
    saveWidget(file="ecoregions_map.html")
      
  

#   observeEvent(input$map_click, {
#     click <- input$map_click
#     econames <- mapClicks(click$lng, click$lat)
#     text<-paste("<p><u>Current</u>: <b>", econames[1], "</b></p>", "<p><u>+2C</u>: <b>", econames[2], "</b></p>",
#                 "<p><u>+4C</u>: <b>", econames[3], "</b></p>")
#     proxy <- leafletProxy("map")
#     proxy %>% clearPopups() %>%
#       addPopups(click$lng, click$lat, text)
#   })
# }  


#add mappane
#simplify polygons with sf
#reproject polygons
#leaflet click response
leaflet(options = leafletOptions(preferCanvas = T))
