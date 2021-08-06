library(raster)
library(mapview)
library(leaflet)
library(leafem)
library(tidyverse)
library(shiny)
library(sf)
library(DT)
library(mapedit)

r <- raster("alt_gsmnp_agg.grd")

r1 = aggregate(r, 2)

sp = st_as_sf(rasterToPolygons(r1))
cn = st_coordinates(st_transform(st_centroid(sp),4326))
sp = st_transform(sp, 4326)
sp = cbind(sp, cn)
sp$id <- 1:nrow(sp)
colnames(sp)[1] <- "value"


## UI
ui <- fluidPage(
  leafletOutput("map"),
  uiOutput("newValueUI"),
  textInput("newVal", label = "Enter new value"),
  actionButton("enter", "Enter new value"),
  hr(),
  dataTableOutput("table")
)


## SERVER
server <- function(input, output){
  
  ## Reactive Shapefile
  sp_react <- reactiveValues(sp = sp)
  
  ## Leaflet Map
  output$map <- renderLeaflet({
    pal= colorNumeric(topo.colors(25), sp_react$sp$value)
    leaflet() %>% 
      addPolygons(data = sp_react$sp, label= paste(
        "Lng: ", as.character(round(sp_react$sp$X,4)),
        "Lat: ", as.character(round(sp_react$sp$Y,4)),
        "Val: ", as.character(round(sp_react$sp$value,4))),
        color = ~pal(sp_react$sp$value), 
        layerId = sp_react$sp$id
      )
  })
  
  ## Observe Map Clicks
  observeEvent(input$map_shape_click, {
    
    click_id = input$map_shape_click$id
    
    click_grid <- sp_react$sp[sp_react$sp$id == click_id,]
    
  })
  
  ## Observe Action Button
  observeEvent(input$enter, {
    click_id <- input$map_shape_click$id
    sp_react$sp[sp_react$sp$id == click_id,]$value <- as.numeric(input$newVal)
  })
  
  ## Data Table
  output$table <- DT::renderDataTable({
    sp_react$sp %>% st_set_geometry(NULL) %>% 
      dplyr::select(id,X,Y,value)
  })
  proxy = dataTableProxy('table')
  
  ## Table Proxy
  observeEvent(input$map_shape_click$id, {
    req(input$map_shape_click$id)
    proxy %>% selectRows(as.numeric(input$map_shape_click$id))
  })
}

shinyApp(ui, server)


###########################################

mapview(r1) %>%
  editMap()




