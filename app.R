
require(sf)
require(leaflet)
require(leaflet.extras)
require(RColorBrewer)
require(pals)

rutas<- st_read('rutas_FINAL.gpkg')

colnames(rutas) <- c('Nombre', 'Distancia_Km', 'Desnivel_Positivo', 'geom')


rutas <- st_transform(rutas, crs="+proj=longlat +datum=WGS84 +no_defs")
st_geometry(rutas) <- 'geom'

server <- function(input, output, session) {

  filteredData <- reactive({
    rutas[rutas$Distancia_Km >= input$range[1] & rutas$Distancia_Km <= input$range[2] &
            rutas$Desnivel_Positivo >= input$range2[1] & rutas$Desnivel_Positivo <= input$range2[2], ]
  })
    
  factpal <- colorFactor(palette = polychrome(25), rutas$Nombre)
  

  output$map <- renderLeaflet({

    leaflet(rutas) %>% addTiles() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      
      addPolylines(color= ~factpal(Nombre), label = paste("Nombre", rutas$Nombre, "<br>",
                                                          "Distancia:", rutas$Distancia_Km, "<br>",
                                                          "Desnivel:", rutas$Desnivel_Positivo), opacity = 1,
					  highlightOptions = highlightOptions(
                   			  weight = 5,
					  stroke = 4,
                    			  color = "red",
                    			  bringToFront = TRUE)
)  %>%
      #### Barra de herramientas de dibujo
      addDrawToolbar(
        targetGroup='rutas',
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  

  })
  
  observe({
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolylines(color= ~factpal(Nombre), label = paste("Nombre:", substr(filteredData()$Nombre, 0, 30),',',
                                                           "Distancia:", filteredData()$Distancia_Km,',',
                                                           "Desnivel:", filteredData()$Desnivel_Positivo), opacity = 1,
					labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
              						textsize = "15px",direction = "auto"),
              
  					  highlightOptions = highlightOptions(
                   			  weight = 5,
					  stroke = 4,
                    			  color = "red",
                    			  bringToFront = TRUE,
					  sendToBack = TRUE,
					  opacity = 1))
  })

  
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$filenames, '.gpx', sep='')
    },
    content = function(file) {
      write_sf(rutas[rutas$Nombre == input$filenames, ]$geom, driver='GPX', file)
    }
  )
}
