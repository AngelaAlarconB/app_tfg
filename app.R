library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(leaflet.providers)
library(sp)
library(sf)
library(dplyr)
library(forecast)
library(TTR)

#Sys.setlocale(category = "LC_ALL", locale = "en_US.UTF-8")

separator <- file.path("dummy", "dummy")
separator <- substr(separator, 6, 6)

#Leer datos
load(paste0("data",separator,"datos.rda"))
load(paste0("data",separator,"grid300.rda"))
load(paste0("data",separator,"barrios.rda"))

#tratamiento de datos
datos <- datos %>%  rename('LONGITUD' = 'X_UTM') %>% rename('LATITUD' = 'Y_UTM') %>% rename('FECHA' = 'FH_INICIOINCIDENTE')

datos <- datos[(datos$LONGITUD>-6.0),]
datos$Crimen <- ifelse(datos$Crimen == 'Robo con violencia/intimidacion',
                       'Robo con violencia o intimidacion', 
                       datos$Crimen)


#convertir datos y ajustar CRS
coordinates (datos)= ~LONGITUD + LATITUD
proj4string(datos) <- CRS("+init=epsg:25830")
datos <- spTransform(datos, CRS("+proj=longlat")) 

#datos para mapa de puntos
datos_mapa <- as.data.frame(datos) 

crimenes <- datos_mapa$Crimen %>% unique()
crimenes_stkde <- paste0(crimenes, "_stkde")

# Colores 
Colores <- c("black", "red", "orange", "darkgreen", "purple","blue") 
# Generamos la paleta
paleta <- colorFactor(Colores, domain = crimenes)

# Datos para cada crimen
lista_crimen <- list()
labels <- list()
for (i in 1:length(crimenes)) {
  lista_crimen[[i]] <- datos_mapa %>% dplyr::filter(Crimen == crimenes[i])
}
names(lista_crimen) <- crimenes

#Rejilla de Valencia
grid <- spTransform(grid, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
gridn <- SpatialPolygonsDataFrame(grid, data.frame(id=1:length(grid)))

barrios <- spTransform(barrios, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

datos_kde <- list()
for (c in names(lista_crimen)) {
  nombre <- gsub(" ", "_", c)
  datos_kde[[c]] <- readRDS(paste0("data",separator,nombre,"_kde",".rds"))
  gridn[[c]] <- rep(0, nrow(gridn))
  gridn[[c]] <- datos_kde[[c]]
}

labels <- list()
mostrarmapa <- function(){
  # Crear mapa con capas
  mapa <- leaflet() %>% addTiles()
  
  # Centrar y zoom
  mapa <- mapa %>% setView(lng =-0.371 , lat = 39.47, zoom = 13) 
  for (i in 1:length(crimenes)) {
    labels[[i]] <- sprintf(
      "Crimen:<string>%s</string><br/>Fecha: %s",
      lista_crimen[[i]]$Crimen,lista_crimen[[i]]$FECHA) %>%  lapply(htmltools::HTML)
    
    mapa <- mapa %>% addCircles(data = lista_crimen[[i]], lat = ~LATITUD, 
                                lng = ~LONGITUD, color = ~paleta(Crimen), 
                                fillOpacity = 1, label = labels[[i]], 
                                group = crimenes[i])
  }
  
  #Leyenda
  mapa <- mapa %>% leaflet::addLegend(data = datos, "bottomright", 
                             pal = paleta, values = ~Crimen, title = "Crimenes", 
                             opacity = 1, group = "Leyenda")
  
  #Control
  Grupos <- c("Leyenda", crimenes)
  
  mapa <- mapa %>% addLayersControl(overlayGroups = Grupos,
                                    options = layersControlOptions(collapsed = TRUE))
  
  
  mapa
}

mostrarmapacalor <- function(){
  #Mapa de calor
  mapa_calor <- leaflet() %>% addTiles()
  for (i in 1:length(crimenes)) {
    mapa_calor <- mapa_calor %>% addHeatmap(data = lista_crimen[[i]], 
                                            lat = ~LATITUD, lng = ~LONGITUD, 
                                            group = crimenes[i], blur = 20, radius = 10)
  }
  # Centrar y zoom
  mapa_calor <- mapa_calor %>% setView(lng =-0.371 , lat = 39.47, zoom = 13) 
  mapa_calor <- mapa_calor %>% addLayersControl(baseGroups = crimenes, 
                                                options = layersControlOptions(collapsed = FALSE))
  mapa_calor
}

mostrarmapakde <- function(){
  mapakde <- leaflet()  %>% addProviderTiles("Stamen.TonerLite")
  for (c in names(lista_crimen)) {
    paletaKde <- colorNumeric(palette = "viridis", domain = gridn[[c]]) 
    mapakde <- mapakde %>% addPolygons(data=gridn, weight = 0.1, fillColor=~paletaKde(gridn[[c]]), group = c, dashArray="3", fillOpacity = 0.9)
  }
  # Centrar y zoom
  mapakde <- mapakde %>% setView(lng =-0.371 , lat = 39.47, zoom = 13) 
  mapakde <- mapakde %>% addLayersControl(baseGroups = crimenes, 
                                          options = layersControlOptions(collapsed = FALSE))
  mapakde
}

hs <- 300
ht <- 7

mostrarmapastkde <- function(fecha){
  
  datosfecha <- readRDS(paste0("data", separator,as.character(fecha),".rds"))
  for (c in names(lista_crimen)) {
    gridn[[paste0(c, "_stkde")]] <- rep(0, nrow(gridn))
    gridn[[paste0(c, "_stkde")]] <- datosfecha[[c]]
  }
  
  mapastkde <- leaflet()  %>% addProviderTiles("Stamen.TonerLite")
  for (c in names(lista_crimen)) {
    paletastKde <- colorNumeric(palette = "viridis", domain = na.omit(gridn[[paste0(c, "_stkde")]]))
    mapastkde <- mapastkde %>% addPolygons(data=gridn, weight = 0.1, fillColor=~paletastKde(gridn[[paste0(c, "_stkde")]]), group = paste0(c, "_stkde"), dashArray="3", fillOpacity = 0.9)
  }
  # Centrar y zoom
  mapastkde <- mapastkde %>% setView(lng =-0.371 , lat = 39.47, zoom = 12)
  mapastkde <- mapastkde %>% addLayersControl(baseGroups = crimenes_stkde,
                                              options = layersControlOptions(collapsed = FALSE))
  
  mapastkde
}

mostrarmapasstemp <- function(){
  mapasstemp <- leaflet() %>% addProviderTiles("Stamen.TonerLite") %>% addPolygons(data=barrios,
                                                                                   options = pathOptions(clickable = TRUE))
  mapasstemp <- mapasstemp %>% setView(-0.376288, 39.469907, zoom = 12)

  mapasstemp
}

PoligonoShapePunto <- function(shp, lon, lat){
  aux_point=data.frame(lon,lat)
  coordinates(aux_point)=~lon+lat
  proj4string(aux_point)="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  aux=over(aux_point,shp)
  poligono=which(shp$SpUnit==aux$SpUnit)
  return(poligono)
}


sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Inicio', tabName = 'inicio', icon = icon("home"))
    , menuItem('Crimenes', tabName = 'crimenes')
    , menuItem('Mapa de calor', tabName = 'mapacalor')
    , menuItem('Método KDE', tabName = 'kde')
    , menuItem('Método STKDE', tabName = 'stkde')
    , menuItem('Suavizado EMA', tabName = 'ema')
    , menuItem('Suavizado ARIMA', tabName = 'sstempstkde')
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "inicio", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#image {height: calc(100vh - 80px) !important;}"), 
            uiOutput("image")),
    tabItem(tabName = "crimenes", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapa {height: calc(100vh - 80px) !important;}"), 
            leafletOutput("dibujarmapa",width="100%", height="100%")),
    tabItem(tabName = "mapacalor", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapacalor {height: calc(100vh - 80px) !important;}"),  
            leafletOutput("dibujarmapacalor", width="100%", height="100%")),
    tabItem(tabName = "kde", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapakde {height: calc(100vh - 80px) !important;}"), 
            leafletOutput("dibujarmapakde", width="100%", height="100%")),
    tabItem(tabName = "stkde",
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapastkde {height: calc(100vh - 80px) !important;}"), 
            div(
              style = "margin-left: 1cm;",
            sliderInput("fecha", "Fecha:", min = base::as.Date("2014-01-01","%Y-%m-%d"),
                        max = base::as.Date("2016-01-01","%Y-%m-%d"),
                        value= base::as.Date("2014-10-27","%Y-%m-%d"),
                        timeFormat="%Y-%m-%d", dragRange = FALSE)),
            leafletOutput("dibujarmapastkde",height = "100%")),
    tabItem(tabName = "ema", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapaema {height: calc(100vh - 80px) !important;}"),
            fluidRow(
              column(
                width = 7,
                leafletOutput("dibujarmapaema", width="100%", height="100%")
              ),
              column(
                width = 4,
                absolutePanel(
                  id = "control_suavizado", 
                  class = "panel panel-default", 
                  top = 65, 
                  bottom = 100,
                  right = 15, 
                  width = 470, 
                  fixed = T, 
                  draggable = F, 
                  height = "auto", 
                  size = "auto",
                  selectInput("crimen_seleccionado_ema", "Seleccionar crimen", choices = unique(datos$Crimen), selected = unique(datos$Crimen)[1]),
                  plotOutput("suavizadoema", width = "100%", height = "100%")
                )
              )
            )
    ),
    tabItem(tabName = "sstempstkde", 
            tags$style(type = "text/css", "html, body {width:100%;height:100%}"), 
            tags$style(type = "text/css", "#dibujarmapasstempstkde {height: calc(100vh - 80px) !important;}"),
            fluidRow(
              column(
                width = 7,
                leafletOutput("dibujarmapasstempstkde", width="100%", height="100%")
              ),
              column(
                width = 4,
                absolutePanel(
                  id = "control_suavizado", 
                  class = "panel panel-default", 
                  top = 65, 
                  bottom = 100,
                  right = 15, 
                  width = 470, 
                  fixed = T, 
                  draggable = F, 
                  height = "auto", 
                  size = "auto",
                  selectInput("crimen_seleccionado_stkde", "Seleccionar crimen", choices = unique(datos$Crimen), selected = unique(datos$Crimen)[1]),
                  plotOutput("suavizadostkde", width = "100%", height = "100%")
                )
              )
            )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "Valencia - Análisis de delincuencia", 
                  titleWidth = 400),
  sidebar,
  body
)      


#server
server <- function(input, output) {

  
  output$image <- renderUI({
    img_path <- "portadaapp.png"
    img(src = img_path, width = "1100", height = "600")
  })
  
  
  output$suavizadostkde <- renderPlot({
    if(!is.null(input$dibujarmapasstempstkde_shape_click)){
      poligono = PoligonoShapePunto(barrios, input$dibujarmapasstempstkde_shape_click$lng,
                                    input$dibujarmapasstempstkde_shape_click$lat)
      
      barrio <- barrios$NOMBRE[barrios$SpUnit==poligono]
      n_barrio <- gsub("[.'\\s]", "_", barrio, perl = TRUE)
      datosbarrio <- readRDS(paste0("data",separator,n_barrio,".rds"))
      crimen_seleccionado <- input$crimen_seleccionado_stkde
      datosbarrio <- datosbarrio[[crimen_seleccionado]]
      
      serie_temporal <- ts(datosbarrio, start = c(2014, 1), frequency = 365)
      suavizado <- auto.arima(serie_temporal)
      predicciones <- forecast(suavizado, h = 5)
      point_forecast <- predicciones$mean
      intmin <- predicciones$lower[, 2]
      intmax <- predicciones$upper[, 2]
      suavizado_fitted <- as.data.frame(suavizado$fitted)
      
      dates <-  round(seq(2015.9946, length.out = 6, by = 0.0054), 4)
      etiquetas <- c("Jan 2014", "Jul 2014", "Jan 2015", "Jul 2015", "Jan 2016")
      
      plot.ts(serie_temporal, main = paste0("Serie temporal y suavizado", "\n", barrios[barrios$SpUnit == poligono,]$NOMBRE), ylab = "Valor", xlab = "Fecha", xaxt = "n")
      
      lines(suavizado$fitted, col = "red", lwd = 3)
      
      lines(dates, c(suavizado_fitted$x[729], point_forecast) , col = "green", lwd = 3)
      lines(dates,c(suavizado_fitted$x[729], intmin) , col = "blue", lty = 2)
      lines(dates,c(suavizado_fitted$x[729], intmax) , col = "blue", lty = 2)
      
      axis(1, at = c(2014.0,2014.5,2015.0,2015.5,2016.0), labels = etiquetas)
      
      observeEvent(input$crimen_seleccionado_stkde, {
        crimen_seleccionado_stkde <<- input$crimen_seleccionado_stkde
      })
    }
  })
  
  output$suavizadoema <- renderPlot({
    if(!is.null(input$dibujarmapaema_shape_click)){
      poligono = PoligonoShapePunto(barrios, input$dibujarmapaema_shape_click$lng,
                                    input$dibujarmapaema_shape_click$lat)
      
      barrio <- barrios$NOMBRE[barrios$SpUnit==poligono]
      n_barrio <- gsub("[.'\\s]", "_", barrio, perl = TRUE)
      datosbarrio <- readRDS(paste0("data",separator,n_barrio,".rds"))
      crimen_seleccionado <- input$crimen_seleccionado_ema
      datosbarrio <- datosbarrio[[crimen_seleccionado]]
      
      serie_temporal <- ts(datosbarrio, start = c(2014, 1), frequency = 365)
      suavizado <- EMA(serie_temporal, n = 30)
      predicciones <- forecast(suavizado, h = 5)
      point_forecast <- predicciones$mean
      intmin <- predicciones$lower[, 2]
      intmax <- predicciones$upper[, 2]
      
      dates <-  round(seq(2015.9946, length.out = 6, by = 0.0054), 4)
      etiquetas <- c("Jan 2014", "Jul 2014", "Jan 2015", "Jul 2015", "Jan 2016")
      
      plot.ts(serie_temporal, main = paste0("Serie temporal y suavizado", "\n", barrios[barrios$SpUnit == poligono,]$NOMBRE), ylab = "Valor", xlab = "Fecha", xaxt = "n")
      lines(as.numeric(time(serie_temporal)), suavizado, col = "red", lwd = 3)
      
      lines(dates, c(suavizado[729], point_forecast) , col = "green", lwd = 3)
      lines(dates, c(suavizado[729], intmin) , col = "blue", lty = 2)
      lines(dates, c(suavizado[729], intmax) , col = "blue", lty = 2)
      
      axis(1, at = c(2014.0,2014.5,2015.0,2015.5,2016.0), labels = etiquetas)
      
      observeEvent(input$crimen_seleccionado_ema, {
        crimen_seleccionado_ema <<- input$crimen_seleccionado_ema
      })
    }
  })
  
  output$dibujarmapa <- renderLeaflet({
    mostrarmapa()
  })
  
  output$dibujarmapacalor <- renderLeaflet({
    mostrarmapacalor()
  })

  output$dibujarmapakde <- renderLeaflet({
  mostrarmapakde()
  })
  
  output$dibujarmapastkde <- renderLeaflet({
    mostrarmapastkde(input$fecha)
  })
  
  output$dibujarmapaema <- renderLeaflet({
    mostrarmapasstemp()
  })

  output$dibujarmapasstempstkde <- renderLeaflet({
    mostrarmapasstemp()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
