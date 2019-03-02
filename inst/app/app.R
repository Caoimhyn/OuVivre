library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(shinydashboard)
library(DT)
library(sf)
library(tmap)
library(leaflet.extras)
library(dashboardthemes)

widthParam = 400

ui <- dashboardPage(dashboardHeader(titleWidth = widthParam),
                    dashboardSidebar(width = widthParam,
                     br(),
                        box(title ="Paramètres", width = 395, collapsible = T,solidHeader = T,status = "primary",
                        DTOutput("data"),
                        actionButton("delete_btn", "Supprimer"),
                        sliderInput(
                          "distance",
                          "Eloignement max (km)",
                          min = 0,
                          max = 100,
                          step = 0.5,
                          value = 20
                        ),
                        actionButton("generateZone", "Où vivre?")
                          ),
                        box(title = "Résultats", width = 395, collapsible = T,solidHeader = T,status = "primary",
                            tabsetPanel(tabPanel("Tous les critères", DTOutput("results", width = 380)),
                                        tabPanel("Au moins 1", DTOutput("resCrit1",width = 380))
                                    )
                            ),
                     actionButton("reset", "Recommencer")
                      ),
                    dashboardBody(
                      shinyDashboardThemes(
                        theme = "grey_dark"
                      ),
                      tabsetPanel(
                      id = "nav",

                      tabPanel(
                        "Cartes",
                        tags$head(# Include our custom CSS
                          includeCSS("./www/style.css")),

                        leafletOutput("map", height = 900),


                        tags$div(
                          id = "cite",  'All Data are available on ', tags$em('https://www.data.gouv.fr/fr/'),' and compiled by Kevin POTARD.'
                        )
                      ),
                      tabPanel("Données utilisées",
                               DTOutput("cities"),
                               tags$div(
                                 id = "cite",  'All Data are available on ', tags$em('https://www.data.gouv.fr/fr/'),' and compiled by Kevin POTARD.'
                               ))
                    )))

server <- function(input, output, session) {
  this_table = data.frame()
  this_results <- data.frame()
  crit1 <- data.frame()
  data <- as_tibble(read.csv("./Data/cities.csv"))
  data_sf <-st_as_sf(data, coords = c("gps_lng", "gps_lat"), crs = 4326)

  # --------- MAP panel
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, dragging = T)) %>%
      addProviderTiles(provider = "OpenStreetMap.France") %>%
      addSearchOSM() %>%
      setView(lng = 2.43, lat = 46.53, zoom = 7) %>%
      setMaxBounds(
        lng1 = 2.43 + 9,
        lat1 = 46.53 + 12,
        lng2 = 2.43 - 7,
        lat2 = 46.53 - 10
      )
  })

  this_table <- reactiveVal(this_table)

  ## Observe mouse clicks and add markers
  observeEvent(input$map_click, {
    ## Get the click info like had been doing
    click <- input$map_click
    clat <- click$lat
    clng <- click$lng

    ## Add the maker to the map proxy
    ## not need to re-render the whole thing
    ## the markers a group, "markers", so you can
    ## then do something like hide all the markers with hideGroup('markers')
    leafletProxy('map') %>% # use the proxy to save computation
      addMarkers(lng = clng,
                 lat = clat,
                 group = 'markers')

    t = rbind(
      data.frame(
        Latitude = round(click$lat,2),
        Longitude = round(click$lng,2),
        Distance = input$distance
      ),
      this_table()
    )
    this_table(t)
  })


  observeEvent(input$delete_btn, {
    t = this_table()
    if (!is.null(input$data_rows_selected)) {
      t <- t[-as.numeric(input$data_rows_selected), ]

    }
    this_table(t)
  })

  observeEvent(input$delete_btn, {
    proxy <- leafletProxy('map')
    if (!is.null(input$data_rows_selected)) {
      print(input$data_rows_selected)

      proxy %>%
        clearMarkers()%>%
        addMarkers(lng = this_table()[,2], lat = this_table()[,1])
    }
  })

  observeEvent(input$reset_btn, {
    this_table(data.frame())
    this_results(data.frame())
    crit1(data.frame())

    leafletProxy('map') %>%
      clearMarkers()%>%
      clearShapes()
  })

  output$data <- renderDT({
    data <- this_table()
    if(data == data_frame()){
    datatable(this_table(), selection = 'single', options = list(dom = 't'))
    }else
      datatable("Aucune donnée disponnible, veuillez cliquer sur la carte", options = list(dom = 't'))
  })


  # ----------------- sidepar Panel

  observeEvent(input$generateZone, {
    df_sf <- st_as_sf(this_table(),
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)
    df_sf <- st_transform(df_sf, crs = 3857)

    df_buf <- st_buffer(df_sf, dist = df_sf$Distance * 1000)
    geom <- st_geometry(df_buf)
    geom_union <- st_union(geom)

    geom_intersect = NULL

    for (i in 1:length(geom)[-2]) {
      if (i == 1) {
        geom_intersect <- st_intersection(x = geom[[i]], y = geom[[i + 1]])
      } else if (i == 2) {
        geom_intersect <- geom_intersect

      } else{
        geom_intersect <- st_intersection(geom_intersect, geom[[i]])
      }
    }

    proxy <- leafletProxy('map')

    geom_union_tran <- st_transform(geom_union,  crs = 4326)

    proxy %>%
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = geom_union_tran)

    min <- st_contains(geom_union_tran, data_sf)
    min <- as.numeric(min[[1]])

    crit1(data[min, c("zip_code", "name")])

    if (length(geom_intersect) != 0) {
      geom_intersect <- st_sfc(st_cast(geom_intersect, "POLYGON"), crs = 3857)
      geom_intersect_tran <- st_transform(geom_intersect,  crs = 4326)

      proxy %>%
        addPolygons(data = geom_intersect_tran, color = "red")

      all <- st_contains(geom_intersect_tran, data_sf)
      all <- as.numeric(all[[1]])

      this_results(data[all, c("zip_code", "name")])
    }
  })

  # panel au moins 1 critère
  crit1<-reactiveVal(crit1)

  output$resCrit1 <- renderDT({
    datatable(crit1())
  })

  # panel tous les critères
  this_results <- reactiveVal(this_results)

  output$results <- renderDT({
    datatable(this_results())
  })


  #--------------- Data panel

  output$cities <- renderDT({
    data[, -c(1, 6)]
  })

}

shinyApp(ui, server)
