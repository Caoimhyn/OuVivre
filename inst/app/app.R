library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(shinydashboard)
library(DT)
library(sf)
library(tmap)
library(leaflet.extras)

this_table = data.frame(lat = NULL, lng = NULL, Distance = NULL)

ui <- fluidPage(
  tabsetPanel(id="nav",

             tabPanel("InteractiveMap",
                      tags$head(
                        # Include our custom CSS
                        includeCSS("./www/style.css")
                      ),

                      leafletOutput("map", height=900),
                      # Shiny versions prior to 0.11 should use class = "modal" instead.
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                    width = 450, height = "auto",

                                    h2("Paramètres"),

                                    box(DTOutput("data"),
                                        actionButton("delete_btn", "Delete"),
                                        sliderInput("distance", "Distance maximum d'éloignement en mètres",min=0, max=50000, step = 1, value=1000),
                                        actionButton("generateZone", "Générer les lieux de vies possible"))


                                    ),

                      tags$div(id="cite",
                               'All Data are available on ', tags$em('https://www.data.gouv.fr/fr/'), ' and compiled by Kevin POTARD.'
                      )
             ),
             tabPanel("result",
                      leafletOutput("my_tmap")

                      ),
             tabPanel("Data")
  )
)

server <- function(input, output, session) {
  # --------- MAP panel
  output$map<- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, dragging = T))%>%
      addProviderTiles(provider = "OpenStreetMap.France")%>%
      addSearchOSM()%>%
      setView(lng = 2.43, lat=46.53,zoom = 7) %>%
      setMaxBounds(lng1 = 2.43 + 9,
                   lat1 = 46.53 + 12,
                   lng2 = 2.43 - 7,
                   lat2 = 46.53 - 10)

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
      addMarkers(lng=clng, lat=clat, group='markers')

    t = rbind(data.frame(lat = click$lat,
                         lng = click$lng,
                         Distance = input$distance), this_table())
    this_table(t)
  })


  observeEvent(input$delete_btn, {
    t = this_table()
    if (!is.null(input$data_rows_selected)) {
      t <- t[-as.numeric(input$data_rows_selected),]

    }
    this_table(t)
  })

  observeEvent(input$delete_btn, {
    proxy <- leafletProxy('map')
    if (!is.null(input$data_rows_selected)){
      print(input$data_rows_selected)
      proxy %>% removeMarker("1")
    }
  })

  output$data<-renderDT({
    datatable(this_table(), selection = 'single', options = list(dom = 't'))
  })

  observeEvent(input$generateZone,{


     updateTabsetPanel(session, "nav",
                       selected = "result")
  })

  # ----------------- Result Panel

  output$my_tmap = renderLeaflet({
    df_sf <- st_as_sf(this_table(), coords = c("lng", "lat"), crs=4326)
    df_sf<- st_transform(df_sf, crs=3857)

    df_buf <- st_buffer(df_sf, dist = df_sf$Distance)
    geom<-st_geometry(df_buf)
    geom_union<-st_union(geom)

    geom_union_tran <- st_transform(geom_union,  crs=4326)

    proxy <- leafletProxy('map')
    proxy%>%
      clearMarkers()%>%
      addPolygons(data=geom_union_tran)
  })
}

shinyApp(ui, server)
