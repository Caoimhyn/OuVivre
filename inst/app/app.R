library(shiny)
library(leaflet)
library(dplyr)
library(ggmap)
library(shinydashboard)
library(DT)

this_table = data.frame(lat = NULL, lng =NULL, Distance = NULL)

ui <- fluidPage(
  navbarPage("Où Vivre", id="nav",

             tabPanel("Interactive map",
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
                                        sliderInput("distance", "Distance maximum d'éloignement en mètres",min=0, max=50000, step = 1, value=1000))

                      ),

                      tags$div(id="cite",
                               'All Data are available on ', tags$em('https://www.data.gouv.fr/fr/'), ' and compiled by Kevin POTARD.'
                      )
             ),
             tabPanel("Data"

                      )
  )
)

server <- function(input, output, session) {
  # --------- MAP panel
  output$map<- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, dragging = T))%>%
      addProviderTiles(provider = "OpenStreetMap.France")%>%
      setView(lng = 2.43, lat=46.53,zoom = 7) %>%
      setMaxBounds(lng1 = 2.43 + 9,
                   lat1 = 46.53 + 12,
                   lng2 = 2.43 - 7,
                   lat2 = 46.53 - 10)

  })

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
  })

  # ------------- Data Absolute panel
  this_table <- reactiveVal(this_table)

  observeEvent(input$map_click, {
    click <- input$map_click
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




}

shinyApp(ui, server)
