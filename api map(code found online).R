library(shiny)
library(googleway)

rsconnect::setAccountInfo(name='spriteliu', token='2B2BDB1EE7D94484580FC1A8FB53F194', secret='2jN02SWK8nT0irg6Or/JR6p+aHaZe699Hld5Apki')

ui <- navbarPage("APP NAME", position = c("static-top"),tabPanel("MAP",
                                                                 google_mapOutput(outputId = "mapWarsaw"),
                                                                 textInput(inputId = "origin", label = "Departure point"),
                                                                 textInput(inputId = "waypoint", label = "Waypoint 1"),
                                                                 textInput(inputId = "waypoint2", label = "Waypoint 2"),
                                                                 textInput(inputId = "destination", label = "Destination point"),
                                                                 actionButton(inputId = "getRoute", label = "Get Route")
)
)

server <- function(input, output, session) {
  
  map_key <- "https://maps.googleapis.com/maps/api/directions/json?origin=Disneyland&destination=Universal+Studios+Hollywood&key=AIzaSyAF0P-Ju5YqPBbSA3u66Xu7WbWQP7n-mgI"
  api_key <- "AIzaSyDGeXXfD_R2lBGkJoS5tWNCIraM9ku-tkA"
  
  output$mapWarsaw <- renderGoogle_map({
    google_map(key = map_key, 
               search_box = TRUE, 
               scale_control = TRUE, 
               height = 1000) %>%
      add_traffic()
  })
  
  observeEvent(input$getRoute,{
    
    print("getting route")
    
    o <- input$origin
    w <- input$waypoint
    q <- input$waypoint2
    d <- input$destination
    
    res <- google_directions(key = api_key,
                             origin = o,
                             waypoints = list(stop = w,
                                              stop = q),
                             destination = d,
                             optimise_waypoints = TRUE,
                             mode = "driving")
    
    df_route <- data.frame(route = res$routes$overview_polyline$points)
    
    df_way <- cbind(
      res$routes$legs[[1]]$end_location,
      data.frame(address = res$routes$legs[[1]]$end_address)
    )
    
    df_way$order <- as.character(1:nrow(df_way))
    
    google_map_update(map_id = "mapWarsaw") %>%
      clear_traffic() %>%
      clear_polylines() %>%
      clear_markers() %>%
      add_traffic() %>%
      add_polylines(data = df_route,
                    polyline = "route",
                    stroke_colour = "#FF33D6",
                    stroke_weight = 7,
                    stroke_opacity = 0.7,
                    info_window = "New route",
                    load_interval = 100) %>%
      add_markers(data = df_way,
                  info_window = "end_address",
                  label = "order")
  })
}


shinyApp(ui, server)