

server <- function(input, output, session) {

  # Activate thematic
  thematic::thematic_on(font = "auto")

  
  output$plot <- renderPlot({
    mtcars |>
    ggplot(aes(!!sym(input$var), !!sym(input$var2))) +
      geom_point(size = 5) +
      theme_classic(base_size = 18) +
      labs(title = 'This is the first chart') +
      theme(
        axis.text = element_text(size = 18)
      )
  })
  
  output$plot2 <- renderPlot({
    mtcars |>
      ggplot(aes(!!sym(input$var), !!sym(input$var2), color = factor(cyl))) +
      geom_point(size = 5) +
      theme_classic(base_size = 18) +
      labs(title = 'This is a second chart') +
      theme(
        axis.text = element_text(size = 18)
      )
  })
  
  observeEvent(input$dark_mode, {
  output$map <- renderLeaflet({ 
    
    leaflet(shapefile) %>% 
      addPolygons(
        fillColor = ~qpal(Compliance.Status),
        fillOpacity = 1,
        color = 'white',
        weight = 2
        # label = sprintf("%s",
        #                 paste('<span style="font-size: 1.5em">',
        #                       "<b>Geography: </b>", ky_counties_joined$NAME, '<br>',
        #                       # "<b>Variable: </b>", a('link', href = 'kde.org', target='_blank'), '<br>',
        #                       "<b>Status: </b>", ky_counties_joined$Compliance.Status, '</span>'
        #                 )
        # ) %>%
        #   lapply(htmltools::HTML)
      ) %>% 
      # addLegend(title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 'Compliant?',"</span>")),
      #           position = 'bottomright',
      #           values = ~Compliance.Status, # change here
      #           # pal =  qpal_cases(), # app WORKS
      #           # pal =  isolate(qpal_cases()), # app WORKS
      #           pal = qpal_cases, # interactive
      #           opacity = 1
      # ) %>% 
      addMarkers(.,
                 lng = centroid_coords[, 1], #~ as.numeric(unlist(INTPTLON10)), 
                 lat = centroid_coords[, 2], #~ as.numeric(unlist(INTPTLAT10)),
                 popup = PoPuP
        ) %>%
      # addMarkers(lng = centroid_coords[, 1], lat = centroid_coords[, 2]) %>% 
      addLabelOnlyMarkers(
        lng = centroid_coords[, 1], lat = centroid_coords[, 2],
        label = ~ NAMELSAD10, 
        icon = NULL,
        labelOptions = labelOptions(
          noHide = TRUE,
          sticky=F,
          textsize = '10px', 
          textOnly = T,
          style = list("color" = 'white')
        )
      ) %>% 
      setView(., lng = -85.711244, lat = 37.735969, zoom = 8) %>% 
      # addControl(paste('Title 902 | Chapter 008 | Regulation 160',br(), br(), 'Local Needs Assessment'), position = 'topright') %>% 
     {if (input$dark_mode %in% 'dark') {leaflet.extras::setMapWidgetStyle(., list(background= '#454545'))} else {leaflet.extras::setMapWidgetStyle(., list(background= 'white'))} } 
    }) # end renderLeaflet
  }) # end observeEvent
}
