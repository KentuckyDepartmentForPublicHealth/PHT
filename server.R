

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    mtcars |>
    ggplot(aes(!!sym(input$var), !!sym(input$var2))) +
      geom_point(size = 5) +
      theme_classic(base_size = 18) +
      theme(
        axis.text = element_text(size = 18)
      )
  })
  
  observeEvent(input$dark_mode, {
  output$map <- renderLeaflet({ 
    leaflet(ky_counties_joined) %>% 
      addPolygons(
        fillColor = ~qpal_cases(Compliance.Status),
        fillOpacity = 1,
        color = 'black',
        weight = 1,
        label = sprintf("%s",
                        paste('<span style="font-size: 1.5em">',
                              "<b>Geography: </b>", ky_counties_joined$NAME, '<br>',
                              # "<b>Variable: </b>", chosen_data()$variable2, '<br>',
                              "<b>Status: </b>", ky_counties_joined$Compliance.Status, '</span>'
                        )
        ) %>%
          lapply(htmltools::HTML)
      ) %>% 
      addLegend(title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 'This is a title',"</span>")),
                position = 'bottomright',
                values = ~Compliance.Status, # change here
                # pal =  qpal_cases(), # app WORKS
                # pal =  isolate(qpal_cases()), # app WORKS
                pal = qpal_cases, # interactive
                opacity = 1
      ) %>% 
     {if (input$dark_mode %in% 'dark') {leaflet.extras::setMapWidgetStyle(., list(background= 'black'))} else {leaflet.extras::setMapWidgetStyle(., list(background= 'white'))} } 
    })
  })
}
