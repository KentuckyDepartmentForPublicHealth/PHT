

server <- function(input, output, session) {

  # Activate thematic
  # thematic::thematic_on(font = "auto")

  # output$plot2 <- renderPlot({
  #   mtcars |>
  #     ggplot(aes(!!sym(input$var), !!sym(input$var2), color = factor(cyl))) 
  # })
  # 
  
  observeEvent(input$resetMap,
               updateRadioButtons(session, 'labelthemap', selected = '12px')
               )
  
  observeEvent( list(input$resetMap), {
  output$map <- renderLeaflet({ 
    
    leaflet(shapefile) %>% 
      addPolygons(
        fillColor = ~qpal(Compliance.Status),
        fillOpacity = 1,
        color = 'black',
        weight = 2,
        label = sprintf("%s",
                        paste('<span style="font-size: 1.5em">',
                              # "<b>Geography: </b>", shapefile$NAME10, '<br>',
                             shapefile$NAME10, '<br>',
                              # "<b>Variable: </b>", a('link', href = 'kde.org', target='_blank'), '<br>',
                              "<b>Compliant? </b>", shapefile$Compliance.Status, '</span>'
                        )
        ) %>%
          lapply(htmltools::HTML)
      ) %>% 
      addControl(paste('Title 902 | Chapter 008 | Regulation 160',br(), br(), 'Local Needs Assessment'), position = 'topright') %>%
      addLegend(title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 'Compliant?',"</span>")),
                position = 'topright',
                values = ~Compliance.Status, # change here
                # pal =  qpal(), # app WORKS
                # pal =  isolate(qpal()), # app WORKS
                pal = qpal, # interactive
                opacity = 1
      ) %>%
      addMarkers(.,
                 lng = ~ as.numeric(unlist(INTPTLON10)), 
                 lat = ~ as.numeric(unlist(INTPTLAT10)),
                 popup = sprintf(
                     paste0('<a href="%s" target="_blank">CHA/CHIP</a>'),
                     serve_submissions1
                   )
        ) %>%
      # addMarkers(lng = centroid_coords[, 1], lat = centroid_coords[, 2]) %>% 
      addLabelOnlyMarkers(
        lng = ~ as.numeric(unlist(INTPTLON10)), lat = ~ as.numeric(unlist(INTPTLAT10)),
        label = ~ NAMELSAD10, 
        icon = NULL,
        labelOptions = labelOptions(
          noHide = TRUE,
          sticky=F,
          textsize = input$labelthemap,
          textOnly = T,
          style = list("color" = 'black') #e95420 #772953
        )
      ) %>% 
      setView(., lng = -85.711244, lat = 37.735969, zoom = 8) %>% 
     leaflet.extras::setMapWidgetStyle(., list(background= 'white'))
    }) # end renderLeaflet
  }) # end observeEvent
  
  ## Downloads panel
  # Reactive expression to return the files based on the selected directory
  selected_files <- reactive({
    nested_data %>%
      filter(dir == input$bydirectory) %>%
      unnest(files)  # Unnest the files for the selected directory
  })
  
  # Render the table with downloadable links
  observeEvent(input$searchdownloads, {
  output$file_table <- renderTable({
    selected_files() %>%
      mutate(download_link = paste0("<a href='", files, "' download>", basename(files), "</a>")) %>%
      select(download_link) %>%
      rename("Downloadable Files" = download_link)  # Display as clickable links
  }, sanitize.text.function = function(x) x)  # Disable sanitizing to allow HTML rendering
})

} # end Server
