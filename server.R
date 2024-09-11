

server <- function(input, output, session) {

  # Activate thematic
  # thematic::thematic_on(font = "auto")

  # output$plot2 <- renderPlot({
  #   mtcars |>
  #     ggplot(aes(!!sym(input$var), !!sym(input$var2), color = factor(cyl))) 
  # })
  # 
  
  observeEvent(input$resetMap, {
               updateRadioButtons(session, 'labelthemap', selected = '12px')
               update_switch('showmarkers', value = T)
               update_switch('showpopup', value = T)
               output$map <- renderLeaflet({ 
                 mapPrecursor()
               }) 
               })
  
  
  observeEvent(input$resetdownloads, {
    # Reset the selected input (e.g., set 'bydirectory' back to 'Allen County')
    updateSelectInput(session, 'bydirectory', selected = 'Allen County')
    
    # Clear the table by rendering a message or setting it to NULL
    output$file_table <- renderTable({
      data.frame(NULL)
    })
  })
  

# leaflet -----------------------------------------------------------------

  mapPrecursor <- reactive({
    
    leaflet(shapefile) %>% 
      addPolygons(
        fillColor = ~qpal(Compliance.Status),
        fillOpacity = 1,
        color = 'white',
        weight = 2,
        label = if (!input$showpopup) {NULL} else {sprintf("%s",
                                                           paste('<span style="font-size: 1.5em">',
                                                                 # "<b>Geography: </b>", shapefile$NAME10, '<br>',
                                                                 '<b>LHD: </b>', shapefile$NAME10, '<br>',
                                                                 '<b>Counties: </b>',shapefile$Listing, '<br>',
                                                                 # "<b>Variable: </b>", a('link', href = 'kde.org', target='_blank'), '<br>',
                                                                 "<b>Submitted? </b>", shapefile$Compliance.Status, '</span>'
                                                           )
        ) %>%
            lapply(htmltools::HTML)}
      ) %>% 
      addControl(paste('Title 902 | Chapter 008 | Regulation 160',br(), br(), 'Local Needs Assessment'), position = 'topright') %>%
      addLegend(title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 'Submitted?',"</span>")),
                position = 'topright',
                values = ~Compliance.Status, # change here
                # pal =  qpal(), # app WORKS
                # pal =  isolate(qpal()), # app WORKS
                pal = qpal, # interactive
                opacity = 1
      ) %>%
      # {if (!input$showmarkers) {.} else { addMarkers(.,
      #            lng = ~ as.numeric(unlist(INTPTLON10)), 
      #            lat = ~ as.numeric(unlist(INTPTLAT10)),
      #            popup = sprintf(
      #                paste0('<a href="%s" target="_blank">CHA/CHIP</a>'),
      #                serve_submissions1
      #              )
      #   )}} %>%
      {if (!input$showmarkers) {.} else {
        addMarkers(.,
                   lng = ~ as.numeric(unlist(INTPTLON10)),
                   lat = ~ as.numeric(unlist(INTPTLAT10)),
                   popup = ~{
                     lapply(1:nrow(shapefile), function(i) {
                       marker_files <- nested_data_flat %>%
                         filter(NAME == shapefile$NAMELSAD10[i])
                       
                       if (nrow(marker_files) == 0 || all(is.na(marker_files$files))) {
                         "No files available"
                       } else {
                         paste0("<ul>", 
                                paste0("<li><a href='", marker_files$files, 
                                       "' target='_blank'>", 
                                       basename(marker_files$files), 
                                       "</a></li>", collapse = ""), 
                                "</ul>")
                       }
                     })
                   })
      }} %>%
      # addMarkers(lng = centroid_coords[, 1], lat = centroid_coords[, 2]) %>% 
      {if (input$labelthemap %in% 'nolabels') {.} else {addLabelOnlyMarkers(.,
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
      )}} %>% 
      setView(., lng = -85.711244, lat = 37.735969, zoom = 8) %>% 
      leaflet.extras::setMapWidgetStyle(., list(background= 'white'))
  })
  
  output$map <- renderLeaflet({ 
    if (!input$resetMap) mapPrecursor()
    }) # end renderLeaflet
  
  ## Downloads panel
  # Reactive expression to return the files based on the selected directory

  # Render the table with downloadable links
  observeEvent(input$searchdownloads, {
    
    selected_files <- reactive({
      nested_data2 %>%
        filter(NAME %in% isolate(input$bydirectory)) %>%
        unnest(files)  # Unnest the files for the selected directory
    })
    
    output$file_table <- renderTable({
      # Check if selected_files() is empty
      if (nrow(selected_files()) == 0) {
        # Return a data frame with the "No files available" message
        tibble("Downloadable Files" = paste("No files available for", isolate(input$bydirectory)))
      } else {
        # If there are files, generate the download links
        selected_files() %>%
          mutate(download_link = paste0("<a href='", files, "' download>", basename(files), "</a>")) %>%
          select(download_link) %>%
          rename("Downloadable Files" = download_link)  # Display as clickable links
      }
    }, sanitize.text.function = function(x) x)  # Disable sanitizing to allow HTML rendering
  })

} # end Server
