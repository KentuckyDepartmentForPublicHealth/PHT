

server <- function(input, output, session) {

  # Activate thematic
  # thematic::thematic_on(font = "auto")

  # output$plot2 <- renderPlot({
  #   mtcars |>
  #     ggplot(aes(!!sym(input$var), !!sym(input$var2), color = factor(cyl))) 
  # })
  # 
  
  observe({
    # close sidebar when not in Home
    if (!input$navBar %in% 'Home') {
    sidebar_toggle(
      id = "mySidebar",
      open = F
    )
    } else {
    sidebar_toggle(
      id = "mySidebar",
      open = T
    )
    }
  })
  

# valuebox ----------------------------------------------------------------

  
  
  output$my_value_box <- renderUI({
    layout_columns(
    value_box(
      title = "% of Local Health Departments (county, district, and independent) who've submitted a Local Needs Assessment", 
      value = paste0(sprintf('%0.0f', prop.table(table(shapefile$Status) )[[2]] * 100 ), '%'),
      theme = value_box_theme(
        bg = if (input$mode_toggle %in% 'dark') chfs$cols9[2] else chfs$cols9[9],
        fg = if (input$mode_toggle %in% 'dark') 'white' else chfs$cols9[2]
      ),
      showcase = icon('calendar-check'),
      showcase_layout = "top right", full_screen = T, fill = T,
      height = NULL     # Set height of the value box
    ),
    value_box(
      title = "Number of downloadable files available in this site", 
      value = numberOfListings,
      theme = value_box_theme(
        bg = if (input$mode_toggle %in% 'dark') chfs$cols9[2] else chfs$cols9[9],
        fg = if (input$mode_toggle %in% 'dark') 'white' else chfs$cols9[2]
      ),
      showcase = icon('hashtag'),
      showcase_layout = "top right", full_screen = T, fill = T,
      height = NULL      # Set height of the value box
    )
    # value_box(
    #   title = "Top Health Priorities",
    #   value = HTML("<ol>
    #   <li>Substance Abuse</li>
    #   <li>Physical Health</li>
    #   <li>Mental Health</li>
    #   <li>Access To Care</li>
    #   <li>Tobacco</li>
    #   <li>Cancer</li>
    #   <li>Diabetes</li>
    #   <li>Food Security/Access</li>
    #   <li>Cardiovascular Disease</li>
    #   <li>Housing</li>
    #   </ol>"),
    #   theme = value_box_theme(
    #     bg = if (input$mode_toggle %in% 'dark') chfs$cols9[2] else chfs$cols9[9],
    #     fg = if (input$mode_toggle %in% 'dark') 'white' else chfs$cols9[2]
    #   ),
    #   showcase = icon('calendar-check'),
    #   showcase_layout = "top right", full_screen = T, fill = T,
    #   height = NULL      # Set height of the value box
    # )
    )
  })
  # 
  # Use leafletProxy() to update the map on reset instead of re-rendering it
  observeEvent(input$resetMap, {
    updateRadioButtons(session, 'labelthemap', selected = '12px')
    update_switch('showmarkers', value = TRUE)
    update_switch('showpopup', value = TRUE)
    updateSelectInput(session, 'whichcounty', selected = 'All')
    
    leafletProxy("map") %>% 
      realignViewOfKentucky()
  #   output$map <- renderLeaflet({
  #     mapPrecursor()  
  # })
  })
  
  

  
  # observeEvent(shapefileReactive(), {
  #   leafletProxy("map", data = shapefileReactive())
  # })
  # 
  observeEvent(input$resetdownloads, {
    # Reset the selected input (e.g., set 'bydirectory' back to 'Allen County')
    updateSelectInput(session, 'bydirectory', selected = 'Allen County')
    
    # Clear the table by rendering a message or setting it to NULL
    output$file_table <- renderTable({
      data.frame(NULL)
    })
  })
  

# qpal --------------------------------------------------------------------



  qpal <- reactive({
  if (input$mode_toggle %in% 'dark') {
    
  # dark  
colorFactor(palette = c(chfs$cols9[2], chfs$cols9[7]), domain = shapefile$Status)
  } else {  
  
  # light
colorFactor(palette = c(chfs$cols9[9], chfs$cols9[1]), domain = shapefile$Status)
  }
  })
  
  shapefileReactive <- eventReactive(input$whichcounty, {
    if (!input$whichcounty %in% 'All') {
      shapefile %>%
        dplyr::filter(grepl(input$whichcounty, Listing))
    } else {
      shapefile 
    }
  }, ignoreInit = F)
  
# leaflet -----------------------------------------------------------------
  mapPrecursor <- reactive({
    
    leaflet(shapefileReactive()) %>% 
      addPolygons(,
        fillColor = ~qpal()(Status),
        fillOpacity = 1,
        color = 'white',
        weight = 2,
        label = if (!input$showpopup) {NULL} else {
          sprintf("%s",
           paste('<span style="font-size: 1.5em">',
                 # "<b>Geography: </b>", shapefile$NAME10, '<br>',
                 '<b>LHD: </b>', shapefileReactive()$NAME10, '<br>',
                 '<b>Counties: </b>',shapefileReactive()$Listing, '<br>',
                 # "<b>Variable: </b>", a('link', href = 'kde.org', target='_blank'), '<br>',
                 "<b>Submitted? </b>", shapefileReactive()$Status, '</span>'
           )
        ) %>%
            lapply(htmltools::HTML)}
      ) %>% 
      addControl(paste('902 KAR 8:160 Local health department operations requirements',br(), br(), 'Section 10: Identification of Local Needs'), position = 'topright') %>%
      addLegend(title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 'Submitted?',"</span>")),
                position = 'topright',
                values = ~Status, # change here
                pal =  qpal(), # app WORKS
                # pal =  isolate(qpal()), # app WORKS
                # pal = qpal, # interactive
                opacity = 1
      ) %>%
      {if (!input$showmarkers) {.} else {
        addMarkers(.,
                   lng = ~ as.numeric(unlist(INTPTLON10)),
                   lat = ~ as.numeric(unlist(INTPTLAT10)),
                   popup = ~{
                     lapply(1:nrow(shapefileReactive()), function(i) {
                       marker_files <- nested_data_flat %>%
                         filter(NAME %in% shapefileReactive()$NAMELSAD10[i])

                       if (nrow(marker_files) == 0 || all(is.na(marker_files$files))) {
                         "No files available"
                       } else {
                         paste0("<ul class='mapfilebullet'>",
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
          style = if (input$mode_toggle %in% 'light') {
            list(
              "color" =  "black",
                'text-shadow' = '0 0 10px #fff, 0 0 10px #fff, 0 0 10px #fff'
            ) }
          else {
           list(
              "color" = "white",
                'text-shadow' = '0 0 10px #95D3F5, 0 0 10px #95D3F5, 0 0 10px #95D3F5'
          )
      }
      ))}} %>%
      leaflet.extras::setMapWidgetStyle(., if (input$mode_toggle %in% 'light') list(background = '#ddd') else list(background = 'black'))
      # {if (!input$whichcounty %in% 'All') {setView(., lng = as.numeric(unlist(INTPTLON10)), lat = as.numeric(unlist(INTPTLAT10)), zoom = 8)} else  { realignViewOfKentucky(.) } }# %>%
  })
  
  observeEvent(T, {
    output$map <- renderLeaflet({
      mapPrecursor()  
    })
  }, once = T)
  
  
## Downloads panel
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
