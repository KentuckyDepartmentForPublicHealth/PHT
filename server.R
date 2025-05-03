server <- function(input, output, session) {
  observe({
    # close sidebar when not in Home
    if (!input$navBar %in% "Home") {
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

# javascript for sidebar background color --------------------------------
# observe({
#   session$sendCustomMessage("toggleSidebarBg", input$mode_toggle)
# })
  # valuebox ----------------------------------------------------------------



  output$my_value_box <- renderUI({
    layout_columns(
      value_box(
        title = "% of 61 Local Health Departments who've submitted a Local Needs Assessment",
        value = paste0(sprintf("%0.0f", prop.table(table(shapefile$Status))[[2]] * 100), "%"),
        theme = value_box_theme(
          bg = if (input$mode_toggle %in% "dark") chfs$cols9[2] else chfs$cols9[9],
          fg = if (input$mode_toggle %in% "dark") "white" else chfs$cols9[2]
        ),
        showcase = icon("calendar-check"),
        showcase_layout = "top right", full_screen = T, fill = T,
        height = NULL # Set height of the value box
      ),
      value_box(
        title = "Number of downloadable files available in this site",
        value = numberOfListings,
        theme = value_box_theme(
          bg = if (input$mode_toggle %in% "dark") chfs$cols9[2] else chfs$cols9[9],
          fg = if (input$mode_toggle %in% "dark") "white" else chfs$cols9[2]
        ),
        showcase = icon("hashtag"),
        showcase_layout = "top right", full_screen = T, fill = T,
        height = NULL # Set height of the value box
      ),
      value_box(
        title = "Number of accredited local health departments",
        value = numberOfAccreditedLHDs,
        theme = value_box_theme(
          bg = if (input$mode_toggle %in% "dark") chfs$cols9[2] else chfs$cols9[9],
          fg = if (input$mode_toggle %in% "dark") "white" else chfs$cols9[2]
        ),
        showcase = icon("award"),
        showcase_layout = "top right", full_screen = T, fill = T,
        height = NULL # Set height of the value box
      )

    )
  })
  
output$priority_plot <- renderPlot({
  # Assuming `data`, `priority_labels`, and `chfs$cols7` are defined in your server
  priority_plot_data %>% 
  ggplot(aes(x = reorder(priority_label, prop), y = prop, fill = priority_label)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = scales::percent(prop, accuracy = 1)),
    hjust = -0.2, size = 5,
    color = if (input$mode_toggle == "dark") "white" else "black"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.2))
  ) +
  # Conditional fill palette (light mode only)
  {
    if (input$mode_toggle %in% "dark") NULL else scale_fill_manual(values = chfs$cols7)
  } +
  coord_flip() +
  labs(
    # x = "Public Health Priority",
    x = NULL,
    y = "Percent of all LHDs",
    title = "Local Public Health Priorities Selected by LHDs"
  ) +
  # Conditional theme (dark/light)
  { if (input$mode_toggle %in% "dark") 
      theme_dark(base_size = 16) +
      theme(
        panel.background = element_rect(fill = "#222222", color = NA),
        plot.background = element_rect(fill = "#111111", color = NA),
        text = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white")
      )
    else 
      theme_minimal(base_size = 16) +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        plot.title = element_text(color = "black")
      )
  } +
  theme(plot.margin = margin(t = 10, r = 40, b = 10, l = 10))

})

output$priority_plot_number_2 <- renderPlot({
    n_options <- dplyr::n_distinct(ship23$option2)
    col_vec <- rep(chfs$cols9, length.out = n_options)

  ship23 %>%
    mutate(option = as.factor(option2)) %>%
    ggplot(aes(x = reorder(option2, proportion), y = proportion, fill = option2)) +
    geom_col(show.legend = FALSE) +
    geom_text(
      aes(label = scales::percent(proportion, accuracy = 0.1)),
      hjust = -0.2, size = 4,
      color = if (input$mode_toggle == "dark") "white" else "black"
    ) +
    #Optionally add custom colors for light mode only
    {
      if (input$mode_toggle %in% "dark") NULL else scale_fill_manual(values = col_vec)
    } +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.2))
    ) +
    coord_flip() +
    labs(
    # x = "Public Health Priority",
    x = NULL,
      y = "Percent of Votes",
      title = "Statewide Public Health Priorities (SHIP)"
    ) +
    {
      if (input$mode_toggle == "dark")
        theme_dark(base_size = 16) +
        theme(
          panel.background = element_rect(fill = "#222222", color = NA),
          plot.background = element_rect(fill = "#111111", color = NA),
          text = element_text(color = "white"),
          axis.text = element_text(color = "white"),
          plot.title = element_text(color = "white")
        )
      else
        theme_minimal(base_size = 16) +
        theme(
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          text = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          plot.title = element_text(color = "black")
        )
    } +
    theme(
      plot.margin = margin(t = 10, r = 40, b = 10, l = 10)
    )
})

  # reset map -----

  # Use leafletProxy() to update the map on reset instead of re-rendering it
  observeEvent(input$resetMap, {
    updateRadioButtons(session, "labelthemap", selected = "12px")
    update_switch("showPHAB", value = FALSE)
    update_switch("showmarkers", value = TRUE)
    update_switch("showpopup", value = TRUE)
    updateSelectInput(session, "whichcounty", selected = "All")
    updateSelectInput(session, "whichqpal", selected = "Status")

    leafletProxy("map") %>%
      realignViewOfKentucky()
    #   output$map <- renderLeaflet({
    #     mapPrecursor()
    # })
  })

  # Use observeEvent to update the map when specific inputs change
  # observeEvent(list(input$whichqpal, input$mode_toggle, shapefileReactive()), {
  #   # Use leafletProxy to update only the polygons with new colors
  #   leafletProxy("map") %>%
  #     clearShapes() %>%
  #     addPolygons(
  #       data = shapefileReactive(),
  #       fillColor = ~qpal()(chosen_qpal_var()),
  #       fillOpacity = 1,
  #       weight = 2
  #     )
  # })


  # observeEvent(shapefileReactive(), {
  #   leafletProxy("map", data = shapefileReactive())
  # })
  #
  observeEvent(input$resetdownloads, {
    # Reset the selected input (e.g., set 'bydirectory' back to 'Allen County')
    updateSelectInput(session, "bydirectory", selected = "Allen County")

    # Clear the table by rendering a message or setting it to NULL
    output$file_table <- renderTable({
      data.frame(NULL)
    })
  })


  # qpal --------------------------------------------------------------------
chosen_qpal_var <- reactive({
  if (input$whichqpal == "Status") {
    shapefile$Status
  } else if (input$whichqpal == "phpriority_2025___1") {
    shapefile$phpriority_2025___1
  } else if (input$whichqpal == "phpriority_2025___2") {
    shapefile$phpriority_2025___2
  } else if (input$whichqpal == "phpriority_2025___3") {
    shapefile$phpriority_2025___3
  } else if (input$whichqpal == "phpriority_2025___4") {
    shapefile$phpriority_2025___4
  } else if (input$whichqpal == "phpriority_2025___5") {
    shapefile$phpriority_2025___5
  } else if (input$whichqpal == "phpriority_2025___6") {
    shapefile$phpriority_2025___6
  } else if (input$whichqpal == "phpriority_2025___7") {
    shapefile$phpriority_2025___7
  } else if (input$whichqpal == "is_accredited") {
    shapefile$is_accredited
  }

})


  qpal <- reactive({
    if (input$mode_toggle %in% "dark") {
      # dark
      # colorFactor(palette = c(chfs$cols9[2], chfs$cols9[7]), domain = chosen_qpal_var()) # original
      colorFactor(palette = c("#49120C", chfs$cols9[7]), domain = chosen_qpal_var())
    } else {
      # light
      # colorFactor(palette = c(chfs$cols9[9], chfs$cols9[1]), domain = chosen_qpal_var()) # original
      colorFactor(palette = c("#C78989", chfs$cols9[1]), domain = chosen_qpal_var()) 
    }
  })


  shapefileReactive <- eventReactive(
    c(input$whichcounty, input$whichqpal),
    {
      if (!input$whichcounty %in% "All") {
        shapefile %>%
          mutate(whichrow = row_number()) %>%
          # dplyr::filter(grepl(input$whichcounty, Listing))
          # dplyr::filter(str_detect(Listing, fixed(input$whichcounty, ignore_case = TRUE)))
          dplyr::filter(str_detect(Listing, paste0("(^|\\s)", input$whichcounty, "(\\s|$)")))
      } else {
        shapefile
      }
    },
    ignoreInit = F
  )

  # leaflet -----------------------------------------------------------------
  mapPrecursor <- reactive({
    leaflet(shapefileReactive()) %>%
      addProviderTiles(
        if (input$mode_toggle %in% "light") {
          providers$CartoDB.Positron
        } else {
          providers$CartoDB.DarkMatter
        }
      ) %>%
        # Add a tile layer with roads
    # addProviderTiles(providers$CartoDB.Positron) %>%  # Light, clean map with roads
    # Or alternatively:
    # addProviderTiles(providers$OpenStreetMap) %>%  # Standard OpenStreetMap
    # addProviderTiles(providers$Esri.WorldStreetMap) %>%  # More detailed street map
        # Option 1: CartoDB Dark Matter - Elegant dark map with subtle road network
      addPolygons(,
        fillColor = ~ qpal()(chosen_qpal_var()),
        fillOpacity = 1,
        color = 'white',
        weight = 2,
        label = if (!input$showpopup) {
          NULL
        } else {
          sprintf(
            "%s",
            paste(
              '<span style="font-size: 1.5em">',
              # "<b>Geography: </b>", shapefile$NAME10, '<br>',
              "<b>LHD: </b>", shapefileReactive()$NAME10, "<br>",
              "<b>Counties: </b>", shapefileReactive()$Listing, "<br>",
              # "<b>Variable: </b>", a('link', href = 'kde.org', target='_blank'), '<br>',
              "<b>LNA Submitted? </b>", shapefileReactive()$Status, "</span>"
            )
          ) %>%
            lapply(htmltools::HTML)
        }
      ) %>%
      addControl(paste("902 KAR 8:160 Local health department operations requirements", br(), br(), "Section 10: Identification of Local Needs"), position = "topright") %>%
          # Add this new control as a caption
    addControl(
      HTML("<div style='background-color: white; color: #0C3151; font-family: \"Helvetica Neue\", Arial, sans-serif; padding: 6px 8px; border-radius: 4px; box-shadow: 0 1px 5px rgba(0,0,0,0.4); font-size: 14px;'><i class='fa fa-map-marker'></i> Click location markers for downloads</div>"),
      position = "topleft"
    ) %>%
      addLegend(
          title = HTML(paste0("<span style='color: #0C3151; font-size: 1.2em;'>", 
                     names(which(color_map_choices == input$whichqpal)), 
                     "</span>")),
                position = 'topright',
                values = ~chosen_qpal_var(), # change here
                pal =  qpal(), # app WORKS
                # pal =  isolate(qpal()), # app WORKS
                # pal = qpal, # interactive
                opacity = 1
      ) %>%
      # location markers
      {
        if (!input$showmarkers) {
          .
        } else {
          addMarkers(.,
            lng = ~ as.numeric(unlist(INTPTLON10)),
            lat = ~ as.numeric(unlist(INTPTLAT10)),
            popup = ~ {
              lapply(1:nrow(shapefileReactive()), function(i) {
                marker_files <- nested_data_flat %>%
                  filter(NAME %in% shapefileReactive()$NAMELSAD10[i])

                if (nrow(marker_files) == 0 || all(is.na(marker_files$files))) {
                  "No files available"
                } else {
                  paste0(
                    "<ul class='mapfilebullet'>",
                    paste0("<li><a href='", marker_files$files,
                      "' target='_blank'>",
                      basename(marker_files$files),
                      "</a></li>",
                      collapse = ""
                    ),
                    "</ul>"
                  )
                }
              })
            }
          )
        }
      } %>%

# accredited health departments
{
  if (!input$showPHAB) {
    .
  } else {
    # Filter to only show features where Status is not NA and not empty
    filtered_shape <- shapefileReactive() %>%
      # filter(!is.na(Status) & Status != "")
      # filter(Status == "No")
      filter(is_accredited)

    # Prepare the custom star icon
    redStarIcon <- makeIcon(
      iconUrl = "star-32-gold.png",
      iconWidth = 32, iconHeight = 32,
      iconAnchorX = 16, iconAnchorY = 16
    )
    addMarkers(.,
      lng = ~ as.numeric(unlist(filtered_shape$INTPTLON10)),
      lat = ~ as.numeric(unlist(filtered_shape$INTPTLAT10)),
      icon = redStarIcon,
      popup = ~ {
        lapply(1:nrow(filtered_shape), function(i) {
          marker_files <- nested_data_flat %>%
            filter(NAME %in% filtered_shape$NAMELSAD10[i])
          if (nrow(marker_files) == 0 || all(is.na(marker_files$files))) {
            "No files available"
          } else {
            paste0(
              "<ul class='mapfilebullet'>",
              paste0("<li><a href='", marker_files$files,
                "' target='_blank'>",
                basename(marker_files$files),
                "</a></li>",
                collapse = ""
              ),
              "</ul>"
            )
          }
        })
      }
    )
  }
} %>%
      # addMarkers(lng = centroid_coords[, 1], lat = centroid_coords[, 2]) %>%
      {
        if (input$labelthemap %in% "nolabels") {
          .
        } else {
          addLabelOnlyMarkers(.,
            lng = ~ as.numeric(unlist(INTPTLON10)), lat = ~ as.numeric(unlist(INTPTLAT10)),
            label = ~NAMELSAD10,
            icon = NULL,
            labelOptions = labelOptions(
              noHide = TRUE,
              sticky = F,
              textsize = input$labelthemap,
              textOnly = T,
              style = if (input$mode_toggle %in% "light") {
                list(
                  "color" = "black",
                  "text-shadow" = "0 0 10px #fff, 0 0 10px #fff, 0 0 10px #fff"
                )
              } else {
                list(
                  "color" = "white",
                  "text-shadow" = "0 0 10px #95D3F5, 0 0 10px #95D3F5, 0 0 10px #95D3F5"
                )
              }
            )
          )
        }
      } %>%
      leaflet.extras::setMapWidgetStyle(., if (input$mode_toggle %in% "light") list(background = "#ddd") else list(background = "black")) %>%
      # {if (input$whichcounty %in% 'All') { realignViewOfKentucky(.) } else {setView(., lng = as.numeric(unlist(shapefileReactive()$INTPTLON10)), lat = as.numeric(unlist(shapefileReactive()$INTPTLAT10)), zoom = 9)}}
      {
        if (input$whichcounty %in% "All") {
          realignViewOfKentucky(.)
        } else {
          setView(., lng = centroid_coords[, 1][[shapefileReactive()$whichrow]], lat = centroid_coords[, 2][[shapefileReactive()$whichrow]], zoom = 10)
        }
      }
  })

  observeEvent(T,
    {
      output$map <- renderLeaflet({
        mapPrecursor()
      })
    },
    once = T
  )

  # MIGHT BE NEEDED Use observeEvent to update the map when specific inputs change
  # observeEvent(list(input$whichqpal, input$mode_toggle, shapefileReactive()), {
  #   # Use leafletProxy to update only the polygons with new colors
  #   leafletProxy("map") %>%
  #     clearShapes() %>%
  #     addPolygons(
  #       data = shapefileReactive(),
  #       fillColor = ~qpal()(chosen_qpal_var()),
  #       fillOpacity = 1,
  #       weight = 2
  #     )
  # })

  ## Downloads panel
  # Render the table with downloadable links
  observeEvent(input$searchdownloads, {
    selected_files <- reactive({
      nested_data2 %>%
        filter(NAME %in% isolate(input$bydirectory)) %>%
        unnest(files) # Unnest the files for the selected directory
    })

    output$file_table <- renderTable(
      {
        # Check if selected_files() is empty
        if (nrow(selected_files()) == 0) {
          # Return a data frame with the "No files available" message
          tibble("Downloadable Files" = paste("No files available for", isolate(input$bydirectory)))
        } else {
          # If there are files, generate the download links
          selected_files() %>%
            mutate(download_link = paste0("<a href='", files, "' download>", basename(files), "</a>")) %>%
            select(download_link) %>%
            rename("Downloadable Files" = download_link) # Display as clickable links
        }
      },
      sanitize.text.function = function(x) x
    ) # Disable sanitizing to allow HTML rendering
  })

  # Contact form logic -----
  
  status_type <- reactiveVal("ready")
  status_message <- reactiveVal("Ready to submit")
  
  output$status <- renderUI({
    div(
      class = "status-message",
      style = paste0(
        "padding: 15px; border-radius: 8px; ",
        "border-left: 5px solid ", if (grepl("Error", status_message())) "#f44336" else "#4caf50", "; ",
        "margin-top: 10px; box-shadow: 2px 2px 10px rgba(0,0,0,0.1);"
      ),
      tags$div(
        style = "display: flex; align-items: center;",
        tags$i(
          class = if (grepl("Error", status_message())) "fa fa-exclamation-circle" else "fa fa-check-circle",
          style = paste0(
            "margin-right: 10px; font-size: 24px; color: ",
            if (grepl("Error", status_message())) "#f44336" else "#4caf50"
          )
        ),
        h3(style = "margin: 0; font-weight: 500;", status_message())
      )
    )
  })
  
  observeEvent(input$submit, {
    # Validate inputs
    if (input$name == "" || input$email == "" || input$message == "") {
      showNotification(
        ui = div(
          tags$b("Error:"),
          "Please fill in all required fields."
        ),
        type = "error",
        duration = 5
      )
      status_type("error")
      status_message("Error: All fields are required!")
      return()
    }
    
    # Validate name for safe characters
    if (!grepl("^[A-Za-z0-9 ]+$", input$name)) {
      showNotification(
        ui = div(
          tags$b("Error:"),
          "Name contains invalid characters."
        ),
        type = "error",
        duration = 5
      )
      status_type("error")
      status_message("Error: Invalid characters in name!")
      return()
    }
    
    # Validate message isn't empty or only whitespace
    if (nchar(trimws(input$message)) == 0) {
      showNotification(
        ui = div(
          tags$b("Error:"),
          "Message cannot be empty or only whitespace."
        ),
        type = "error",
        duration = 5
      )
      status_type("error")
      status_message("Error: Message cannot be empty!")
      return()
    }
    
    status_type("ready")
    status_message("Processing submission...")
    
    api_token <- Sys.getenv("api_token")
    board_id <- Sys.getenv("board_id")
    
    # Log board_id for debugging
    cat("Board ID:", board_id, "\n")
    
    current_date <- format(Sys.Date(), "%Y-%m-%d")
    
    # Escape input$message for JSON and GraphQL
    escaped_message <- gsub('(["\\])', "\\\\\\1", input$message) # Escape quotes and backslashes
    escaped_message <- gsub("\n", "\\n", escaped_message) # Explicitly escape newlines
    escaped_message <- gsub("\t", "\\t", escaped_message) # Explicitly escape tabs
    cat("Escaped message:", escaped_message, "\n")
    
    column_values <- paste0(
      "{",
      '"text_mkq6vaar": "', current_date, '",',
      '"text_mkq6awc2": "', escaped_message, '",',
      '"text_mkq6cbxg": "', input$email, '"',
      "}"
    )
    
    # Log column_values for debugging
    cat("Column values:", column_values, "\n")
    
    query <- paste0("mutation {
    create_item (
      board_id: ", board_id, ',
      item_name: "', input$name, '",
      column_values: "', gsub('"', '\\\\"', column_values), '"
    ) {
      id
    }
  }')
    
    # Log query for debugging
    cat("Query:", query, "\n")
    
    # Send API request with error handling
    response <- tryCatch(
      {
        POST(
          url = "https://api.monday.com/v2",
          add_headers("Authorization" = api_token, "Content-Type" = "application/json"),
          body = list(query = query),
          encode = "json"
        )
      },
      error = function(e) {
        showNotification(
          ui = div(
            tags$b("Error:"),
            "Network issue connecting to Monday.com"
          ),
          type = "error",
          duration = 5
        )
        status_type("error")
        status_message("Error: Network issue connecting to Monday.com")
        return(NULL)
      }
    )
    
    if (is.null(response)) {
      return()
    }
    
    # Parse response
    response_body <- content(response, "parsed")
    cat("Response:", toJSON(response_body, pretty = TRUE), "\n")
    
    if (status_code(response) == 200 && !is.null(response_body$data$create_item$id)) {
      item_id <- response_body$data$create_item$id
      showNotification(
        ui = div(
          tags$b("Success!"),
          paste("Your message has been submitted. Item ID:", item_id)
        ),
        type = "message",
        duration = 5
      )
      updateTextInput(session, "message", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "email", value = "")
      status_type("success")
      status_message(paste("Successfully submitted! Item ID:", item_id))
    } else {
      error_msg <- if (!is.null(response_body$errors)) toJSON(response_body$errors, pretty = TRUE) else "Unknown error"
      showNotification(
        ui = div(
          tags$b("Error!"),
          paste("Status code:", status_code(response), "Details:", error_msg)
        ),
        type = "error",
        duration = 5
      )
      status_type("error")
      status_message(paste("Error submitting. Status code:", status_code(response), "Details:", error_msg))
    }
  })
  
    observeEvent(input$clear_form, {
      updateTextInput(session, "message", value = "")
      updateTextInput(session, "name", value = "")
      updateTextInput(session, "email", value = "")
    })
  

} # end Server
