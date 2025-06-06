options(rsconnect.max.bundle.size = 4000000000) # Set to 4 GB

# libs --------------------------------------------------------------------

library(shiny)
library(bslib)
# library(bsicons)
# library(thematic)
# library(ggplot2)
library(DT)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(leaflet)
library(sf)
library(leaflet.extras)
library(fresh)
library(shinycssloaders)
library(bsicons)
library(httr)
library(jsonlite)
library(ggplot2)
library(scales)
# colors ------------------------------------------------------------------

chfs <- list(
  cols2 = c('#95D3F5', '#0C3151'),
  cols3 = c('#62BCF0', '#01203D', '#84BC49'),
  cols4 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48'),
  cols5 = c('#95D3F5', '#5CB2E5', '#0C3151', '#517F44', '#9FCA70'),
  cols6 = c('#95D3F5', '#5CB2E5', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols7 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#305E4C', '#517F44', '#9FCA70'),
  cols8 = c('#95D3F5', '#5CB2E5', '#3A7CA6', '#0C3151', '#00060C', '#305E4C', '#517F44', '#9FCA70'),
  cols9 = c('#5CB2E5', '#0C3151', '#305E4C', '#76AB48','#00060C', '#305E4C', '#517F44', '#76AB48', '#9FCA70')
)

# timestamp
# Read the timestamp that was saved by deploy.R
currentDate <- tryCatch(
  {
    readRDS(file = "dat/currentDate.rds")
  },
  error = function(e) {
    format(Sys.time(), "%a, %b %d, %Y at %I:%M %p %Z") # Fallback
  }
)

# theme -------------------------------------------------------------------
# my_theme <- bs_theme(5, bootswatch = 'sandstone')

# base_theme <- bs_theme(
#   version = 5,
#   primary = "#007bff",
#   secondary = "#6c757d",
#   # Set up custom variables for dark mode if needed
#   "body-bg" = "#ffffff",
#   "body-color" = "#000000"
# )
# boilerplate -------------------------------------------------------------

# numberOfListings <- system("find './www/Listing' -type f ! -path '*/.*'  | sort | wc -l", intern = T)
numberOfListings <- list.files('./www/Listing', recursive = T) |> length()

realignViewOfKentucky <- function(shapefile) {
  setView(shapefile, lng = -85.711244, lat = 37.735969, zoom = 8)
}

# modeToggling <- function(map, background) {
#   leaflet.extras::setMapWidgetStyle(map, list(background = 'red'))
# }


# load(file = 'dat/ky_joined.RData')


# resourceLinks -----------------------------------------------------------




link_khda <- tags$a(
  shiny::icon("building"), "Kentucky Health Departments Association (KHDA)",
  href = "https://khda-ky.org/",
  target = "_blank"
)
link_naccho <- tags$a(
  shiny::icon("thumbtack"), "NACCHO Directory of Local Health Departments",
  href = "https://www.naccho.org/membership/lhd-directory?searchType=standard&lhd-state=KY#card-filter",
  target = "_blank"
)
link_lhdmap <- tags$a(
  shiny::icon("globe"), "Kentucky Districts and Counties (PDF Map)",
  href = "doc/LHDdistrictsandcounties.pdf",
  target = "_blank"
)

link_sha2023 <- tags$a(
  shiny::icon("file-lines"), "State Health Assessment (SHA) 2023",
  href = "doc/StateHealthAssessment2023.pdf",
  target = "_blank"
)

link_sha2023_exec <- tags$a(
  shiny::icon("file-lines"), "SHA Executive Summary 2023",
  href = "doc/SHA Executive Summary - final approved.pdf",
  target = "_blank"
)

link_ship2024 <- tags$a(
  shiny::icon("file-lines"), "State Health Improvement Plan (SHIP) 2024-2028",
  href = "doc/SHIP2024-28.pdf",
  target = "_blank"
)

link_ship2024_exec <- tags$a(
  shiny::icon("file-lines"), "SHIP Executive Summary 2024-2028",
  href = "doc/SHIPExecutiveSummary.pdf",
  target = "_blank"
)

link_pht_flyer <- tags$a(
  shiny::icon("bullhorn"), "PHT Dashboard Flyer",
  href = "KDPH PHT Dashboard.png",
  target = "_blank"
)

link_pht_source <- tags$a(
  shiny::icon("github"), "PHT Dashboard Source Code",
  href = "https://github.com/KentuckyDepartmentForPublicHealth/PHT",
  target = "_blank"
)


# leaflet -----------------------------------------------------------------
# ky_counties <- DBI::dbGetQuery(con, 'select * from geo.ky_county') %>%
#   select(NAME2) %>%
#   pull()
# ky_counties[62] <- 'LaRue'
# saveRDS(ky_counties, 'dat/ky_counties.rds')
ky_counties <- readRDS('dat/ky_counties.rds') 
# select2cols <- readxl::read_xlsx('dat/select2cols.xlsx') |>
# dplyr::select(FID, Listing, Status)
# 
# save(select2cols, file = 'dat/select2cols.RData')
load(file = 'dat/select2cols.RData') # add Listing

# shapefile <- st_read('dat/temp/ky-lhd.shp')
# shapefile <- st_transform(shapefile, crs = 4326)
# shapefile <- merge(shapefile, select2cols, by = 'FID')
# save(shapefile2025, file = 'dat/shapefile2025.RData')
load(file = 'dat/shapefile2025.RData') # from redcap-api in onedev
shapefile <- shapefile2025

shapefile <- shapefile %>%
  mutate(
    across(
      starts_with("phpriority") & !matches("factor|oth|8"),
      ~ case_when(
        as.integer(.) == 1 ~ "Yes",
        as.integer(.) == 0 ~ "No",
        TRUE ~ NA_character_
      ),
      .names = "{.col}_yn"
    ),
        Status = if_else(NAMELSAD10 %in% c("Johnson County", "Lincoln County"), "No", "Yes")

  )

  # Store total number of LHDs (records) separately
  total_lhds <- nrow(shapefile)

# test qpal dynamic inputs
# valuesforcolor <- sample(c("Yes", "No", "Yes", "Yes", "Yes", "Yes"), 61, replace = T)
# save(valuesforcolor, file = 'dat/valuesforcolor.RData')
# load(file = 'dat/valuesforcolor.RData')
# print(valuesforcolor)
# shapefile$colored1 <- valuesforcolor
# shapefile$colored2 <- valuesforcolor
# # print(shapefile$colored1)
# shapefile$Status2 <- shapefile$Status



# centroids <- st_centroid(shapefile)
# centroid_coords <- st_coordinates(centroids)
# saveRDS(centroid_coords, 'dat/centroid_coords.rds')
centroid_coords <- readRDS('dat/centroid_coords.rds')
# shapefile$Compliance.Status <- sample(c('Yes','No','Yes','Yes','Yes','Yes'), 61, replace = T)

# pal <- colorNumeric(palette = "viridis", domain = shapefile$FID)
# qpal <- colorFactor(palette = c('#004080', '#001F3F'), domain = shapefile$Compliance.Status)
# qpal <- colorFactor(palette = c(chfs$cols9[6], chfs$cols9[2]), domain = shapefile$Status)

# ship23 <- read.csv('dat/ship23.csv')
# ship23$option2 <-  c(
#   "Alcohol Use",
#   "Asthma",
#   "Sexual Activity",
#   "Infectious Diseases",
#   "Oral Health",
#   "Alzheimers",
#   "Suicide/Self Harm",
#   "Cancer",
#   "Diabetes",
#   "Immunizations",
#   "Maternal Child Health",
#   "Cardiovascular Disease",
#   "Adverse Childhood",
#   "Community Safety",
#   "Physical Activity",
#   "Obesity",
#   "Drug Use",
#   "Nutrition",
#   "Smoking/Vaping",
#   "Mental Health",
#   "Access to Care"
# )
# save(ship23, file ='dat/ship23.RData')
load(file = 'dat/ship23.RData')


# Function to read directories and capture files along with their folder names
read_files_from_directories <- function(base_dir) {
  # List all directories (folders)
  dirs <- list.dirs(base_dir, recursive = FALSE, full.names = TRUE)
  
  # Create a tibble with the directory name as NAME and the files inside as 'files'
  tibble(NAME = basename(dirs)) %>%
    mutate(files = map(dirs, function(d) {
      files <- list.files(d, full.names = TRUE)
      
      # Remove leading "www/" if present
      files <- gsub("^www/", "", files)
      
      # Collapse multiple slashes to a single slash
      files <- gsub("/+", "/", files)
      
      return(files)
    }))
}

# Example directories on your host machine
nested_data2 <- read_files_from_directories(c('www/Listing/'))

nested_data_flat <- nested_data2 %>%
  unnest(cols = files, keep_empty = T)



# Define the color map choices once
color_map_choices <- c(
  "LNA Submission Status" = "Status",
  "PHAB: Is Accredited?" = "is_accredited",
  "Local Priority: Chronic Disease" = "phpriority_2025___1",
  "Local Priority: Cancer Control" = "phpriority_2025___2",
  "Local Priority: Women's Health" = "phpriority_2025___3",
  "Local Priority: Pediatric Health" = "phpriority_2025___4",
  "Local Priority: Adult Health" = "phpriority_2025___5",
  "Local Priority: Health Access" = "phpriority_2025___6",
  "Local Priority: Health Inequities" = "phpriority_2025___7",
  "Solid Color" = "none"
)

priority_labels <- c(
  phpriority_2025___1 = "Chronic Disease Mgmt",
  phpriority_2025___2 = "Cancer Control/Screening",
  phpriority_2025___3 = "Womens Health",
  phpriority_2025___4 = "Pediatric Health",
  phpriority_2025___5 = "Adult Health",
  phpriority_2025___6 = "Health Access",
  phpriority_2025___7 = "Health Inequities"
)

# # accreditation
# Define the mapping vector (first set to second set)
# health_dept_mapping <- c(
#   "Barren River Health District" = "Barren River District Health Department",
#   "Buffalo Trace Health District" = "Buffalo Trace District Health Department",
#   "Bullitt County" = "Bullitt County Health Dept.",
#   "Christian County" = "Christian County Health Department",
#   "Franklin County" = "Franklin County Health Department",
#   "Green River Health District" = "Green River District Health Department",
#   "Jessamine County" = "Jessamine County Health Department",
#   "Fayette County" = "Lexington-Fayette County Health Department",
#   "Jefferson County" = "Louisville Metro Public Health and Wellness",
#   "Lake Cumberland Health District" = "Lake Cumberland District Health Department",
#   "Laurel County" = "Laurel County Health Department",
#   "Lincoln Trail Health District" = "Lincoln Trail District Health Department",
#   "Madison County" = "Madison County Health Department",
#   "Marshall County" = "Marshall County Health Department",
#   "Montgomery County" = "Montgomery County Health Department",
#   "Northern Kentucky Health District" = "Northern Kentucky Independent District Health Department",
#   "Three Rivers Health District" = "Three Rivers District Health Department",
#   "Wedco Health District" = "WEDCO District Health Department"
# )

# Update NAMELSAD10 in the shapefile
# Define the second set names that map to the first set (health departments)
# accredited_names <- c(
#   "Barren River Health District",
#   "Buffalo Trace Health District",
#   "Bullitt County",
#   "Christian County",
#   "Franklin County",
#   "Green River Health District",
#   "Jessamine County",
#   "Fayette County",
#   "Jefferson County",
#   "Lake Cumberland Health District",
#   "Laurel County",
#   "Lincoln Trail Health District",
#   "Madison County",
#   "Marshall County",
#   "Montgomery County",
#   "Northern Kentucky Health District",
#   "Three Rivers Health District",
#   "Wedco Health District"
# )

# # Create new variable is_accredited
# shapefile2025$is_accredited <- as.logical(shapefile2025$NAMELSAD10 %in% accredited_names)
# table(shapefile$is_accredited, useNA = "ifany")
# select(shapefile, NAMELSAD10, is_accredited)  %>% 
# filter(is_accredited) %>% 
# pull(NAMELSAD10) %>% 
# sort()

numberOfAccreditedLHDs <- sum(shapefile$is_accredited)
# accreditationPercent <- round(100 * numberOfAccreditedLHDs / nrow(shapefile), 1)
# numberOfAccreditedLHDs <- paste(numberOfAccreditedLHDs, " (", accreditationPercent, "%)", sep = "")
# extra -------------------------------------------------------------------


# fresh -------------------------------------------------------------------
# using a temporary file but use the path you want
# using a temporary file but use the path you want
# tmp <- file.path(tempdir(), "custom-theme.css")

# my_theme <- create_theme(
#   bs4dash_vars(
#     # Navbar colors
#     navbar_light_color = "#305E4C",  # Text color for the light navbar
#     navbar_light_active_color = "#76AB48", # Active link color for the light navbar
#     navbar_light_hover_color = "#76AB48",  # Hover link color for the light navbar
#     
#     # General body styles
#     body_bg = "#FFFFFF",  # Lighter body background color for light mode
#     text_color = "#0C3151", # Text color for body
#     
#     # Sidebar styles for light mode
#     sidebar_light_bg = "#F7F9FB",  # Lighter sidebar background color for light mode
#     sidebar_light_color = "#0C3151",  # Sidebar text color in light mode
#     sidebar_light_hover_color = "#76AB48",  # Sidebar hover text color in light mode
#     sidebar_light_active_color = "#76AB48",  # Sidebar active link color in light mode
#     
#     # Sidebar styles for dark mode
#     sidebar_dark_bg = "#0C3151",  # Darker sidebar background color for dark mode
#     sidebar_dark_color = "#FFFFFF",  # Sidebar text color in dark mode
#     sidebar_dark_hover_color = "#76AB48",  # Sidebar hover text color in dark mode
#     sidebar_dark_active_color = "#76AB48"  # Sidebar active link color in dark mode
#   ),
#   bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFFFFF",  # Text color for dark background
#     text_light = "#0C3151"  # Text color for light background
#   ),
#   bs4dash_layout(
#     main_bg = "#FFFFFF"  # Lighter main background for light mode
#   )
# )






# library(tigris)
# ky_counties <- counties(state = 'KY') %>%
#   sf::st_transform(crs = 'WGS84') %>%
#   mutate(across(NAME, ~ toupper(.x)))
# saveRDS(ky_counties, 'dat/ky_counties.rds')
# ky_counties <- readRDS('dat/ky_counties.rds')
# lnaDat <- read.csv('dat/jul82024-lna-online.csv')
# saveRDS(lnaDat, 'dat/lnaDat.rds')
# lnaDat <- readRDS('dat/lnaDat.rds')
# ky_counties_joined <- sp::merge(ky_counties, lnaDat, by.x = 'NAME', by.y = 'County') %>%
#  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
# save(ky_counties_joined, lnaDat, file = 'dat/ky_joined.RData')


# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = shapefile,
#               color = ~qpal(FID),  # Color by population attribute
#               weight = 1,
#               opacity = 1,
#               fillOpacity = 0.5,
#               popup = ~paste("Population: ", FID))

# serve_submissions1 <- vector('list', 61)
# 
# serve_submissions1 <- paste0('subs/', list.files('www/subs/'))
# # serve_submissions2 <- paste0('hubs/', list.files('www/hubs/')[1:30])
# 
# 
# serve_submissions1
# # sprintf("%02d", 1:61)
# 

# PoPuP <- sprintf(
#   paste0('<a href="%s" target="_blank">', titled, '</a>'),
#     X
# )
# PoPuP
# }
# 
# Ziggy('Download files')

##

# FOR MAP LOCATION MARKER DOWNLOADS
# Function to list files from multiple directories and remove "www" from paths
# list_files_from_directories <- function(dirs) {
#   tibble(dir = dirs) %>%
#     mutate(files = map(dir, function(d) {
#       files <- list.files(d, full.names = TRUE)
#       # Remove "www" from the directory path if it exists and avoid extra slashes
#       files <- gsub("^www/", "", files)  # Remove leading "www/" only
#       # Ensure no double slashes
#       files <- gsub("/+", "/", files)  # Collapse multiple slashes to a single slash
#       return(files)
#     }))
# }
# 
# # Example directories on your host machine
# dirs <- c('www/subs/', 'www/hubs/')
# 
# # Create a nested data frame of directories and their file contents
# nested_data <- list_files_from_directories(dirs)

