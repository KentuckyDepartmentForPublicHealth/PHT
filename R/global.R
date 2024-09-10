
# note: percent LHD compliant with 902KAR8.160
# 2021: 40%
# 2022: 53%
# 2023: 72%
# -> valueBox(es)

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
library(purrr)
library(leaflet)
library(sf)
library(leaflet.extras)
library(fresh)
library(shinycssloaders)


# fresh -------------------------------------------------------------------

# my_theme <- create_theme(
#   bs4dash_vars(
#     navbar_light_color = "#FFFFFF",   # Text color
#     navbar_light_active_color = "#FFD700", # Active link color (gold)
#     navbar_light_hover_color = "#FFD700"  # Hover link color (gold)
#   ),
#   bs4dash_yiq(
#     contrasted_threshold = 10,
#     text_dark = "#FFFFFF",  # Text color for dark background
#     text_light = "#004080"  # Text color for light background
#   ),
#   bs4dash_layout(
#     main_bg = "#004080"  # Navbar background color
#   )
# )


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

# deploy ------------------------------------------------------------------

# currentDate <- format(Sys.time(), '%a, %b %d, %Y at %I:%M %p EDT')
# saveRDS(currentDate, file = 'dat/currentDate.rds')
currentDate <- readRDS(file = 'dat/currentDate.rds')

# rsconnect::deployApp(account = 'kdph', appName = 'pht-dashboard', appVisibility = 'private', logLevel = 'verbose')


# boilerplate -------------------------------------------------------------



# load(file = 'dat/ky_joined.RData')

# ky_counties_joined$Compliance.Status<- ifelse(ky_counties_joined$Compliance.Status == 1, 'Yes', 'No')

# qpal_cases <- colorFactor(palette = c('red', 'green'), domain = ky_counties_joined$Compliance.Status)


link_shiny <- tags$a(
  shiny::icon("building"), "Kentucky Health Departments Association (KHDA)",
  href = "https://khda-ky.org/",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("thumbtack"), "NACCHO Directory of Local Health Departments",
  href = "https://www.naccho.org/membership/lhd-directory?searchType=standard&lhd-state=KY#card-filter",
  target = "_blank"
)


# leaflet -----------------------------------------------------------------


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

# LHD

shapefile <- st_read('dat/temp/ky-lhd.shp')
shapefile <- st_transform(shapefile, crs = 4326)
centroids <- st_centroid(shapefile)
centroid_coords <- st_coordinates(centroids)
shapefile$Compliance.Status <- sample(c('Yes','No','Yes','Yes','Yes','Yes'), 61, replace = T)
load(file = 'dat/select2cols.RData') # add Listing
shapefile <- merge(shapefile, y, by = 'FID')

# pal <- colorNumeric(palette = "viridis", domain = shapefile$FID)
# qpal <- colorFactor(palette = c('#004080', '#001F3F'), domain = shapefile$Compliance.Status)
qpal <- colorFactor(palette = c('gray', chfs$cols9[9]), domain = shapefile$Compliance.Status)


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
serve_submissions1 <- paste0('subs/', list.files('www/subs/'))
# # serve_submissions2 <- paste0('hubs/', list.files('www/hubs/')[1:30])
# 
# 
# serve_submissions1
# # sprintf("%02d", 1:61)
# 
# dat <- shapefile %>%
#   mutate(
#     res1 = sprintf("%02d", FID)
#   )
# 
# Ziggy <- function(titled, X =serve_submissions1) {
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
# Function to list files from multiple directories and remove "www" from paths
list_files_from_directories <- function(dirs) {
  tibble(dir = dirs) %>%
    mutate(files = map(dir, function(d) {
      files <- list.files(d, full.names = TRUE)
      # Remove "www" from the directory path if it exists and avoid extra slashes
      files <- gsub("^www/", "", files)  # Remove leading "www/" only
      # Ensure no double slashes
      files <- gsub("/+", "/", files)  # Collapse multiple slashes to a single slash
      return(files)
    }))
}

# Example directories on your host machine
dirs <- c('www/subs/', 'www/hubs/')

# Create a nested data frame of directories and their file contents
nested_data <- list_files_from_directories(dirs)

# y <- readxl::read_xlsx('dat/select2cols.xlsx') %>% 
  # dplyr::select(FID, Listing)

# save(y, file = 'dat/select2cols.RData')


####
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

