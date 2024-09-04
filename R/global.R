
# note: percent LHD compliant with 902KAR8.160
# 2021: 40%
# 2022: 53%
# 2023: 72%
# -> valueBox(es)

# libs --------------------------------------------------------------------

library(shiny)
library(bslib)
# library(bsicons)
library(thematic)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(sf)
library(leaflet.extras)

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
# pal <- colorNumeric(palette = "viridis", domain = shapefile$FID)
qpal <- colorFactor(palette = c('red', 'green'), domain = shapefile$Compliance.Status)

# leaflet() %>%
#   addTiles() %>%
#   addPolygons(data = shapefile,
#               color = ~qpal(FID),  # Color by population attribute
#               weight = 1,
#               opacity = 1,
#               fillOpacity = 0.5,
#               popup = ~paste("Population: ", FID))
