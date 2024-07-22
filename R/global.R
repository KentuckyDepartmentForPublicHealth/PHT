

# libs --------------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(thematic)
library(ggplot2)
library(magrittr)
library(dplyr)
library(leaflet)
library(sf)

# deploy ------------------------------------------------------------------

# rsconnect::deployApp(
#   account = 'kdph',
#   appName = 'pht-dashboard',
#   appVisibility = 'private',
#   logLevel = 'verbose'
# )


# boilerplate -------------------------------------------------------------


thematic_shiny(font = "auto")

load(file = 'dat/ky_joined.RData')

ky_counties_joined$Compliance.Status<- ifelse(ky_counties_joined$Compliance.Status == 1, 'Yes', 'No') 

qpal_cases <- colorFactor(palette = c('red', 'green'), domain = ky_counties_joined$Compliance.Status)


link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)
link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
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

