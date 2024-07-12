

# libs --------------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(shinyjs)
library(thematic)
library(ggplot2)


# deploy ------------------------------------------------------------------

# rsconnect::deployApp(
#   account = 'kdph',
#   appName = 'pht-dashboard',
#   appVisibility = 'private',
#   logLevel = 'verbose'
# )


# boilerplate -------------------------------------------------------------


thematic_shiny(font = "auto")


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


