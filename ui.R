
ui <- page_navbar(
  title = 'Public Health Transformation (PHT) Dashboard',
  theme = bs_theme(5, 'cosmo'),
  header = NULL,
  footer = currentDate,

  nav_panel(
    title = 'Map',
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PHT.css")
    ),
    leafletOutput('map')

  ),
  nav_panel(
    title = 'Charts',
  layout_columns(  
    plotOutput('plot'),
    plotOutput('plot2'),
    col_widths = c(6, 6)
    )
  ),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  ),
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "dark") #
  ),
  nav_item(
    tags$a('KDPH', href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')
  ),
  sidebar = sidebar(
    selectInput("var", "Select variable", choices = mtcars |> names()),
    hr(),
    selectInput("var2", "Select variable", choices = mtcars |> names())
  )
)