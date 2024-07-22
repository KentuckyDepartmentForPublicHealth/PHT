
ui <- tagList(
  useShinyjs(),
  page_navbar(
  title = 'Public Health Transformation (PHT) Dashboard',
  
  theme = bs_theme(5, 'cosmo'),
  header = NULL,
  footer = 'Last update: July 25, 2024 at 11:15 AM EST',
  
inlineCSS('
  .yy {padding: 0; margin 0; color: red; text-align: center;}
  .leaflet .info {font-size: 2em;}      
          '),
  nav_panel(
    title = 'Map',
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
)