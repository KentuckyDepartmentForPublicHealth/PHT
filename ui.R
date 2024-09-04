
ui <- page_navbar(
  title = span(img(src = "DPH_and_PHAB_logo-removebg-preview.png", height = "55px"),  # Adjust height as needed
    "Public Health Transformation (PHT) Dashboard",
    style = 'vertical-align: middle;'
  ),
  # title = 'Public Health Transformation (PHT) Dashboard',
  theme = bs_theme(5, 'cerulean', base_font = font_google("Roboto")),
  header = NULL, #HTML('<a href = "https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx" target="_blank"><img src="dph.png" width = "20%"></a>'),
  footer = paste('Last updated:', currentDate),

  nav_spacer(),

  nav_panel(
    title = 'Map', icon = icon('map'),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PHT.css")
    ),
    h1('Interactive map tool'),
    h4('Obtain information about each local health department/district'),
    leafletOutput('map'),


  ),
  # nav_panel(
  #   title = 'Charts', icon = icon('chart-column'),
  # layout_columns(  
  #   plotOutput('plot'),
  #   plotOutput('plot2'),
  #   col_widths = c(6, 6)
  #   )
  # ),
  nav_menu(
    title = "Links", icon = icon('link'),
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "dark") #
  ),
  nav_item(
    tags$a('KDPH', title = 'Kentucky Department for Public Health', href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')
  ),
  sidebar = sidebar(
    selectInput("var", "Select variable",  choices = mtcars |> names()),
    hr(),
    selectInput("var2", "Select variable", choices = mtcars |> names()),
    hr(),
    tags$blockquote(a('Powered by the Kentucky Department for Public Health',  href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')),
    
  )
)