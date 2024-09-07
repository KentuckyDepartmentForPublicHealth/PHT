
ui <- page_navbar(
  title = span(img(src = "DPH_and_PHAB_logo-removebg-preview.png", height = "55px"),  # Adjust height as needed
    "Public Health Transformation (PHT) Dashboard",
    style = 'vertical-align: middle;'
  ),
  # title = 'Public Health Transformation (PHT) Dashboard',
  theme = bs_theme(5, 'cerulean', base_font = font_google("Inter")),
  header = NULL,
  footer = paste('Last updated:', currentDate),

  nav_spacer(),

  nav_panel(
    title = 'Map', icon = icon('map'),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PHT.css")
    ),
    h1('Interactive Map'),
    h4('Kentucky Health Departments'),
    navset_card_tab(
      height = 1000,
      full_screen = TRUE,
      title = NULL,
      nav_panel(
        "Main",
        card_title("Click on the location marker for more information"),
        leafletOutput('map')
      ),
      nav_panel(
        "Extra",
        card_title("A leaflet plot")
        #leaflet_widget
      ),
      nav_panel(
        shiny::icon("circle-info"),
        markdown(HTML('<p>Contact the dashboard <a href="mailto:adam.berrones@ky.gov">developer</a>'))
      )
    )
    

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