
ui <- page_navbar(
  title = span(img(src = "DPH_and_PHAB_logo-removebg-preview.png", height = "75px"),  # Adjust height as needed
               "Public Health Transformation (PHT) Dashboard",
               style = 'vertical-align: middle;'
  ),
  # title = 'Public Health Transformation (PHT) Dashboard',

# theme -------------------------------------------------------------------

  
  theme = bs_theme(5, bootswatch = 'cyborg',secondary=chfs$cols9[9],primary=chfs$cols9[1],
                   
                   base_font = font_google("Montserrat", local = TRUE)),# base_font = font_google("Inter")),
  # theme = bs_theme(
  #   # Controls the default grayscale palette
  #   bg = "#e3e4e4", fg = "black",
  #   # Controls the accent (e.g., hyperlink, button, etc) colors
  #   primary = "#EA80FC", secondary = "#48DAC6",
  #   base_font = c("Grandstander", "sans-serif"),
  #   code_font = c("Courier", "monospace"),
  #   heading_font = "'Helvetica Neue', Helvetica, sans-serif",
  #   # Can also add lower-level customization
  #   "input-border-color" = "#EA80FC"
  # ),
  # use_theme(my_theme),


# header/footer -----------------------------------------------------------


  header = NULL,
  footer = paste('Last updated:', currentDate),
  
  nav_spacer(),
  
  nav_panel(
    title = 'Home', icon = icon('house'),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PHT.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
        h2('Kentucky Local Health Departments'),
    HTML("<p>This site hosts Kentucky Local Health Departments' <span style=\"color: white; font-weight: bold;\"> most recently completed Local Needs Assessment</span> (also called Community Health Assessment [CHA], Community Health Improvement Plan [CHIP], Community Health Needs Assessment [CHNA]) for their jurisdiction served as <a href=\"KentuckyLaw.pdf\" target=\"_blank\">required by Kentucky law.</a></p>"),
    

# navset_card_tab ---------------------------------------------------------

    
    navset_card_tab(
      height = 1000,
      full_screen = TRUE,
      title = NULL,

# map ---------------------------------------------------------------------

      
      nav_panel(
        title = tagList(icon("map"), "Map"),
    h3('Interactive Map Explorer'),
        card_title("Click location markers for downloadable information"),
        # p('Zoom in ( + )'),
        # p('Zoom out (', HTML('&ndash;'), ')'),
        div(
          class = "leaflet-wrapper",  # Wrapping in a div for additional control
          withSpinner(leafletOutput('map', height = '800px'), type = 3, color.background = 'black')  # Setting explicit height
        )
      ),


# downloads ---------------------------------------------------------------


      nav_panel(
        title = tagList(icon("cloud-arrow-down"), "Downloads"),
        h3('Directly download available files'),
        card_title("Same data as found within the map location markers"),
        sidebarLayout(
          sidebarPanel(
            # selectInput("directory", "Choose a Directory:", choices = unique(nested_data$dir))
            selectInput("bydirectory", "Choose LHD", choices = shapefile$NAMELSAD10 %>% sort(), selected = 'Allen County'),
            actionButton('searchdownloads', 'Search'),
            actionButton('resetdownloads', 'Reset')
          ),
          mainPanel(
            tableOutput("file_table")
          )
        )
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
    nav_item(link_khda),
    nav_item(link_naccho)
  ),
  # nav_item(
  #   input_dark_mode(id = "dark_mode", mode = "dark") #
  # ),
  # nav_item(
  #   tags$a('KDPH', title = 'Kentucky Department for Public Health', href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')
  # ),
  

# sidebar -----------------------------------------------------------------

  
  sidebar = sidebar(
    h4('Toolbar'),
    actionButton('resetMap', 'Reset Map', icon=icon('rotate')),
    hr(),
    radioButtons('labelthemap', 
                 strong('Map Label Font Size'),
                 choices = c('No Labels' = 'nolabels', 'Small' = '8px', 'Medium' = '12px', 'Large' = '14px', 'X-Large' = '18px'),
                 selected = '12px'
    ),
    hr(),
    input_switch('showmarkers', 'Show location markers?', value = T),
    input_switch('showpopup', 'Show hover info?', value = T),
    hr(),
    tags$blockquote(a('Powered by the Kentucky Department for Public Health',  href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')),
    
  )
)