
ui <- page_navbar(id = 'navBar',
  title = span(img(src = "DPH_and_PHAB_logo-removebg-preview.png", alt = "Kentucky Department for Public Health logo; and Public Health Accreditation Board Accredited Health logo", height = "75px"),  # Adjust height as needed
               "Public Health Transformation (PHT) Dashboard",
               style = 'vertical-align: middle;'
  ),
  # title = 'Public Health Transformation (PHT) Dashboard',

# theme -------------------------------------------------------------------

# base_font = font_google("Montserrat", local = TRUE)),# base_font = font_google("Inter")),
  theme = bs_theme(
    # Controls the default grayscale palette
    # bg = "#e3e4e4", fg = "black",
    # bg = "red", fg = "black",
    # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = chfs$cols2[2], secondary = chfs$cols9[5],
    base_font = c("Grandstander", "sans-serif"),
    code_font = c("Courier", "monospace"),
    heading_font = "'Helvetica Neue', Helvetica, sans-serif",
    # Can also add lower-level customization
    "input-border-color" = chfs$cols9[4]
  ),
 # theme = bs_theme(5, 'default'),


# header/footer -----------------------------------------------------------


  header = NULL,
footer = NULL,
  
  nav_spacer(),

# home --------------------------------------------------------------------

  
  nav_panel(
    title = 'Home', icon = icon('house'),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "PHT.css"),
      tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
        h2('Kentucky Local Health Departments'),
    HTML("<p>This site hosts Kentucky Local Health Departments' <span style=\"font-weight: bold;\"> most recently submitted Local Needs Assessment (LNA)</span>, also called Community Health Assessment (CHA), Community Health Improvement Plan (CHIP) or Community Health Needs Assessment (CHNA), for their jurisdiction served as <a href=\"doc/KentuckyLaw.pdf\" target=\"_blank\">required by Kentucky law.</a></p>"),
    

# navset_card_tab ---------------------------------------------------------

    # 
    # navset_card_tab(
    #   height = 1000,
    #   full_screen = TRUE,
    #   title = NULL,

# map ---------------------------------------------------------------------
        h4(span(icon('map'), 'Interactive Map Explorer', style = paste0('color:', chfs$cols9[1]))),
        span("Click location markers for downloadable information", style = 'text-align: left;'),
       span(paste('Last updated:', currentDate), style = 'font-size: .75em; text-align: right;'),
        # p('Zoom in ( + )'),
        # p('Zoom out (', HTML('&ndash;'), ')'),
        div(
          class = "leaflet-wrapper",  # Wrapping in a div for additional control
          withSpinner(leafletOutput('map', height = '850px'), type = 3, color.background = 'black', color = chfs$cols9[1])  # Setting explicit height
        )

      # nav_panel(
      #   shiny::icon("circle-info"),
      #   markdown(HTML('<p>Contact the dashboard <a href="mailto:adam.berrones@ky.gov">developer</a>'))
      # )
    # )
    
# downloads ---------------------------------------------------------------
    
  ),
  nav_panel(
    title = 'Downloads', icon = icon('cloud-arrow-down'),
    h2('Kentucky Local Health Departments'),
    h4(span(icon('file-pdf'), 'Directly download available files', style = paste0('color:', chfs$cols9[1]))),
    "Same data as found within the map location markers",
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


# stats -------------------------------------------------------------------

nav_panel(
  title = "Stats", icon = icon('chart-bar'),
  align = "right",
  div(
    style = "margin-bottom: 30px;",  # Adds space between the value box and the footer
    uiOutput("my_value_box")
  )
),

# nav_panel(
#   title = "About", icon = icon('barcode'),
#   h3('Directly download available files'),
#   card_title("Same data as found within the map location markers")
# ),

# resources ---------------------------------------------------------------
  nav_menu(
    title = "Resources", icon = icon('link'),
    align = "right",
    nav_item(link_khda),
    nav_item(link_naccho),
    nav_item(link_lhdmap),
    # br(),
    nav_item(link_sha2023),
    nav_item(link_sha2023_exec),
    nav_item(link_ship2024),
    nav_item(link_ship2024_exec),
    # br(),
    nav_item(link_pht_flyer),
    nav_item(link_pht_source),
  ),



# mode toggle -------------------------------------------------------------


  nav_item(
    input_dark_mode(id = "mode_toggle", mode = "light") #
  ),
  # nav_item(
  #   tags$a('KDPH', title = 'Kentucky Department for Public Health', href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')
  # ),
  

# sidebar -----------------------------------------------------------------

  
  sidebar = sidebar(id = 'mySidebar',
    h4('Map Toolbar'),
    actionButton('resetMap', 'Reset Map', icon=icon('rotate')),
    selectInput('whichcounty', strong('County Zoom'), choices = c('All', ky_counties), selected = 'All'),
    br(),
    # input_switch('showallcounties', 'Show all counties?', value = T),
    # hr(),
    wellPanel(
    radioButtons('labelthemap', 
                 strong('Label Size'),
                 choices = c('No Labels' = 'nolabels', 'Small' = '8px', 'Medium' = '12px', 'Large' = '14px', 'X-Large' = '18px'),
                 selected = '12px'
    ),
    tags$strong('Customize'),
    input_switch('showmarkers', 'Show location markers?', value = T),
    input_switch('showpopup', 'Show hover info?', value = T)
    ),
    hr(),
    
    tags$blockquote(a('Powered by the Kentucky Department for Public Health',  href = 'https://www.chfs.ky.gov/agencies/dph/Pages/default.aspx', target = '_blank')),
    
  )
)