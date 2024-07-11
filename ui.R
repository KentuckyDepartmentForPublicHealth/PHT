
ui <- tagList(
  useShinyjs(),
  page_navbar(
  title = 'Public Health Transformation (PHT) Dashboard',
theme = bs_theme(5, 'cosmo'),
header = 'Content header',
footer = 'Content footer',

inlineCSS('.yy {padding: 0; margin 0; color: red; text-align: center;}'),

nav_panel(
    title = 'One',
    p('1 content')
  ),
nav_panel(
  title = 'Two',
  p('2 content')
),
nav_panel(
  title = 'Three',
  p('3 content')
),
nav_menu(
  title = "Links",
  align = "right",
  nav_item(link_shiny),
  nav_item(link_posit)
),
nav_spacer(),
nav_menu(
  'Dark/Light Mode',
  icon = bs_icon('toggle-on'),
    nav_item(
      class = 'yy',
      input_dark_mode(mode = 'light')
    )
),
  sidebar = sidebar(

    selectInput("var", "Select variable", choices = mtcars |> names())
  )
)
)