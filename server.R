

server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    mtcars |>
    ggplot(aes(!!sym(input$var), !!sym(input$var2))) +
      geom_point(size = 5) +
      theme_classic(base_size = 18) +
      theme(
        axis.text = element_text(size = 18)
      )
  })
}
