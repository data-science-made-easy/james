library(shiny)
library(shinyglide)

ui <- fixedPage(
  glide(
    screen(
      p("This is a very simple shinyglide application."),
      p("Please click on Next to go to the next screen.")
    ),
    screen(
      p("Please choose a value."),
      numericInput("n", "n", value = 10, min = 10)
    ),
    screen(
      p("And here is the result."),
      plotOutput("plot")
    )
  )
)


server <- function(input, output, session) {

  output$plot <- renderPlot({
    hist(
      rnorm(input$n),
      main = paste("n =", input$n),
      xlab = ""
    )
  })

}

shinyApp(ui, server)