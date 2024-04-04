mybooty <- function(x, fun){
  xs <- sample(x = x,size = length(x),replace = TRUE)
  do.call(what = fun, list(x = xs)) # How does this work?
}

ddt <- read.csv("DDT.csv")

library(dplyr)

df <- ddt %>% filter(SPECIES == "SMBUFFALO")
L <- df$LENGTH

library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("Bootstrapping"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("iter",
                  "Number of Iterations:",
                  min = 100,
                  max = 10000,
                  value = 500,
                  step = 100),

      sliderInput("alpha",
                  "Alpha Value:",
                  min = 0,
                  max = 0.2,
                  value = 0.05,
                  step = 0.01),

      selectInput(inputId = "fun",
                  label = "Function to Calculate Stats",
                  choices = c("mean","var","sd", "IQR") )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("bootPlot"),
      textOutput("ci")

    )
  )
)

server <- function(input, output) {

  fun <- reactive(input$fun)
  iter <- reactive(input$iter)
  alpha <- reactive(input$alpha)

  v <- reactive({
    xs <- replicate(iter(), expr = mybooty(L, fun()) )
    ci <- quantile(xs, c(alpha()/2, 1-alpha()/2))
    list(xs = xs,ci = ci)
  })

  output$bootPlot <- renderPlot({

    h <- hist(v()$xs, freq = FALSE, plot = FALSE, warn.unused = FALSE)
    hcol <- h$density/max(h$density)
    hist(v()$xs,
         xlab = "Length",
         ylab = "Density",
         main = paste0("SMBUFFALO lengths",", iterations=", iter()),
         col = rgb(hcol, hcol^2, 0))
  },res = 96)

  output$ci <- renderText({
    v()$ci
  })
}

# Run the application
shinyApp(ui = ui, server = server)
