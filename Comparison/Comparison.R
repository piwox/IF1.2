#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
  titlePanel(""),
   
   # Sidebar with a slider input
   sidebarLayout(
      sidebarPanel(
         sliderInput("I",
                     "Number of asset steps",
                     min=30,
                     max=2000,
                     value=100),
         sliderInput("vol",
                     "Volatility",
                     min=0,
                     max=0.7,
                     value=sigma_g*sqrt(n_days_g)),
         # sliderInput("bar",
         #             "Barrier offset",
         #             min=0,
         #             max=1000,
         #             value=0),
         width = 3
      ),
      # Show a plot
      mainPanel(
         plotlyOutput("surface"),width = 9
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$surface <- renderPlotly({
      dS = 3 * strike_g / input$I
      S = input$I:0 * dS
      dt <- 0.45 * (1 + (input$type == 'call'))/(input$vol/sqrt(n_days_g))^2/input$I^2
      T_    <- floor(n_days_g/dt) + 1
      dt    <- n_days_g/T_
   
      # if(input$bar == 0)
      # {
         Error <- cpp_FD(I=input$I, sigma = input$vol/sqrt(n_days_g))[,1] -
            BSPrice(sigma = input$vol/sqrt(n_days_g),S=seq(7200,0,by=-dS))
         
      # }
      # else
      # {
      #    Error <- cpp_FD_Bar(I=input$I,type=input$type, sigma = input$vol/sqrt(n_days_g))[,1] -
      #       BSPrice(type = input$type, sigma = input$vol/sqrt(n_days_g),S=seq(7200,0,by=-dS))
      #    p <- plot_ly(x=~ S, width = 800, height = 600) %>% add_lines(y= ~Error, color = "darkblue", lwd = 1.5)
      # }
      p <- plot_ly(x=~ S, width = 1100, height = 800, colors = "Blues") %>% add_lines(y= ~Error)
      p %>% layout(margin = list(l=130,r=50,b=100,t=100,pad=4),yaxis=list(range = c(-0.2,0.2)), title = "Error at t=0")
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

