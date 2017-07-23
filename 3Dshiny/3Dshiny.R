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
  # titlePanel("Option Value Surfaces"),
   
   # Sidebar with a slider input
   sidebarLayout(
      sidebarPanel(
         radioButtons("type",
                     "Type",
                     c("call","put"),
                     inline = TRUE),
         radioButtons("style",
                      "Style",
                      c("European","American"),
                      inline = FALSE),
         checkboxInput("uv",
                       "Uncertain volatility",
                       FALSE),
         sliderInput("uvol",
                     "Volatility range",
                     min = 0,
                     max=0.7,
                     value=c(0.2,0.3)),
         checkboxInput("gamma",
                       "Gamma sign",
                       FALSE),
         checkboxInput("stop",
                       "Stopping time",
                       FALSE),
         sliderInput("I",
                     "Number of asset steps",
                     min=50,
                     max=1000,
                     value=100),
         sliderInput("div",
                     "Dividend",
                     min = 0,
                     max = 1000,
                     value = 200),
         sliderInput("vol",
                     "Volatility",
                     min = 0,
                     max = 0.7,
                     value = sigma_g*sqrt(n_days_g)),
         sliderInput("bar",
                     "Barrier offset",
                     min=0,
                     max=2400,
                     value=200),width = 3
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
      dS = (ifelse(input$type == "call", 1, 2) * 2400 + input$bar) / input$I
      S = input$I:0 * dS
      if (input$type == 'put') 
         S <- S + 2400 - input$bar
      dt    <- 0.9/(1+(input$uv==TRUE))/(1+ (input$type == "put"))/(input$vol/sqrt(n_days_g))^2 /input$I^2
      T_    <- floor(n_days_g/dt) + 1
      dt    <- n_days_g/T_
      t <- 0:T_ * dt
      print(dt)
      if(input$uv == FALSE)
      {
      if(input$style == "European")
         Value <- cpp_FD_Bar_Div(I=input$I,type=input$type,div=input$div, sigma = input$vol/sqrt(n_days_g), barrier_spread = input$bar)
      else
         Value <- cpp_FD_Bar_American_Div(I=input$I,type=input$type,div=input$div, sigma = input$vol/sqrt(n_days_g), barrier_spread = input$bar)
      
      }
      else
      {
         if(input$style == "European")
            Value <- cpp_FD_Bar_UV_Div(I=input$I,type=input$type,div=input$div, sigma_min = input$uvol[1]/sqrt(n_days_g),
                                       sigma_max = input$uvol[2]/sqrt(n_days_g), barrier_spread = input$bar)
         else
            Value <- cpp_FD_Bar_American_UV_Div(I=input$I,type=input$type,div=input$div, sigma_min = input$uvol[1]/sqrt(n_days_g),
                                                sigma_max = input$uvol[2]/sqrt(n_days_g), barrier_spread = input$bar)
         
      }
      light = list(roughness = 0.6, specular=0.22, diffuse=0.8,ambient=1,fresnel = 0.3)
      p <- plot_ly(z =~Value, y=~ S, x =~ t, mode = 'surface', width = 1000, height = 900,
                   lighting = light) %>% add_surface()
      if(input$gamma == TRUE)
      {
         gamma <- GammaSign(Value)-25
         p <- plot_ly(z =~Value, y=~ S, x =~ t, lighting = light, width = 1000, height = 900) %>% add_surface(opacity=ifelse(input$gamma == TRUE, 0.98,1)) %>% add_surface(z= ~gamma, opacity =1, showscale=FALSE)
      }
      if(input$stop == TRUE)
      {
         stop <- StoppingTime(Value)-25
         p <- plot_ly(z =~Value, y=~ S, x =~ t, lighting = light, width = 1000, height = 900) %>% add_surface(opacity=ifelse(input$stop == TRUE, 0.98,1)) %>% add_surface(z= ~stop, opacity =1, showscale=FALSE)
      }
      p %>% layout(plot_bgcolor="#444",paper_bgcolor="#111",margin = list(l=150,r=50,b=100,t=100,pad=4), title = "Option value surface", hovermode='closest')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

