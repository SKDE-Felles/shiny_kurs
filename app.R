
library(shiny)
load("data/testdata2.rda")


ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "bohf", 
                  label = "Boområde HF", 
                  choices = unique(x = testdata2$boomr_HF)
      ),
      selectInput(inputId = "bosh", 
                  label = "Boomr. sykehus", 
                  choices = unique(x = utvalg$boomr_sykehus)
      )
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      
      plotOutput("distPlot2")
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  bo_type <- shiny::reactive({
      return(input$bohf)
    })
  
  observe(
    boomr <- bo_type()
    
  )
  
  output$distPlot <- renderPlot({
    # Filtrere datasettet på bohf, valgt av bruker
    utvalg <- dplyr::filter(testdata2, boomr_HF == input$bohf)
    utvalg2 <- dplyr::filter(utvalg, boomr_sykehus == input$bosh)
    tmp <- tapply(utvalg2$kontakter, utvalg2$behandlende_sykehus, sum)
    barplot(height = tmp, las = 1, horiz = T)
    # x = table(utvalg$behandlende_sykehus), y = utvalg$kontakter)
    
    # draw the histogram with the specified number of bins
    #      hist(x, col = 'darkgray', border = 'white')
  })
  output$distPlot2 <- renderPlot({
    # Filtrere datasettet på bohf, valgt av bruker
    utvalg <- dplyr::filter(testdata2, boomr_HF == input$bohf)
    utvalg2 <- dplyr::filter(utvalg, boomr_sykehus == input$bosh)
    tmp <- tapply(utvalg$kontakter, utvalg$behandlende_sykehus, sum)
    barplot(height = tmp, las = 1, horiz = T)
    # x = table(utvalg$behandlende_sykehus), y = utvalg$kontakter)
    
    # draw the histogram with the specified number of bins
    #      hist(x, col = 'darkgray', border = 'white')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

