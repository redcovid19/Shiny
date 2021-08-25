#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI para aplicação que constrói um histograma
ui <- fluidPage(

    # Título aplicação
    titlePanel("Disitribuição Normal - Histograma Interativo"),

    # Sidebar com um slider para o número de intervalos 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Intervalos",
                        "Número de intervalos:",
                        min = 1,
                        max = 100,
                        value = 30),
            
            sliderInput("tamanho", "Tamanho da Amostra", 1, 50000, 1000),
            sliderInput("variancia", "Variância", 1, 10, 5),
            numericInput("media", "Média", 0),
            
            ),
        
        # Mostra um plot da distribuição gerada
        mainPanel(
           plotOutput("distNormal")
        )
    )
)

       
# Define lógica requerida para construir o histograma
server <- function(input, output) {

    output$distNormal <- renderPlot({
        # Gera os intervalos baseado em input$Intervalos de ui.R
        x    <- rnorm(n = input$tamanho, mean = input$media, sd = sqrt(input$variancia))
        bins <- seq(min(x), max(x), length.out = input$Intervalos + 1)

        # Constrói o histograma com o número especificado de intervalos
        hist(x, breaks = bins, col = "blue", border = "pink", 
             main = "Histograma da Normal gerada")
    })
}

# Gera a aplicação 
shinyApp(ui = ui, server = server)
