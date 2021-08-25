library(shiny)
library(shinydashboard)
library(plotly)

ui <- dashboardPage(skin = "red",
                dashboardHeader(title = "Inferência Estatística"),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem("Início", tabName = "home", icon = icon("home")),
                    menuItem("Intervalos de Confiança", tabName = "ic", icon = icon("arrow-right")),
                    menuItem("Consistência dos Estimadores", tabName = "con", icon = icon("arrow-right")),
                    menuItem("Vício dos Estimadores", tabName = "vicio", icon = icon("arrow-right")),
                    menuItem("Erro Quadrático Médio", tabName = "eqm", icon = icon("arrow-right"))
                  )),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "home",
                            h1("Inferência Estatística - Aplicativo Web"),
                            h4("Aluno: Gabriel Machado de Oliveira Assis"),
                            h4("Professora: Renata Souza Bueno"),
                            h2(""),
                            h4("Este aplicativo em Shiny foi desenvolvido como atividade para a Monitoria de
                    Inferência Estatística I na Escola Nacional de Ciências Estatísticas (ENCE)
                    no primeiro semestre de 2019."),
                            h2(""),
                            h4("A atividade consistiu no aprendizado do pacote Shiny para a linguagem de 
                   programação R e no desenvolvimento de um aplicativo web sobre inferência
                   estatística."),
                            h4("O aplicativo consiste em quatro seções, sendo estas as seguintes"),
                            h2(""),
                            h2("1. Intervalos de Confiança:"),
                            h2(""),
                            h4("Nesta seção, temos os gráficos e os cálculos de intervalos para 4 tipos de
                   estimação intervalar diferentes. O usuário pode inserir um valor para o parâmetro
                   alvo, o tamanho da amostra e o nível de confiança, e receber o intervalo que seria
                   obtido caso tivesse apenas informações sobre aquela amostra."),
                            h2(""),
                            h2("2. Consistência dos Estimadores:"),
                            h2(""),
                            h4("Já nesta seção, temos um exemplo do comportamento de um estimador consistente quando
                   se aumenta o número de observações da amostra."),
                            h2(""),
                            h2("3. Vício dos Estimadores:"),
                            h2(""),
                            h4("Para exemplificar o vício de estimadores, temos nesta seção, um gráfico contendo 
                    a distribuição de um estimador viciado e a distribuição de um estimador não viciado
                    e podemos observar os seus comportamentos em relação ao parâmetro alvo."),
                            h2(""),
                            h2("4. Erro Quadrático Médio (EQM):"),
                            h2(""),
                            h4("Aqui, temos o gráfico dos EQMs de dois estimadores diferentes para o mesmo parâmetro
                    alvo, e o podemos observar a posição relativa entre as duas curvas."),
                            h4("Para desenvolvimento da aplicação e construção de gráficos foram usados os
                   seguintes pacotes do R:"),
                            uiOutput("pacotes")
                    ),
                    tabItem(tabName = "ic",
                            h1("Intervalos de Confiança"),
                            h4("Um intervalo de confiança é um tipo de estimativa intervalar para um parâmetro
                   populacional desconhecido. Este intervalo é obtido através das observações de
                   uma amostra da população e incluirá o verdadeiro valor do parâmetro de acordo
                   com um determinado nível de confiança."),
                            h4("O método para a construção de um intervalo de confiança é denominado 'Método
                   da Quantidade Pivotal' e consiste na obtenção de uma Quantidade Pivotal, isto
                   é, uma estatística amostral que seja função do parâmetro de interesse e cuja
                   distribuição não dependa deste parâmetro."),
                            h4("Alguns dos principais tipos de intervalo são:"),
                            h2(""),
                            h3("1. Intervalo para média de uma distribuição normal com variância conhecida:"),
                            h4("A quantidade pivotal e o intervalo com nível de confiança γ usados para esta
                   estimação são:"),
                            withMathJax(paste0("$$\\operatorname{Q}(X, \\mu) = \\frac{\\bar{X} - \\mu}{\\frac{\\sigma}{\\sqrt{n}}} \\sim
                                   \\operatorname{N}(0,1) \\quad \\text{e} \\quad 
                                   \\operatorname{IC}[\\gamma ; \\mu] = \\left[\\bar{X} - 
                                   \\frac{z\\sigma}{\\sqrt{n}} ; \\bar{X} + 
                                   \\frac{z\\sigma}{\\sqrt{n}}\\right]$$")),
                            h4("Sendo z o quantil da distribuição Normal Padrão e supondo que a amostra obtida veio
                    de uma população normal com média μ e variância σ², temos:"),
                            fluidRow(
                              box(plotlyOutput("graf1")),
                              tabBox(id = "val1",
                                     tabPanel("Média", numericInput("mu1", "Média (Parâmetro a ser estimado)", 0)),
                                     tabPanel("Variância", sliderInput("sigma21", "Variância", 1, 10, 1)),
                                     tabPanel("Tamanho", sliderInput("ams1", "Tamanho da Amostra", 1, 1000, 1000)),
                                     tabPanel("Nível", sliderInput("nv1", "Nível de Confiança", 0, 1, 0.95))),
                              box(verbatimTextOutput("ic1"))
                            ),
                            h2(""),
                            h3("2. Intervalo para média de uma distribuição normal com variância desconhecida:"),
                            h4("A quantidade pivotal e o intervalo com nível de confiança γ usados para esta
                   estimação são:"),
                            withMathJax(paste0("$$\\operatorname{Q}(X, \\mu) = \\frac{\\bar{X} - \\mu}{\\frac{S}{\\sqrt{n}}} \\sim
                                   \\operatorname{t-student}_{n-1} \\quad \\text{e} \\quad 
                                   \\operatorname{IC}[\\gamma ; \\mu] = \\left[\\bar{X} - 
                                   \\frac{t S}{\\sqrt{n}} ; \\bar{X} + 
                                   \\frac{t S}{\\sqrt{n}}\\right]$$")),
                            h4("Sendo t o quantil da distribuição t-student e supondo que a amostra obtida veio 
                    de uma população normal com média μ:"),
                            fluidRow(
                              box(plotlyOutput("graf2")),
                              tabBox(id = "val2",
                                     tabPanel("Média", numericInput("mu2", "Média (Parâmetro a ser estimado)", 0)),
                                     tabPanel("Variância", sliderInput("sigma22", "Variância", 1, 10, 1)),
                                     tabPanel("Tamanho", sliderInput("ams2", "Tamanho da Amostra", 1, 1000, 1000)),
                                     tabPanel("Nível", sliderInput("nv2", "Nível de Confiança", 0, 1, 0.95))),
                              box(verbatimTextOutput("ic2"))
                            ),
                            h2(""),
                            h3("3. Intervalo para variância de uma distribuição normal com média desconhecida:"),
                            h4("A quantidade pivotal e o intervalo com nível de confiança γ usados para esta
                   estimação são:"),
                            withMathJax(paste0("$$\\operatorname{Q}(X, \\sigma^2) = \\frac{(n-1)S^2}{\\sigma^2} \\sim
                                   \\operatorname{\\chi^2}_{n-1} \\quad \\text{e} \\quad 
                                   \\operatorname{IC}[\\gamma ; \\sigma^2] = \\left[\\frac{(n-1)S^2}
                                   {q_2} ; \\frac{(n-1)S^2}{q_1}\\right]$$")),
                            h4("Sendo q₁ e q₂ os quantis da distribuição Qui-Quadrado e supondo que a amostra
                    obtida veio de uma população normal com média μ e variância σ²:"),
                            fluidRow(
                              box(plotlyOutput("graf3")),
                              tabBox(id = "val3",
                                     tabPanel("Média", numericInput("mu3", "Média", 0)),
                                     tabPanel("Variância", sliderInput("sigma23", "Variância
                                                           (Parâmetro a ser estimado)",
                                                                       1, 10, 1)),
                                     tabPanel("Tamanho", sliderInput("ams3", "Tamanho da Amostra", 3, 100, 50)),
                                     tabPanel("Nível", sliderInput("nv3", "Nível de Confiança", 0, 1, 0.95))),
                              box(verbatimTextOutput("ic3"))
                            ),
                            h2(""),
                            h3("4. Intervalo para a diferença de médias de duas populações normais independentes 
                   com variâncias iguais e desconhecida:"),
                            h4("A quantidade pivotal e o intervalo com nível de confiança γ usados para esta
                   estimação são:"),
                            withMathJax(paste0("$$\\operatorname{Q}(X; \\mu_1 - \\mu_2) = \\frac{\\bar{X}-
                                  \\bar{Y} - (\\mu_1 - \\mu_2)}{\\sqrt{S^2_p \\left( \\frac{1}{n}
                                  + \\frac{1}{m}\\right)}} \\sim
                                   \\operatorname{t-student}_{n+m-2} \\quad \\text{e} \\quad 
                                   \\operatorname{IC}[\\gamma ; \\mu_1 - \\mu_2] = 
                                   \\left[\\bar{X} - \\bar{Y} - t \\sqrt{S^2_p \\left( \\frac{1}{n}
                                  + \\frac{1}{m}\\right)}  ; \\bar{X} - \\bar{Y} + t \\sqrt{S^2_p \\left( \\frac{1}{n}
                                  + \\frac{1}{m}\\right)} \\right]$$")),
                            h4("Sendo t o quantil da distribuição t-student e supondo que as amostras 
                    independentes de tamanhos n e m obtidas vieram de populações normais com médias
                    μ₁ e μ₂ e variância comum σ²:"),
                            fluidRow(
                              box(plotlyOutput("graf4")),
                              tabBox(id = "val4",
                                     tabPanel("Média 1", numericInput("mu14", "Média da População 1", 2)),
                                     tabPanel("Média 2", numericInput("mu24", "Média da População 2", 1)),
                                     tabPanel("Variância", sliderInput("sigma24", "Variância", 1, 10, 1)),
                                     tabPanel("Tamanho 1", sliderInput("ams14", "Tamanho da Amostra 1", 1, 500, 250)),
                                     tabPanel("Tamanho 2", sliderInput("ams24", "Tamanho da Amostra 2", 1, 500, 250)),
                                     tabPanel("Nível", sliderInput("nv4", "Nível de Confiança", 0, 1, 0.95))),
                              box(verbatimTextOutput("ic4"))
                            )
                            
                    ),
                    tabItem(tabName = "con",
                            h1("Consistência dos Estimadores"),
                            h4("Um estimador consistente é um estimador cujo sua estimativa se aproxima do valor
                   do parâmetro conforme o tamanho da amostra aumenta, isto é:"),
                            withMathJax(paste0("$$\\lim_{n \\rightarrow \\infty} P(|\\hat{\\theta}_n - \\theta| \\geq
                                   \\epsilon) = 0 $$")),
                            h4("Onde θ é o parâmetro a ser estimado."),
                            h4("Para demonstrar esta propriedade dos estimadores, podemos plotar o gráfico da
                   distribuição de um estimador consistente e ver seu comportamento conforme
                   o tamanho da amostra aumenta. Para isso podemos utilizar um estimador de 
                   máxima verossimilhança que tem a propriedade da consistência."),
                            h4("Por exemplo, o estimador de máxima verossimilhança (consistente) para a média de uma
                    distribuição normal com média μ e variância σ² é:"),
                            withMathJax(paste0("$$\\hat{\\mu} = \\bar{X} \\sim \\operatorname{N}\\left(\\mu,
                                   \\frac{\\sigma^2}{n} \\right)$$")),
                            fluidRow(
                              box(plotlyOutput("graf5")),
                              box(sliderInput("mu5", "Valor da média (Parâmetro de Interesse)", -2, 2, 0)),
                              box(sliderInput("sigma25", "Valor da Variância", 1, 10, 1)),
                              box(sliderInput("ams5", "Tamanho da Amostra", 1, 1000, 10))
                            ),
                            h4("Então, podemos observar que conforme o tamanho da amostra aumenta,
                   a distribuição do estimador consistente converge para uma variável aleatória 
                   degenerada no ponto equivalente ao parâmetro alvo.")
                    ),
                    tabItem(tabName = "vicio",
                            h1("Vício dos Estimadores"),
                            h4("Um estimador é dito não viciado (ou não viesado) para algum parâmetro θ, caso
                    seu valor esperado seja igual a este parâmetro θ. Com isso, podemos definir
                    o vício de um estimador T como:"),
                            withMathJax(paste0("$$\\operatorname{b}(T) = \\operatorname{E}(T) - \\theta $$")),
                            h4("Para demonstrar esta propriedade dos estimadores, podemos plotar o gráfico da
                   distribuição de um estimador viciado e um estimador não viciado para o mesmo 
                   parâmetro e observar a posição relativa do parâmetro em cada curva."),
                            h4("Por exemplo, suponha uma amostra aleatória de X com tamanho n, onde X tem 
                    distribuição Exponencial com parâmetro λ. E suponha que desejamos estimar a média de X (1/λ).
                    Para isso, considere os seguintes estimadores:"),
                            withMathJax(paste0("$$X_{(1)} = \\min(X_1, X_2, ..., X_n) \\quad \\text{e}
                                   \\quad \\bar{X} = \\frac{1}{n}\\sum\\limits_{i = 1}^{n} X_i$$")),
                            h4("E seus valores esperados:"),
                            withMathJax(paste0("$$\\operatorname{E}(X_{(1)}) = \\frac{1}{n\\lambda} \\neq 
                                   \\frac{1}{\\lambda} \\text{(viciado)} \\quad \\text{e}
                                   \\quad \\operatorname{E}(\\bar{X}) = \\frac{1}{\\lambda} 
                                   \\text{(não viciado)}$$")),
                            h4("Os estimadores tem as seguintes distribuições:"),
                            withMathJax(paste0("$$X_{(1)} \\sim \\operatorname{Exp}(n\\lambda) \\quad \\text{e}
                                    \\quad \\bar{X} \\sim \\operatorname{Gama}(n, n\\lambda)$$")),
                            fluidRow(
                              box(plotlyOutput("graf6")),
                              box(sliderInput("lambda6", "Valor de λ", 0.2, 2, 1)),
                              box(sliderInput("ams6", "Tamanho da Amostra", 1, 20, 2))
                            ),
                            h4("Então, podemos observar que a distribuição do estimador não viciado é mais
                    concentrada próxima do parâmetro alvo do que no estimador viciado.")
                    ),
                    tabItem(tabName = "eqm",
                            h1("Erro Quadrático Médio"),
                            h4("O Erro Quadrático Médio (EQM) é uma medida de aproximação de um estimador T
             em relação ao parâmetro alvo θ. O EQM de T é definido por:"),
                            withMathJax(paste0("$$\\operatorname{EQM}(T) = \\operatorname{E}_{X|\\theta}
                             [(T - \\theta)^2] = \\operatorname{Var}(T) - (\\operatorname{b}
                             (T))^2$$")),
                            h4("Onde b(T) é o vício (viés) do estimador T."),
                            h4("O EQM portanto, incorpora dois componentes, um que mede a variabilidade de T e outro
             que mede o seu viés. Logo, um bom estimador em EQM é um estimador que apresenta um 
             valor baixo de EQM."),
                            h4("Então, o EQM se torna uma boa medida para comparação entre dois estimadores quando
              se quer avaliar estas propriedades."),
                            h4("Por exemplo, podemos considerar o EQM de dois estimadores e verificar qual deles é
              menor (melhor). Para isso, suponha dois estimadores para o parâmetro θ de uma amostra
              aleatória de tamanho n de uma variável aleatória U[0, θ], onde θ é maior que 0:"),
                            withMathJax(paste0("$$T_1 = X_{(n)} \\quad \\text{e} \\quad T_2 = \\frac{\\bar{X}}{2}$$")),
                            h4("E seus respectivos EQMs:"),
                            withMathJax(paste0("$$\\operatorname{EQM}(T_1) = \\frac{2\\theta^2}{(n+1)(n+2)}
                             \\quad \\text{e} \\quad \\operatorname{EQM}(T_2) = 
                             \\frac{4\\theta^2 + 27n\\theta^2}{48}$$")),
                            h4("Então, fazendo o gráfico dos dois EQMs para θ:"),
                            fluidRow(
                              box(plotlyOutput("graf7")),
                              box(sliderInput("ams7", "Tamanho da Amostra", 1, 20, 1))
                            ),
                            h4("Então, podemos observar que o EQM de T₁ é sempre menor que o EQM de T₂, e com isso,
              pode-se concluir que T₁ é melhor em EQM do que T₂.")
                            
                    )
                    
                  )
                )
  )

server <- function(input, output){
  
  IC1 <- reactive({
    set.seed(111)
    x1 <- rnorm(input$ams1, input$mu1, sqrt(input$sigma21))
    qt1 <- qnorm((1+input$nv1)/2)
    IC1 <- c(mean(x1) - ((qt1*sqrt(input$sigma21))/sqrt(input$ams1)), 
             mean(x1) + ((qt1*sqrt(input$sigma21))/sqrt(input$ams1)))
    IC1
  })
  output$graf1 <- renderPlotly(
    plot_ly(x = seq(-3, 3, 0.01), y = dnorm(seq(-3, 3, 0.01)), type = "scatter",
            mode = "lines", line = list(color = "#043142"), showlegend = F, 
            fill = "tozeroy")  %>%
      add_trace(x = qnorm((1 - input$nv1)/2), y = seq(0, dnorm(qnorm((1 - input$nv1)/2)), 0.0001), 
                type = "scatter", mode = "lines") %>%
      add_trace(x = qnorm((1 + input$nv1)/2), y = seq(0, dnorm(qnorm((1 + input$nv1)/2)), 0.0001), 
                type = "scatter", mode = "lines") %>%
      layout(title = "Distribuição da Quantidade Pivotal - N(0,1)", xaxis = list(title = ""))
  )  
  
  output$ic1 <- renderPrint({
    paste("O IC obtido é de: [",round(IC1()[1], 6),";",round(IC1()[2], 6),"]")
  })
  
  IC2 <- reactive({
    set.seed(121)
    x2 <- rnorm(input$ams2, input$mu2, sqrt(input$sigma22))
    qt2 <- qt((1+input$nv2)/2, input$ams2-1)
    IC2 <- c(mean(x2) - ((qt2*sd(x2))/sqrt(input$ams2)), 
             mean(x2) + ((qt2*sd(x2))/sqrt(input$ams2)))
    IC2
  })
  output$graf2 <- renderPlotly(
    plot_ly(x = seq(-3, 3, 0.01), y = dt(seq(-3, 3, 0.01), input$ams2-1), type = "scatter",
            mode = "lines", line = list(color = "#043142"), showlegend = F, 
            fill = "tozeroy")  %>%
      add_trace(x = qt((1 - input$nv2)/2, input$ams2-1), 
                y = seq(0, dt(qt((1 - input$nv2)/2, input$ams2-1), input$ams2-1), 0.0001), 
                type = "scatter", mode = "lines") %>%
      add_trace(x = qt((1 + input$nv2)/2, input$ams2-1), 
                y = seq(0, dt(qt((1 + input$nv2)/2, input$ams2-1), input$ams2-1), 0.0001), 
                type = "scatter", mode = "lines") %>%
      layout(title = "Distribuição da Quantidade Pivotal - t-student(n-1)", 
             xaxis = list(title = ""))
  )  
  
  output$ic2 <- renderPrint({
    paste("O IC obtido é de: [",round(IC2()[1], 6),";",round(IC2()[2], 6),"]")
  })
  
  IC3 <- reactive({
    set.seed(131)
    x3 <- rnorm(input$ams3, input$mu3, sqrt(input$sigma23))
    qt13 <- qchisq((1-input$nv3)/2, input$ams3-1)
    qt23 <- qchisq((1+input$nv3)/2, input$ams3-1)
    IC3 <- c((var(x3) * (input$ams3-1))/qt23,(var(x3)*(input$ams3-1))/qt13)
    IC3
  })
  output$graf3 <- renderPlotly(
    plot_ly(x = seq(qchisq(0.005,input$ams3-1), qchisq(0.995, input$ams3-1), 0.1), 
            y = dchisq(seq(qchisq(0.005,input$ams3-1), qchisq(0.995, input$ams3-1), 0.1), 
                       input$ams3-1), type = "scatter", mode = "lines", 
            line = list(color = "#043142"), showlegend = F, fill = "tozeroy")  %>%
      add_trace(x = qchisq((1 - input$nv3)/2, input$ams3-1), 
                y = seq(0, dchisq(qchisq((1 - input$nv3)/2, input$ams3-1), input$ams3-1), 0.0001), 
                type = "scatter", mode = "lines") %>%
      add_trace(x = qchisq((1 + input$nv3)/2, input$ams3-1), 
                y = seq(0, dchisq(qchisq((1 + input$nv3)/2, input$ams3-1), input$ams3-1), 0.0001), 
                type = "scatter", mode = "lines") %>%
      layout(title = "Distribuição da Quantidade Pivotal - χ²(n-1)", 
             xaxis = list(title = ""))
  )  
  
  output$ic3 <- renderPrint({
    paste("O IC obtido é de: [",round(IC3()[1], 6),";",round(IC3()[2], 6),"]")
  })  
  
  IC4 <- reactive({
    set.seed(141)
    x4 <- rnorm(input$ams14, input$mu14, sqrt(input$sigma24))
    y4 <- rnorm(input$ams24, input$mu24, sqrt(input$sigma24))
    qt4 <- qt((1+input$nv4)/2, input$ams14+input$ams24-2)
    s2p <- (((input$ams14-1)*var(x4)) + ((input$ams24-1)*var(y4))) / (input$ams14 + input$ams24 - 2)
    IC4 <- c(mean(x4) - mean(y4) - qt4*sqrt(s2p * ((1/input$ams14)+(1/input$ams24))),
             mean(x4) - mean(y4) + qt4*sqrt(s2p * ((1/input$ams14)+(1/input$ams24))))
    IC4
  })
  output$graf4 <- renderPlotly(
    plot_ly(x = seq(-3, 3, 0.01), y = dt(seq(-3, 3, 0.01), input$ams14 + input$ams24 - 2), 
            type = "scatter", mode = "lines", line = list(color = "#043142"), showlegend = F, 
            fill = "tozeroy")  %>%
      add_trace(x = qt((1 - input$nv4)/2, input$ams14 + input$ams24-2), 
                y = seq(0, dt(qt((1 - input$nv4)/2, input$ams14 + input$ams24-2), 
                              input$ams14 + input$ams24-2), 0.0001), 
                type = "scatter", mode = "lines") %>%
      add_trace(x = qt((1 + input$nv4)/2, input$ams14 + input$ams24-2), 
                y = seq(0, dt(qt((1 + input$nv4)/2, input$ams14 + input$ams24-2), 
                              input$ams14 + input$ams24-2), 0.0001), 
                type = "scatter", mode = "lines") %>%
      layout(title = "Distribuição da Quantidade Pivotal - t-student(n+m-2)", 
             xaxis = list(title = ""))
  )  
  
  output$ic4 <- renderPrint({
    pt14 <- paste("A diferença de médias (parâmetro) estimada é:",input$mu14 - input$mu24)
    pt24 <- paste("e o  IC obtido é de: [",round(IC4()[1], 6),";",round(IC4()[2], 6),"]")
    paste(list(pt14, pt24))
  })
  
  output$graf5 <- renderPlotly(
    plot_ly(x = seq(-3, 3, 0.01), y = dnorm(seq(-3,3,0.01), input$mu5, sqrt(input$sigma25/input$ams5)),
            type = "scatter", mode = "lines", line = list(color = "#043142"), showlegend = F, 
            fill = "tozeroy")  %>%
      add_trace(x = input$mu5, 
                y = seq(0, dnorm(input$mu5, input$mu5, sqrt(input$sigma25/input$ams5)), 0.0001), 
                type = "scatter", mode = "lines") %>%
      layout(title = "Distribuição do Estimador Consistente", 
             xaxis = list(title = ""))
  )
  
  output$graf6 <- renderPlotly(
    plot_ly(x = seq(0.2, 5.5, 0.001),
            y = dgamma(seq(0.2, 5.5, 0.001),
                       input$ams6, input$ams6 * input$lambda6), type = "scatter", 
            mode = "lines", fill = "tozeroy", name = "Gama")  %>%
      add_trace(x = seq(0.2, 5.5, 0.001),
                y = dexp(seq(0.2, 5.5, 0.001), input$ams6 * input$lambda6), type = "scatter",
                mode = "lines", fill = "tozeroy", name = "Exponencial") %>%
      add_trace(x = 1/input$lambda6, y = seq(0, max(dgamma(1/input$lambda6, input$ams6, 
                                                           input$ams6*input$lambda6), 
                                                    dexp(1/input$lambda6, input$ams6*input$lambda6)), 
                                             0.0001), 
                type = "scatter", mode = "lines", line = list(color = "#043142"), showlegend = F) %>%
      layout(title = "Distribuição dos dois Estimadores", 
             xaxis = list(title = ""))
  )
  
  output$graf7 <- renderPlotly(
    plot_ly(x = seq(0.1,10,0.01), y = (2*(seq(0.1,10,0.01))^2) / ((input$ams7+2)*(input$ams7+1)) ,
            type = "scatter", mode = "lines", fill = "none", name = "EQM(T₁)") %>%
      add_trace(x = seq(0.1,10,0.01), y = (4*(seq(0.1,10,0.01))^2 + 27*input$ams7*(seq(0.1,10,0.01))^2)/48,
                type = "scatter", mode = "lines", fill = "none", name = "EQM(T₂)")%>%
      layout(title = "EQM de T₁ e T₂", 
             xaxis = list(title = "θ"))
  )
  
  output$pacotes <- renderUI(HTML("<ul> <li> shiny </li> <li> shinydashboard </li> <li> plotly </li>
                               <li> ggplot2 </li></ul>"))
  
  
}

# Gera a aplicação 
shinyApp(ui = ui, server = server)