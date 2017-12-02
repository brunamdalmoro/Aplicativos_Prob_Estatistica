# ----------------------------------------
# Nome: TCL
# Autora: Bruna M Dalmoro
# Outubro, 2017
# ----------------------------------------

# Pacotes
if (!require("ggplot2"))
    install.packages("ggplot2")
if (!require("shinyBS"))
    install.packages("shinyBS")
if (!require("shiny"))
    install.packages("shiny")
if(!require("shinythemes"))
    install.packages("shinythemes")


# UI
shinyUI(fluidPage(theme = shinytheme("flatly"),
     navbarPage(title="TCL",
                # APLICATIVO ---------------------------------------------------
                tabPanel("Simulação", icon = icon("bar-chart-o"),
                         fluidRow(
                             column(3,
                                    wellPanel(
                                        selectInput("distribuicao", "Distribuição de probabilidade:",
                                                    c("Binomial" = "binomial",
                                                      "Poisson" = "poisson",
                                                      "Uniforme" = "uniforme",
                                                      "Exponencial" = "exponencial",
                                                      "Normal" = "normal",
                                                      "t-Student" = "dist.t",
                                                      "Qui-Quadrado" = "quiquadrado",
                                                      "F-Snedecor" = "dist.f")
                                                    ),

                                        # Parametros
                                        h4("Parâmetros:"),
                                        
                                        conditionalPanel( # Binomial
                                            condition = "input.distribuicao == 'binomial'",
                                            
                                            numericInput("size.bin", "Número de ensaios",
                                                         min = 1, value = 10),
                                            sliderInput("prob.bin", "Probabilidade de sucesso",
                                                         min = 0, max = 1, value = 0.5)
                                            ),
                                        conditionalPanel( # Poisson
                                            condition = "input.distribuicao == 'poisson'",
                                            
                                            numericInput("lambda.pois", "Média (lambda)",
                                                         min = 0, value = 2)
                                        ),
                                        conditionalPanel( # Uniforme
                                            condition = "input.distribuicao == 'uniforme'",
                                            
                                            numericInput("min.unif", "Mínimo",
                                                         value = 0),
                                            numericInput("max.unif", "Máximo",
                                                         value = 1)
                                        ),
                                        conditionalPanel( # Exponencial
                                            condition = "input.distribuicao == 'exponencial'",
                                            
                                            numericInput("lambda.exp", "Taxa (lambda)",
                                                         value = 2)
                                        ),
                                        conditionalPanel( # Normal
                                            condition = "input.distribuicao == 'normal'",
                                            
                                            numericInput("media.norm", "Média",
                                                         value = 0),
                                            numericInput("desvio.norm", "Desvio padrão",
                                                         min = 0, value = 1)
                                        ),
                                        conditionalPanel( # t-student
                                            condition = "input.distribuicao == 'dist.t'",
                                            
                                            numericInput("gl.t", "Graus de liberdade",
                                                         min = 0, value = 5)
                                        ),
                                        conditionalPanel( # Qui-quadrado
                                            condition = "input.distribuicao == 'quiquadrado'",
                                            
                                            numericInput("gl.qui", "Graus de liberdade",
                                                         min = 0, value = 5)
                                        ),
                                        conditionalPanel( # F Snedecor
                                            condition = "input.distribuicao == 'dist.f'",
                                            
                                            numericInput("gl1.f", "Graus de liberdade 1",
                                                         min = 0, value = 2),
                                            numericInput("gl2.f", "Graus de liberdade 2",
                                                         min = 0, value = 5)
                                        ),
                                        
                                        h4("Amostras:"),
                                        
                                        # Tamanho de amostra
                                        sliderInput("tam.amostra", "Tamanho das amostras (n)",
                                                    min = 10, max = 100, value = 30, step = 5),
                                        
                                        # Número de amostras
                                        sliderInput("num.amostra", "Número de amostras",
                                                    min = 10, max = 1000, value = 200, step = 10),
                                        
                                        actionButton("vai.tcl", strong("Gerar amostras!"),
                                                     icon = icon("refresh"),
                                                     class = "btn btn-success")
                                        )
                                        ),
                                 
                                 column(9,
                                        plotOutput("plot.dist"),
                                        plotOutput("plot.media")
                                        )
                             )
                         ),
                # SOBRE O APP ----------------------------------------------------
                tabPanel("Sobre o App", icon = icon("info-circle"),
                         fluidRow(
                             column(4,
                                    wellPanel(
                                        h3("O Teorema Central do Limite"),
                                        HTML("<p>Segundo o <strong>Teorema Central do Limite</strong>,
                                             se a população de onde foi extraída a amostra aleatória não tiver distribuição normal, então a 
                                             distribuição amostral da média se aproximará da normal <u>à medida que o tamanho da amostra 
                                             cresce</u>. Se a população de onde foi extraída a amostra aleatória tiver distribuição normal, 
                                             então a distribuição amostral da média será normal.</p>
                                             <p><p>Assim, a população de onde a variável foi extraída 
                                             pode ter distribuição normal ou não, podendo inclusive ter distribuição de probabilidade discreta,
                                             mas se tomarmos várias amostras grandes desta população e fizermos um histograma das médias
                                             amostrais, a forma se parecerá como uma distribuição normal.</p></p>
                                             <p><em>A ideia do aplicativo é conseguir observar 
                                             graficamente este teorema, comparando a distribuição 
                                             de probabilidade das médias com a distribuição de 
                                             probabilidade da amostra.</em></p>"
                                        ),
                                        br(),
                                        p("Código no",
                                          a(href = "https://github.com/brunamdalmoro/Aplicativos_Prob_Estatistica/tree/master/2-probabilidade_TCL", "GitHub"),
                                          ".")
                                        )),
                             column(4,
                                    h2("Controles"),
                                    HTML("<p><strong><u>Distribuição de probabilidade</u>: </strong>
                                         Neste seletor você pode escolher de qual distribuição de probabilidade serão geradas amostras,
                                         ou seja, qual a distribuição de probabilidade da população;</p>
                                         <p><strong><u>Parâmetros</u>:</strong> De acordo com a distribuição de probabilidade escolhida, você 
                                         poderá definir os seus parâmetros logo abaixo, podendo também observar como os dados se comportam 
                                         conforme os parâmetros são alterados;</p>
                                         <p><strong><u>Tamanho das amostras</u>:</strong> Aqui você pode definir qual tamanho terá cada uma de suas
                                         amostras;</p>
                                         <p><strong><u>Número de amostras</u>:</strong> E, por fim, poderá definir quantas amostras serão geradas a 
                                         partir da distribuição escolhida para que sejam calculadas as médias.</p>
                                         <p> <p>Depois de definir os campos acima, clique em <strong>Gerar amostras!</strong> para que as amostras sejam
                                         sorteadas aleatoriamente eos gráficos sejam gerados.</p></p>")),
                             column(4,
                                    h2("Gráficos"),
                                    HTML("<p>O <strong>primeiro gráfico</strong> apresenta a 
                                         distribuição de probabilidade das amostras aleatórias geradas. É um gráfico de barras (em caso 
                                         discreto) ou histograma (em caso contínuo). <em>Por exemplo, se nos controles você definiu que: 
                                         a distribuição que a distribuição de probabilidade é uma Exponencial, o parâmetro lambda é igual 
                                         a 2 e as amostras têm tamanho 10, então o gráfico será um histograma de uma distribuição Exponencial(2) 
                                         com tamanho de amostra n = 10.</em></p>
                                         <p>O <strong>segundo gráfico</strong> apresenta a distribuição das médias amostrais em formato 
                                         de histograma, junto à curva da distribuição normal. <em>A distribuição das médias <u>sempre</u> 
                                         será uma distribuição normal com média igual à média das média e desvio padrão igual ao desvio 
                                         padrão das médias das amostras.</em></p>
                                         <p>Interaga com as distribuições e seus parâmetros, experimente trocar os tamanhos de amostra e 
                                         números de amostra gerados, e observe graficamente o que acontece. <strong>Esse é o Teorema Central do 
                                         Limite em ação!</strong></p>"))
                             ))
                )))
