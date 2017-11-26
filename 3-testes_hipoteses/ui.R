# ----------------------------------------
# Nome: Teste de Hipótese
# Autora: Bruna M Dalmoro
# ----------------------------------------

if (!require("ggplot2"))
    install.packages("ggplot2")

if (!require("shinyBS"))
    install.packages("shinyBS")


shinyUI(fluidPage(
    # FAVICON DA PÁGINA WEB
    # tags$head(tags$link(rel = "icon", type = "image/x-icon", 
    #                     href = "https://webresource.its.calpoly.edu/cpwebtemplate/5.0.1/common/images_html/favicon.ico")),
    
    navbarPage(title="Shiny App",
               tabPanel("Teste de hipóteses",
                     fluidRow(
                         column(3,
                                wellPanel(
                                    h5("Selecione o tipo de teste de hipótese:"),
                                    selectInput("testemi", "Médias:",
                                                choices=list("Uma média"=1,"Duas médias"=2),
                                                selected=1),
                                    
                                    conditionalPanel(
                                        condition = "input.testemi == 1",
                                        radioButtons("testevar", "Variância ou tamanho do n:",
                                                     choices = list("Variância conhecida ou n > 30"=1,
                                                                    "Variância desconhecida ou n <= 30"=2),
                                                     selected = 1)),
                                    
                                    conditionalPanel(
                                        condition = "input.testemi == 2",
                                        selectInput("testeamostra", "Natureza das amostras:",
                                                     choices = list("Duas amostras independentes"=1,
                                                                    "Duas amostras dependentes (pareadas)"=2),
                                                     selected = 1),
                                        
                                        conditionalPanel(
                                            condition = "input.testeamostra == 1",
                                            radioButtons("testeindep", "Variâncias:",
                                                         choices = list("Variâncias conhecidas"=1,
                                                                        "Variâncias desconhecidas, mas iguais"=2,
                                                                        "Variâncias desconhecidas, mas diferentes"=3),
                                                         selected = 1))
                                        )
                                    ),
                                
                                conditionalPanel(
                                    condition = "input.testemi == 1",
                                    numericInput("mi1_1","Média da amostra:", value = NULL),
                                    numericInput("mi0", HTML("Valor padrão &mu;&#8320;:"),value = NULL),
                                    
                                    conditionalPanel(
                                        condition = "input.testevar == 1",
                                        numericInput("varpop_1","Variância da população ou da amostra (quando n > 30):", value = NULL)
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.testevar == 2",
                                        numericInput("varamo_1","Variância da amostra:", value = NULL)
                                    )),
                                
                                conditionalPanel(
                                    condition = "input.testemi == 2",
                                    
                                    conditionalPanel(
                                        condition = "input.testeamostra == 1",
                                        numericInput("mi1_2","Média da primeira amostra:", value = NULL),
                                        numericInput("mi2","Média da segunda amostra:", value = NULL),
                                        
                                        conditionalPanel(
                                            condition = "input.testeindep == 1",
                                            numericInput("varpop1_2","Variância da população 1:", value = NULL),
                                            numericInput("varpop2_2","Variância da população 2:", value = NULL)
                                        ),
                                        
                                        conditionalPanel(
                                            condition = "input.testeindep == 2",
                                            numericInput("varamo1_2","Variância da amostra 1:", value = NULL),
                                            numericInput("varamo2_2","Variância da amostra 2:", value = NULL)
                                            ),
                                        
                                        conditionalPanel(
                                            condition = "input.testeindep == 3",
                                            numericInput("varamo3_2","Variância da amostra 1:", value = NULL),
                                            numericInput("varamo4_2","Variância da amostra 2:", value = NULL))
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.testeamostra == 2",
                                        numericInput("d_3","Diferença média das amostras:", value = NULL),
                                        numericInput("vard_3","Variância da diferença:", value = NULL)
                                    )
                                )),
                         
                         
                         
                         column(2,
                                
                                conditionalPanel(
                                    condition = "input.testemi == 1",
                                    numericInput("namos_1","Tamanho da amostra:", value = NULL)
                                ),
                                
                                conditionalPanel(
                                    condition = "input.testemi == 2",
                                    
                                    conditionalPanel(
                                        condition = "input.testeamostra == 1",
                                        numericInput("namos1_2","Tamanho da amostra 1:", value = NULL),
                                        numericInput("namos2_2","Tamanho da amostra 2:", value = NULL)
                                    ),
                                    
                                    conditionalPanel(
                                        condition = "input.testeamostra == 2",
                                        numericInput("namos_3","Tamanho da amostra:", value = NULL)
                                    )
                                    
                                ),
                                
                                selectInput("direcao", "Direção do teste:",
                                            choices = list("Bilateral"=1,
                                                           "Unilateral superior"=2,
                                                           "Unilateral inferior"=3)),
                                
                                sliderInput("alpha", HTML("Nível de significância &alpha;:"),
                                            min = 0.01, max = 1, step = 0.01,
                                            value = 0.05),
                                
                                actionButton("th", strong("Realizar teste"))
                         ),
                     
                     
                         column(7,
                                br(),br(),
                                conditionalPanel(
                                    condition="input.th>0",
                                    plotOutput("teste.plot"),
                                    # bsPopover("tdist","p-value","TEXTO POPUP",
                                    #           trigger="hover",placement="left"),br()
                                    tableOutput("tabteste")
                                    
                                           
                                           )))
                         ))))