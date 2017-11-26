# ----------------------------------------
# Nome: Descritivas
# Autora: Bruna M Dalmoro
# Agosto, 2017
# ----------------------------------------

if (!require("shiny"))
    install.packages("shiny")
if (!require("ggplot2"))
    install.packages("ggplot2")
if (!require("shinyBS"))
    install.packages("shinyBS")
if (!require("datasets"))
    install.packages("datasets")
if (!require("vcdExtra"))
    install.packages("vcdExtra")
if(!require("e1071"))
    install.packages("e1071")

# RETORNA ERRO GENÉRICO
# options(shiny.sanitize.errors = TRUE)


shinyUI(fluidPage(
    navbarPage(title="Shiny App",
               tabPanel("Descritivas",
                        tabsetPanel(
                            tabPanel("Dados",
                                     fluidRow(
                                         column(3,
                                                wellPanel(
                                                    selectInput("dataset","Escolha um banco de dados:", 
                                                               c('AirCrash',
                                                                 'mtcars',
                                                                 'Titanicp',
                                                                 'WWWusage')),
                                                    
                                                    conditionalPanel(
                                                        "input.dataset == 'WWWusage'",
                                                        h5(strong("Uso de Internet por Minuto")),
                                                        h6("Uma série temporal do número de usuários conectados à internet através de um servidor a cada minuto."),
                                                        h6(strong("Variáveis:")),
                                                        h6(em("time:"), "identificação dos minutos"),
                                                        h6(em("usage:"), "número de usuários conectados à internet através do servidor")
                                                        ),
                                                    
                                                    conditionalPanel(
                                                        "input.dataset == 'AirCrash'",
                                                        h5(strong("Acidentes Aéreos")),
                                                        h6("Dados de todos os acidentes de avião comerciais fatais de 1993 a 2015. Exclui aviões pequenos (menos de 6 passageiros) e aeronaves não comerciais (carga, militares, privadas)."),
                                                        h6(strong("Variáveis:")),
                                                        h6(em("Phase:"),"fase do vôo (em percurso, aterrissagem, decolagem, desconhecido)"),
                                                        h6(em("Cause:"),"causa do acidente (criminoso, erro humano, erro mecânico, clima/tempo, desconhecido)"),
                                                        h6(em("date:"),"data do acidente"),
                                                        h6(em("Fatalities:"),"número de fatalidades")
                                                        ),
                                                    
                                                    conditionalPanel(
                                                        "input.dataset == 'mtcars'",
                                                        h5(strong("Motor Trend Car Road Tests")),
                                                        h6("Dados de consumo de combustível e 10 aspectos do design e desempenho de 32 automóveis (modelos 1973-74)."),
                                                        h6(strong("Variáveis:")),
                                                        h6(em("mpg:"),"milhas/galão"),
                                                        h6(em("cyl:"),"número de cilindros"),
                                                        h6(em("disp:"),"deslocamento (polegada cúbica)"),
                                                        h6(em("hp:"),"potência bruta"),
                                                        h6(em("drat:"),"proporção do eixo traseiro"),
                                                        h6(em("wt:"),"peso (1000 libras)"),
                                                        h6(em("qsec:"),"tempo para percorrer 1/4 de milha"),
                                                        h6(em("vs:"),"motor de combustão interna (V-engine, Straight)"),
                                                        h6(em("am:"),"câmbio (automático, manual)"),
                                                        h6(em("gear:"),"número de marchas"),
                                                        h6(em("carb:"),"número de carburadores")
                                                    ),
                                                    
                                                    conditionalPanel(
                                                        "input.dataset == 'Titanicp'",
                                                        h5(strong("Passageiros do Titanic")),
                                                        h6("Dados sobre os passageiros do Titanic, excluindo a tripulação."),
                                                        h6(strong("Variáveis:")),
                                                        h6(em("pclass"),"classe (1ª, 2ª, 3ª)"),
                                                        h6(em("survived"),"situação do passageiro após naufrágio (sobreviveu, morreu)"),
                                                        h6(em("sex"),"sexo (feminino, masculino)"),
                                                        h6(em("age"),"idade (em anos)"),
                                                        h6(em("sibsp"),"número de irmãos ou cônjuges a bordo"),
                                                        h6(em("parch"),"número de pais ou filhos a bordo")
                                                    )
                                                    )
                                                ),
                                         
                                         column(9,
                                                dataTableOutput("printdata")
                                                )
                                         )
                            ),
                            tabPanel("Gráficos",
                                     fluidRow(
                                         column(3,
                                                wellPanel(
                                                    uiOutput("selectgrafico"),
                                                    uiOutput("infografico")
                                                )
                                         ),
                                         
                                         column(2,
                                                #REMOVER COMPLETAMENTE OS ERROS
                                                tags$style(type="text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }"),
                                                
                                                br(),
                                                uiOutput("selectvarx"),
                                                uiOutput("selectvary"),
                                                # uiOutput("selectgrupo"),
                                                actionButton("desc", strong("Gráfico!"),
                                                             icon = icon("signal"),
                                                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                                         ),
                                         
                                         column(7,
                                                
                                                plotOutput("descplot")
                                         ))
                                     ),
                            tabPanel("Medidas Descritivas",
                                     fluidRow(
                                         column(3,
                                                wellPanel(
                                                    uiOutput("selectvars")
                                                )
                                         ),
                                         
                                         column(9,
                                                uiOutput("mposicao"),
                                                tableOutput("desctable"),
                                                uiOutput("msepara"),
                                                tableOutput("desctable2"),
                                                uiOutput("mvariacao"),
                                                tableOutput("desctable3"),
                                                uiOutput("mformato"),
                                                tableOutput("desctable4")
                                         ))
                            )
                            )
                        ))))
