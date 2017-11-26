# ----------------------------------------
# Nome: Gráficos e Descritivas
# Autora: Bruna Martini Dalmoro
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
if(!require("shinythemes"))
    install.packages("shinythemes")


shinyUI(
    fluidPage(theme = shinytheme("united"),
              navbarPage(title="Gráficos e Descritivas",
                    tabPanel("Dados", icon = icon("database"),
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
                                                h6(em("pclass:"),"classe (1ª, 2ª, 3ª)"),
                                                h6(em("survived:"),"situação do passageiro após naufrágio (sobreviveu, morreu)"),
                                                h6(em("sex:"),"sexo (feminino, masculino)"),
                                                h6(em("age:"),"idade (em anos)"),
                                                h6(em("sibsp:"),"número de irmãos ou cônjuges a bordo"),
                                                h6(em("parch:"),"número de pais ou filhos a bordo")
                                            )
                                            )
                                        ),
                                 
                                 column(9,
                                        dataTableOutput("printdata")
                                        )
                                 )
                    ),
               
                    tabPanel("Gráficos", icon = icon("bar-chart-o"),
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
                                                     icon = icon("bar-chart-o"),
                                                     class = "btn btn-warning")
                                 ),
                                 
                                 column(7,
                                        
                                        plotOutput("descplot")
                                 )),
                             
                             fluidRow(
                                 column(2,
                                        
                                        h5("Base de dados:"),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'WWWusage'",
                                            h5("Uso de Internet por Minuto"),
                                            h6("Uma série temporal do número de usuários conectados à internet através de um servidor a cada minuto.")
                                        ),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'AirCrash'",
                                            h5("Acidentes Aéreos"),
                                            h6("Dados de todos os acidentes de avião comerciais fatais de 1993 a 2015. Exclui aviões pequenos (menos de 6 passageiros) e aeronaves não comerciais (carga, militares, privadas).")
                                        ),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'mtcars'",
                                            h5("Motor Trend Car Road Tests"),
                                            h6("Dados de consumo de combustível e 10 aspectos do design e desempenho de 32 automóveis (modelos 1973-74).")
                                        ),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'Titanicp'",
                                            h5("Passageiros do Titanic"),
                                            h6("Dados sobre os passageiros do Titanic, excluindo a tripulação.")
                                        )
                                 ),
                                 
                                 column(3,
                                        
                                        h5("Variáveis:"),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'WWWusage'",
                                            h6(em("time:"), "identificação dos minutos"),
                                            h6(em("usage:"), "número de usuários conectados à internet através do servidor")
                                        ),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'AirCrash'",
                                            h6(em("Phase:"),"fase do vôo (em percurso, aterrissagem, decolagem, desconhecido)"),
                                            h6(em("Cause:"),"causa do acidente (criminoso, erro humano, erro mecânico, clima/tempo, desconhecido)"),
                                            h6(em("date:"),"data do acidente"),
                                            h6(em("Fatalities:"),"número de fatalidades")
                                        ),
                                        
                                        conditionalPanel(
                                            "input.dataset == 'mtcars'",
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
                                            h6(em("pclass:"),"classe (1ª, 2ª, 3ª)"),
                                            h6(em("survived:"),"situação do passageiro após naufrágio (sobreviveu, morreu)"),
                                            h6(em("sex:"),"sexo (feminino, masculino)"),
                                            h6(em("age:"),"idade (em anos)"),
                                            h6(em("sibsp:"),"número de irmãos ou cônjuges a bordo"),
                                            h6(em("parch:"),"número de pais ou filhos a bordo")
                                        )
                                 )
                             )
                    ),
               
                    tabPanel("Medidas Descritivas", icon = icon("calculator"),
                             fluidRow(
                                 column(3,
                                        wellPanel(
                                            uiOutput("selectvars"),
                                            
                                            hr(),
                                            
                                            h5("Base de dados:"),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'WWWusage'",
                                                h5("Uso de Internet por Minuto"),
                                                h6("Uma série temporal do número de usuários conectados à internet através de um servidor a cada minuto.")
                                            ),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'AirCrash'",
                                                h5("Acidentes Aéreos"),
                                                h6("Dados de todos os acidentes de avião comerciais fatais de 1993 a 2015. Exclui aviões pequenos (menos de 6 passageiros) e aeronaves não comerciais (carga, militares, privadas).")
                                            ),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'mtcars'",
                                                h5("Motor Trend Car Road Tests"),
                                                h6("Dados de consumo de combustível e 10 aspectos do design e desempenho de 32 automóveis (modelos 1973-74).")
                                            ),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'Titanicp'",
                                                h5("Passageiros do Titanic"),
                                                h6("Dados sobre os passageiros do Titanic, excluindo a tripulação.")
                                            ),
                                            
                                            h5("Variáveis:"),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'WWWusage'",
                                                h6(em("time:"), "identificação dos minutos"),
                                                h6(em("usage:"), "número de usuários conectados à internet através do servidor")
                                            ),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'AirCrash'",
                                                h6(em("Phase:"),"fase do vôo (em percurso, aterrissagem, decolagem, desconhecido)"),
                                                h6(em("Cause:"),"causa do acidente (criminoso, erro humano, erro mecânico, clima/tempo, desconhecido)"),
                                                h6(em("date:"),"data do acidente"),
                                                h6(em("Fatalities:"),"número de fatalidades")
                                            ),
                                            
                                            conditionalPanel(
                                                "input.dataset == 'mtcars'",
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
                                                h6(em("pclass:"),"classe (1ª, 2ª, 3ª)"),
                                                h6(em("survived:"),"situação do passageiro após naufrágio (sobreviveu, morreu)"),
                                                h6(em("sex:"),"sexo (feminino, masculino)"),
                                                h6(em("age:"),"idade (em anos)"),
                                                h6(em("sibsp:"),"número de irmãos ou cônjuges a bordo"),
                                                h6(em("parch:"),"número de pais ou filhos a bordo")
                                            )
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
                    ),
               
                    tabPanel("Sobre o App", icon = icon("info-circle"),
                             fluidRow(
                                 column(5,
                                        wellPanel(
                                            h3(strong("Aplicativo de Gráficos e Descritivas"), class = "text-primary"),
                                            h4("Este aplicativo tem como objetivo contemplar o conteúdo de 
                                               ", strong("Estatísticas Descritivas.")), 
                                            h4("A ideia é que você possa visualizar gráficos e
                                               medidas descritivas das variáveis dos banco de dados disponíveis
                                               no aplicativo. Assim, interagindo com as variáveis e experimentando
                                               os diferentes gráficos, você poderá escolher qual seria a
                                               melhor visualização para aquelas variáveis além de observar
                                               o comportamento dos dados tanto nos gráficos quanto nas
                                               medidas descritivas."),
                                            br(),
                                            p("Código no",
                                              a(href = "https://github.com/brunamdalmoro/Aplicativos_Prob_Estatistica/tree/master/1-descritivas_graficos", "GitHub"),
                                              ".")
                                        )
                                 ),
                                 
                                 column(7,
                                        h3("Este aplicativo possui 3 abas diferentes (além desta):",
                                            em("Dados, Gráficos"), "e", em("Medidas Descritivas.")),
                                        h3(strong("Dados:")),
                                        h4("Na primeira aba, você pode escolher o banco de dados que 
                                           irá utilizar para fazer os gráficos e calcular as medidas 
                                           descritivas. Ao selecionar um dos bancos de dados, é possível 
                                           visualizar as primeiras observações e uma breve descrição sobre
                                           os dados e quais as suas variáveis."),
                                        h3(strong("Gráficos:")),
                                        h4("De acordo o banco de dados escolhido, até cinco opções de 
                                           gráficos serão oferecidas na segunda aba: Gráfico de barras ou 
                                           colunas; gráfico de setores; gráfico de linha; histograma; 
                                           e boxplot. Você pode definir quais variáveis irão nos eixos X e Y
                                           de acordo com o gráfico selecionado e visualizá-lo."),
                                        h3(strong("Medidas descritivas:")),
                                        h4("Na terceira aba é possível observar as medidas descritivas das 
                                           variáveis do banco de dados selecionado. Para variáveis numéricas
                                           são exibidas as medidas de localização, posição ou tendência central,
                                           medidas separatrizes, medidas de variação ou dispersão e medidas de 
                                           formato. Para variáveis categóricas é apresentada a tabela de 
                                           frequências. Não é aplicável a variáveis que representem datas.")
                                 )
                             )
                    )
                    
                    
              )))
