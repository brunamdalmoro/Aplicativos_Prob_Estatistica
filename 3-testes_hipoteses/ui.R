# ----------------------------------------
# Nome: Teste de Hipótese
# Autora: Bruna M Dalmoro
# Abril, 2017
# ----------------------------------------

if (!require("ggplot2"))
    install.packages("ggplot2")
if (!require("shinyBS"))
    install.packages("shinyBS")
if (!require("shiny"))
    install.packages("shiny")
if(!require("shinythemes"))
    install.packages("shinythemes")


shinyUI(fluidPage(theme = shinytheme("cerulean"),
    navbarPage(title="TH",
               tabPanel("Teste de hipóteses", icon = icon("star"),
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
                                        numericInput("varpop_1",HTML("Vari&acirc;ncia da popula&ccedil;&atilde;o ou da amostra (quando n > 30):"), value = NULL)
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
                                
                                actionButton("th", strong("Realizar teste"), class="btn-info")
                         ),
                     
                     
                         column(7,
                                
                                #REMOVER COMPLETAMENTE OS ERROS
                                tags$style(type="text/css",
                                           ".shiny-output-error { visibility: hidden; }",
                                           ".shiny-output-error:before { visibility: hidden; }"),
                                
                                br(),br(),
                                conditionalPanel(
                                    condition="input.th>0",
                                    plotOutput("teste.plot"),
                                    tableOutput("tabteste")
                                    
                                           
                                           )))
                         ),
               tabPanel("Sobre o App", icon = icon("info-circle"),
                        column(5,
                               wellPanel(
                                   h3("Testes de Hipóteses"),
                                   HTML("<p>O <strong> teste de hipóteses</strong> é um procedimento estatístico onde se
                                         busca verificar uma hipótese a respeito da <em>população</em>, tendo por base 
                                         <em>dados amostrais</em>. A hipótese estatística é uma suposição feita sobre o
                                        <strong> parâmetro</strong> da população.</p>
                                        <p><p>Este aplicativo tem o objetivo de realizar os seguintes testes de hipóteses 
                                        para <strong>médias</strong>:</p></p>
                                        <p class=text-info><em>1. Comparação de uma média com um valor padrão:</em></p><ul>	
                                        <li class=text-info>Variância conhecida ou n > 30</li>	
                                        <li class=text-info>Variância desconhecida ou n &le; 30</li></ul>
                                        <p class=text-info><em>2. Comparação entre duas médias:</em></p>
                                        <p class=text-info> - Duas amostras independentes</p>
                                        <ul><li class=text-info>Ambas variâncias conhecidas</li>
                                        <li class=text-info>Ambas variâncias desconhecidas, mas supostas iguais</li>
                                        <li class=text-info>Ambas variâncias desconhecidas, mas supostas diferentes</li></ul>
                                        <p class=text-info> - Duas amostras pareadas</p>"),
                                   br(),
                                   p("Código no",
                                     a(href = "https://github.com/brunamdalmoro/Aplicativos_Prob_Estatistica/tree/master/3-testes_hipoteses", "GitHub"),
                                     ".")
                               )),
                        column(6,
                               h3("Configurando o teste"),
                                HTML("<p><span style=font-size:16px;>Inicialmente você deve escolher qual teste você quer realizar,
                                      no painel à esqueda. Os campos seguintes vão se alterando conforme o teste escolhido.</span></p>
                                      <p><span style=font-size:16px;>Assim que escolher o teste, preencha os campos com as informações
                                      da(s) amostra(s).</span></p>
                                      <p><span style=font-size:16px;>Enfim, você pode definir as configurações do testes de hipóteses,
                                      como a direção do teste e o nívelde significância, &alpha;.</p></span>
                                      <p><span style=font-size:16px;>Clique em <srong>Realizar teste!</strong> quando estiver 
                                      pronto!</span></p>"),
                               h3("Resultados"),
                               HTML("<p><span style=font-size:16px;><b>Gráfico: </b>
                                    o gráfico representa uma curva normal (quando variância conhecida) ou a distribuição t-Student. Nele,
                                    é possível visualizar a região de rejeição em destaque e a estatísticado teste calculada
                                    com os dados inseridos anteriormente.</span></p>
                                    <p><span style=font-size:16px;><strong>Tabela: </strong>
                                    na tabela estão alguns resultados calculados, como o valor da estatística do teste, o p-valor e
                                    a decisão estatística baseada no p-valor.")
                               ),
                        column(1)
                        )
               )))