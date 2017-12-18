# ----------------------------------------
# Nome: Gráficos e Descritivas
# Autora: Bruna Martini Dalmoro
# Agosto, 2017
# ----------------------------------------

library(shiny)
library(ggplot2)
library(shinyBS)
library(datasets)
library(vcdExtra)
library(e1071)
library(shinythemes)


# Função para selecionar variáveis 
opcoes <- function(df,classe){
    sap <- sapply(df, class)
    sub <- subset(sap, sap %in% classe)
    c('Selecione...', names(sub))
}

# Função moda
statmod <- function(x) {
    z <- table(as.vector(x))
    names(z)[z == max(z)]
}

# -------------------------------------------------------------------------------------------------------
# SHINY SERVER ------------------------------------------------------------------------------------------

shinyServer(function(input, output) {
    
    # quando clica no botão p realizar teste
    v <- reactiveValues(doPlot = FALSE) # FALSE
    observeEvent(input$desc, {  v$doPlot <- input$desc  }) #click = TRUE
    
    # DADOS ---------------------------------------------------------------------------------------------
    # Definindo o dataset
    dtset <- reactive({
        switch(input$dataset,
               'WWWusage' = {
                   ww <- data.frame(time=1:length(WWWusage), usage=WWWusage)
                   colnames(ww) <- c("Tempo","Uso")
                   ww
                   },
               'AirCrash' = {
                   data("AirCrash")
                   ac <- AirCrash[,-5]
                   colnames(ac) <- c("Fase","Causa","Data","Fatalidades")
                   levels(ac$Fase) <- c("em rota","pousando","parado","decolando","desconhecido")
                   levels(ac$Causa) <- c("criminoso","erro humano","mecânico","desconhecido","clima/tempo")
                   ac
                   },
               'mtcars' = {
                   x <- mtcars
                   x$am <- factor(x$am)
                   levels(x$am) <- c("Automático", "Manual")
                   x$vs <- factor(x$vs)
                   levels(x$vs) <- c("V-engine", "Straight")
                   colnames(x) <- c("mpg","cilindros","desloc","potencia","eixo","peso","tempo","motor",
                                    "cambio","marchas","carb")
                   x
                   },
               'Titanicp' = {
                   data("Titanicp")
                   tp <- Titanicp
                   colnames(tp) <- c("Classe","Sobreviventes","Sexo","Idade","irmconj","paisfilhos")
                   levels(tp$Sobreviventes) <- c("morreu","sobreviveu")
                   levels(tp$Sexo) <- c("mulher","homem")
                   tp
               })
    })
    
    
    # Printando os dados
    output$printdata <- renderDataTable({
        dtset()
    }, options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese.json'),
                      pageLength = 10, dom = 'lti'))
    
    # GRÁFICO -------------------------------------------------------------------------------------------
    # Selecionando o gráfico
    output$selectgrafico <- renderUI({
        switch(input$dataset,
               'WWWusage' = {
                   selectInput("grafico","Selecione o gráfico:",
                               c('Linha','Histograma','Boxplot'))
               },
               'AirCrash' = {
                   selectInput("grafico","Selecione o gráfico:",
                               c('Barras','Setores','Linha','Histograma','Boxplot'))
               },
               'mtcars' = {
                   selectInput("grafico","Selecione o gráfico:",
                               c('Barras','Setores','Histograma','Boxplot'))
               },
               'Titanicp' = {
                   selectInput("grafico","Selecione o gráfico:",
                               c('Barras','Setores','Histograma','Boxplot'))
               })
    })
    
    # Separando quais variaveis podem ir em qual eixo de acordo com o gráfico
    varx.opcoes <- reactive({
        switch(input$grafico,
               'Barras' = opcoes(dtset(),c("factor","character")),
               'Setores' = opcoes(dtset(),c("factor","character")),
               'Linha' = opcoes(dtset(),c("numeric","integer","double","Date")),
               'Histograma' = opcoes(dtset(),c("numeric","integer","double","ts")),
               'Boxplot' = opcoes(dtset(),c("factor","character")))
        })
    
    vary.opcoes <- reactive({
        switch(input$grafico,
               'Barras' = opcoes(dtset(),c("numeric","integer","double")),
               'Setores' = opcoes(dtset(),c("numeric","integer","double")),
               'Linha' = opcoes(dtset(),c("numeric","integer","double","ts")),
               'Boxplot' = opcoes(dtset(),c("numeric","integer","double","ts")))
    })
    
    # Mandando pro UI as opções para o usuário escolher
    output$selectvarx <- renderUI({
        selectInput("x.varplot", "Variável X:", varx.opcoes())
    })
    
    output$selectvary <- renderUI({
        if(input$grafico %in% c('Barras','Setores','Linha','Boxplot')){
            selectInput("y.varplot", "Variável Y:", vary.opcoes())
        }
    })
    
    # Infos do gráfico
    output$infografico <- renderUI({
        switch(input$grafico,
               'Barras' = {
                   tags$div(
                       h6("No", strong("gráfico de barras/colunas"), "os retângulos possuem tamanho 
                          proporcional às grandezas, seja em relação à sua frequência ou à soma dos
                          valores de outra variável. São úteis para ilustrar comparações entre itens."),
                       h6(em("Dica:"), "Experimente selecional uma variável categórica X e não selecionar 
                          nenhuma variável numérica Y. Assim você estará observando a contagem de
                          frequência de cada uma das categorias. Ao selecionar uma variável Y, você terá
                          a soma dos valores da variável Y para cada item/categoria."),
                       h6(em("Lembre-se:"), "O objetivo dos gráficos é transmitir a informação de forma 
                          rápida e clara. Portanto, escolha variáveis que façam a visualização gráfica
                          fazer sentido."))
                   },
               'Setores' = {
                   tags$div(
                       h6("O", strong("gráfico de setores/pizza"), "é um gráfico circular dividido em 
                          ´fatias´ que representam as grandezas, seja em relação à sua frequência ou à 
                          soma dos valores de outra variável. Indicado para quando se deseja evidenciar 
                          o quanto cada informação representa no total."),
                       h6(em("Dica:"),"O gráfico de setores é mais indicado para representar variáveis
                          com no máximo três categorias, para uma melhor visualização. Para variáveis com
                          com maior número de categorias indica-se o gráfico de barras/colunas."),
                       h6("Experimente selecional uma variável categórica X e não selecionar 
                          nenhuma variável numérica Y. Assim você estará observando a contagem de
                          frequência de cada uma das categorias. Ao selecionar uma variável Y, você terá
                          a soma dos valores da variável Y para cada item/categoria."),
                       h6(em("Lembre-se:"),"O objetivo dos gráficos é transmitir a informação de forma 
                          rápida e clara. Portanto, escolha variáveis que façam a visualização gráfica
                          fazer sentido.")
                       )
                   },
               'Linha' = {
                   tags$div(
                       h6("O", strong("gráfico de linhas"), "é utilizado principalmente para comparar a
                          evolução de uma variável ao longo do tempo. Ideal para mostrar tendências em 
                          dados a intervalos iguais."),
                       h6(em("Dica:"), "Utilizar gráfico de linhas para representar outra coisa que não
                          indique evolução temporal pode tornar o gráfico confuso. Neste caso, outro tipo 
                          de visuaização pode ser mais indicado."),
                       h6(em("Lembre-se:"),"O objetivo dos gráficos é transmitir a informação de forma 
                          rápida e clara. Portanto, escolha variáveis que façam a visualização gráfica
                          fazer sentido.")
                   )
                   },
               'Histograma' = {
                   tags$div(
                       h6("O", strong("histograma"), "consiste de um conjunto de retângulos contíguos cuja
                          base é igual à amplitude do intervalo e a altura proporcional à frequência das
                          respectivas classes. É utilizado para observar o comportamento e distribuição de
                          variáveis numéricas."),
                       h6(em("Dica:"), "São representadas principalmente variáveis contínuas. Não confunda
                          com o gráfico de barras! Entenda mais sobre a construção de um histograma neste
                          link:", a("Exploring Histograms",href="http://tinlizzie.org/histograms/")),
                       h6(em("Lembre-se:"),"O objetivo dos gráficos é transmitir a informação de forma 
                          rápida e clara. Portanto, escolha variáveis que façam a visualização gráfica
                          fazer sentido.")
                   )
                   },
               'Boxplot' = {
                   tags$div(
                       h6("O", strong("boxplot"), "é a representação gráfica do resumo de cinco números:
                          a linha central representa a mediana; os limites da ´caixa´ representam, de baixo
                          pra cima, o primeiro e o terceiro quartil; as pontas dos ´bigodes´ representam,
                          de baixo pra cima, os valores mínimo e máximo não discrepantes. Os valores 
                          discrepantes (ou ´outliers´) são representados por pontos além dos limites dos
                          ´bigodes´. É através do boxplot que conseguimos visualizar a assimetria dos
                          dados."),
                       h6(em("Dica:"), "Experimente selecionar apenas a variável numérica Y. Assim, pode-se
                          observar o boxplot daquela variável isoladamente. Selecione uma variável
                          categórica X e observe o boxplot da variável Y de acordo com as categorias da
                          variável X."),
                       h6(em("Lembre-se:"),"O objetivo dos gráficos é transmitir a informação de forma 
                          rápida e clara. Portanto, escolha variáveis que façam a visualização gráfica
                          fazer sentido.")
                   )
                   }
               )
    })
    
    
    # GRÁFICO! :)
    output$descplot <- renderPlot({
        
        if (v$doPlot == FALSE) return()
        isolate({
            
            switch(input$grafico,
                   'Barras' = {
                       if(input$y.varplot == 'Selecione...'){
                           
                           xis <- dtset()[,input$x.varplot]
                           
                           gg <- ggplot(dtset(), aes(x = factor(xis), fill = factor(xis))) +
                               geom_bar(stat = "count") +
                               xlab(input$x.varplot) +
                               ylab('Frequência') +
                               theme_bw() +
                               guides(fill=F)
                           
                           print(gg)
                           
                       } else if(input$y.varplot != 'Selecione...'){
                           
                           xis <- dtset()[,input$x.varplot]
                           yis <- dtset()[,input$y.varplot]
                           
                           gg <- ggplot(dtset(), aes(x = factor(xis), y = yis, fill = factor(xis))) +
                               geom_bar(stat = "identity") +
                               xlab(input$x.varplot) +
                               ylab(input$y.varplot) +
                               theme_bw() +
                               guides(fill=F)
                           
                           print(gg)
                       }
                       },
                   
                   'Setores' = {
                       
                       cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                                      "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
                       
                       if(input$y.varplot == 'Selecione...'){
                           
                           xis <- dtset()[,input$x.varplot]
                           
                           gg <- ggplot(dtset(), aes(x = factor(1), fill = factor(xis))) +
                               geom_bar(stat = "count", width = 1) +
                               coord_polar(theta = "y", start = 0) +
                               theme_void() + scale_fill_manual(values=cbPalette) +
                               guides(fill=guide_legend(title=NULL))
                           
                           print(gg)
                           
                       } else if(input$y.varplot != 'Selecione...'){
                           
                           xis <- dtset()[,input$x.varplot]
                           yis <- dtset()[,input$y.varplot]
                           
                           gg <- ggplot(dtset(), aes(x = factor(1), y = yis, fill = factor(xis))) +
                               geom_bar(stat = "identity", width = 1) +
                               coord_polar(theta = "y", start = 0) +
                               theme_void() + scale_fill_manual(values=cbPalette) +
                               guides(fill=guide_legend(title=NULL)) 
                           
                           print(gg)
                       }
                       
                   },
                   'Linha' = {
                       
                       xis <- dtset()[,input$x.varplot]
                       yis <- dtset()[,input$y.varplot]
                       
                       gg <- ggplot(dtset(), aes(x = xis, y = yis, group = 1)) +
                           geom_line() +
                           xlab(input$x.varplot) +
                           ylab(input$y.varplot) +
                           theme_bw()
                       
                       print(gg)
                       
                   },
                   'Histograma' = {
                       
                       xis <- dtset()[,input$x.varplot]
                       
                       gg <- ggplot(dtset(), aes(x = xis, fill = factor(1))) +
                           geom_histogram(colour="black", alpha=.7, position="identity")  +
                           xlab(input$x.varplot) +
                           ylab("Frequência") +
                           theme_bw() +
                           guides(fill=guide_legend(title=NULL))
                       
                       print(gg)
                       
                   },
                   'Boxplot' = {
                       
                       if(input$x.varplot == 'Selecione...'){
                           xis <- factor(1)
                       } else if(input$x.varplot != 'Selecione...'){
                           xis <- dtset()[,input$x.varplot]
                       }
                       
                       yis <- dtset()[,input$y.varplot]
                       
                       gg <- ggplot(dtset(), aes(x = factor(xis), y = yis, fill = factor(xis))) +
                           geom_boxplot() +
                           xlab(input$x.varplot) +
                           ylab(input$y.varplot) +
                           theme_bw() +
                           guides(fill=guide_legend(title=NULL)) 
                       
                       print(gg)
                       
                   })
            })
    })
    
    
    # MEDIDAS DESCRITIVAS ----------------------------------------------------------------------------------
    # Seleciona variáveis p tabela de descritivas
    output$selectvars <- renderUI({
        nomes <- names(dtset())
        selectInput("vardescr", "Escolha a variável:", nomes)
    })
    
        
    # TABELA! :) ---------------------------------------------------------------
    
    ## Medidas de posicão ------------------------
    
    output$mposicao <- renderUI({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            tags$div(  h4("Medidas de loalização, posição ou tendência central:")  )
        } else if(tipovar %in% 'Date'){
            tags$div(  h4("Não aplicável a datas.")  )
        } else if(tipovar %in% c('factor','character')){
            tags$div(  h4("Tabela de frequência:")  )
        }
    })
    
    output$desctable <- renderTable({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('factor','character')){
            tt <- as.data.frame(summary(var))
            t(tt)
        } else if(tipovar %in% c('numeric','double','integer','ts')){
            data.frame("Média" = mean(var, na.rm = T),
                       "Moda" = as.numeric(statmod(var)),
                       "Mediana" = median(var, na.rm = T))
        }
    })
    
    
    ## Medidas separatrizes -----------------------
    
    output$msepara <- renderUI({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            tags$div(
                h4("Medidas separatrizes:")
            )
        }
        
    })
    
    output$desctable2 <- renderTable({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            data.frame("Mínimo" = min(var, na.rm = T),
                       "Quartil 1" = quantile(var, 0.25, na.rm = T),
                       "Mediana Q2" = median(var, na.rm = T),
                       "Quartil 3" = quantile(var, 0.75, na.rm = T),
                       "Máximo" = max(var, na.rm = T))
        }
    })
    
    
    ## Medidas de variação -----------------------
    
    output$mvariacao <- renderUI({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            tags$div(
                h4("Medidas de variação ou dispersão:")
            )
        }
        
    })
    
    output$desctable3 <- renderTable({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            data.frame("Amplitude interquartílica" = IQR(var, na.rm = T),
                       "Amplitude total" = max(var, na.rm = T)-min(var, na.rm = T),
                       "Variância" = var(var, na.rm = T),
                       "Desvio Padrão" = sd(var, na.rm = T))
        }
    })
    
    
    ## Medidas de formato --------------------------
    
    output$mformato <- renderUI({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            tags$div(
                h4("Medidas de formato:")
            )
        }
        
    })
    
    output$desctable4 <- renderTable({
        
        var <- dtset()[,input$vardescr]
        tipovar <- class(var)
        
        if(tipovar %in% c('numeric','double','integer','ts')){
            data.frame("Assimetria" = skewness(var, na.rm = T),
                       "Curtose" = kurtosis(var, na.rm = T))
        }
    })
})
        