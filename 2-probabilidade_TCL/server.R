# ----------------------------------------
# Nome: TCL
# Autora: Bruna M Dalmoro
# Outubro, 2017
# ----------------------------------------

####### Library and options #######

options(shiny.maxRequestSize=30*1024^2, shiny.sanitize.errors = TRUE)

library(ggplot2)
library(shinyBS)
library(shiny)

#-------------------------------------------------------------------------------
# Shiny server -----------------------------------------------------------------
shinyServer(function(input, output, session) {
    
    # Check do botão
    v <- reactiveValues(doPlot = FALSE)
    
    observeEvent(input$vai.tcl, {
        v$doPlot <- input$vai.tcl
    }) 
    
    
    #---------------------------------------------------------------------------
    # DISTRIBUIÇÃO DOS DADOS ---------------------------------------------------
    output$plot.dist <- renderPlot({
        
        if(input$distribuicao == 'binomial') # binomial
        {
            # Amostra aleatória
            bino <- rbinom(input$tam.amostra, input$size.bin, input$prob.bin)
            df.bino <- as.data.frame(bino)
            
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.bino, aes(df.bino)) +
                    geom_bar(aes(x=bino), 
                             colour="grey30", fill="antiquewhite1") +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(paste0("Amostra aleatória X ~ Bin("
                                   ,input$size.bin,", ",input$prob.bin,")"))
            })
        } else if(input$distribuicao == 'poisson') # poisson
        {
            # Amostra aleatória
            pois <- rpois(input$tam.amostra, input$lambda.pois)
            df.pois <- as.data.frame(pois)
            
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.pois, aes(df.pois)) +
                    geom_bar(aes(x=pois), 
                             colour="grey30", fill="antiquewhite1") +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(paste0("Amostra aleatória X ~ Poisson(",input$lambda.pois,")"))
                
            })
        } else if(input$distribuicao == 'uniforme') # uniforme
        {
            # Amostra aleatória
            uni <- runif(input$tam.amostra, input$min.unif, input$max.unif)
            df.uni <- as.data.frame(uni)
            
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.uni, aes(df.uni)) +
                    geom_histogram(aes(x=uni), 
                                   bins = 10, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = dunif, 
                                  args = list(min = input$min.unif, max = input$max.unif),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(input$min.unif,input$max.unif)) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(paste0("Amostra aleatória X ~ Uniforme("
                                   ,input$min.unif,", ",input$max.unif,")"))
                
            })
        } else if(input$distribuicao == 'exponencial') # exponencial
        {
            # Amostra aleatória
            expo <- rexp(input$tam.amostra, input$lambda.exp)
            df.expo <- as.data.frame(expo)
            
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.expo, aes(df.expo)) +
                    geom_histogram(aes(y = ..density..),
                                   bins = 10, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = dexp, 
                                  args = list(rate = input$lambda.exp),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(qexp(0.0001,input$lambda.exp),
                                                  qexp(0.9999,input$lambda.exp))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(paste0("Amostra aleatória X ~ Exp(",input$lambda.exp,")"))
                
            })
        } else if(input$distribuicao == 'normal') # normal
        {
            # Amostra aleatória
            normal <- rnorm(input$tam.amostra, input$media.norm, input$desvio.norm)
            df.normal <- as.data.frame(normal)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.normal, aes(df.normal)) +
                    geom_histogram(aes(y = ..density..),
                                   bins = 15, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = input$media.norm, sd = input$desvio.norm),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,input$media.norm,input$desvio.norm),
                                                  qnorm(0.9999,input$media.norm,input$desvio.norm))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(paste0("Amostra aleatória X ~ Normal("
                                   ,input$media.norm,",",input$desvio.norm,")"))
                
            })
        } else if(input$distribuicao == 'dist.t') # t-student
        {
            # Amostra aleatória
            tdist <- rt(input$tam.amostra, input$gl.t)
            df.tdist <- as.data.frame(tdist)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.tdist, aes(df.tdist)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 10, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = dt, 
                                  args = list(df = input$gl.t),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(qt(0.0001,input$gl.t),
                                                  qt(0.9999,input$gl.t))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Amostra aleatória X ~ t"[.(input$gl.t)]))
                
            })
        } else if(input$distribuicao == 'quiquadrado') # qui-quadrado
        {
            # Amostra aleatória
            quidist <- rchisq(input$tam.amostra, input$gl.qui)
            df.quidist <- as.data.frame(quidist)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.quidist, aes(df.quidist)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 10, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = dchisq, 
                                  args = list(df = input$gl.qui),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(qchisq(0.0001,input$gl.qui),
                                                  qchisq(0.9999,input$gl.qui))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Amostra aleatória X ~" ~{chi^2} [.(input$gl.qui)]) )
                
            })
        } else if(input$distribuicao == 'dist.f') # f-snedecor
        {
            # Amostra aleatória
            fdist <- rf(input$tam.amostra, input$gl1.f, input$gl2.f)
            df.fdist <- as.data.frame(fdist)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.fdist, aes(df.fdist)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 10, colour="grey30", fill="antiquewhite1") +
                    stat_function(fun = df, 
                                  args = list(df1 = input$gl1.f, df2 = input$gl2.f),
                                  colour = "deeppink", size = 1
                    ) +
                    scale_x_continuous(limits = c(qf(0.0001,input$gl1.f, input$gl2.f),
                                                  qf(0.98,input$gl1.f, input$gl2.f))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Amostra aleatória X ~ F" [ "("~.(input$gl1.f)~","~.(input$gl2.f)~")"]))
                
            })
        }
    })
    
    #---------------------------------------------------------------------------
    # DISTRIBUIÇÃO DA MÉDIA ---------------------------------------------------
    output$plot.media <- renderPlot({
        
        if(input$distribuicao == 'binomial') # binomial
        {
            # Gerando as N médias
            bino.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) bino.medias[i] <- mean(rbinom(input$tam.amostra,
                                                                         input$size.bin,
                                                                         input$prob.bin))
            df.bino.medias <- as.data.frame(bino.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.bino.medias$bino.medias),2)
            b <- round(sd(df.bino.medias$bino.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.bino.medias, aes(df.bino.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.bino.medias$bino.medias),
                                              sd = sd(df.bino.medias$bino.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'poisson') # poisson
        {
            # Gerando as N médias
            pois.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) pois.medias[i] <- mean(rpois(input$tam.amostra,
                                                                          input$lambda.pois))
            df.pois.medias <- as.data.frame(pois.medias)
            
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.pois.medias$pois.medias),2)
            b <- round(sd(df.pois.medias$pois.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.pois.medias, aes(df.pois.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.pois.medias$pois.medias),
                                              sd = sd(df.pois.medias$pois.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'uniforme') # uniforme
        {
            # Gerando as N médias
            uni.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) uni.medias[i] <- mean(runif(input$tam.amostra,
                                                                       input$min.unif,
                                                                       input$max.unif))
            df.uni.medias <- as.data.frame(uni.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.uni.medias$uni.medias),2)
            b <- round(sd(df.uni.medias$uni.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.uni.medias, aes(df.uni.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.uni.medias$uni.medias),
                                              sd = sd(df.uni.medias$uni.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'exponencial') # exponencial
        {
            # Gerando as N médias
            exp.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) exp.medias[i] <- mean(rexp(input$tam.amostra,
                                                                      input$lambda.exp))
            df.exp.medias <- as.data.frame(exp.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.exp.medias$exp.medias),2)
            b <- round(sd(df.exp.medias$exp.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.exp.medias, aes(df.exp.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.exp.medias$exp.medias),
                                              sd = sd(df.exp.medias$exp.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'normal') # normal
        {
            # Gerando as N médias
            normal.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) normal.medias[i] <- mean(rnorm(input$tam.amostra,
                                                                          input$media.norm,
                                                                          input$desvio.norm))
            df.normal.medias <- as.data.frame(normal.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.normal.medias$normal.medias),2)
            b <- round(sd(df.normal.medias$normal.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.normal.medias, aes(df.normal.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.normal.medias$normal.medias),
                                              sd = sd(df.normal.medias$normal.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'dist.t') # t-student
        {
            # Gerando as N médias
            t.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) t.medias[i] <- mean(rt(input$tam.amostra,
                                                                  input$gl.t))
            df.t.medias <- as.data.frame(t.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.t.medias$t.medias),2)
            b <- round(sd(df.t.medias$t.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.t.medias, aes(df.t.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.t.medias$t.medias),
                                              sd = sd(df.t.medias$t.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'quiquadrado') # qui-quadrado
        {
            # Gerando as N médias
            qui.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) qui.medias[i] <- mean(rchisq(input$tam.amostra,
                                                                        input$gl.qui))
            df.qui.medias <- as.data.frame(qui.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.qui.medias$qui.medias),2)
            b <- round(sd(df.qui.medias$qui.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.qui.medias, aes(df.qui.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.qui.medias$qui.medias),
                                              sd = sd(df.qui.medias$qui.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        } else if(input$distribuicao == 'dist.f') # f-snedecor
        {
            # Gerando as N médias
            f.medias <- numeric(input$num.amostra)
            for (i in 1:input$num.amostra) f.medias[i] <- mean(rf(input$tam.amostra,
                                                                  input$gl1.f,
                                                                  input$gl2.f))
            df.f.medias <- as.data.frame(f.medias)
            # Calculando a média e desvio padrão das amostras
            a <- round(mean(df.f.medias$f.medias),2)
            b <- round(sd(df.f.medias$f.medias),2)
            
            if (v$doPlot == FALSE) return()
            
            isolate({
                # Gráfico
                ggplot(df.f.medias, aes(df.f.medias)) +
                    geom_histogram(aes(y = ..density..), 
                                   bins = 15, colour="grey30", fill="lightblue") +
                    stat_function(fun = dnorm, 
                                  args = list(mean = mean(df.f.medias$f.medias),
                                              sd = sd(df.f.medias$f.medias)),
                                  colour = "dodgerblue3", size = 1
                    ) +
                    scale_x_continuous(limits = c(qnorm(0.0001,a,b),
                                                  qnorm(0.9999,a,b))) +
                    theme_light() + labs(x="",y="") +
                    theme(plot.title = element_text(face = "bold")) +
                    ggtitle(bquote("Média das amostras "~bar(X)~" ~ Normal("~.(a)~","~.(b)~")"))
            })
        }
    })
})

