# ----------------------------------------
# Nome: Teste de Hipótese
# Autora: Bruna M Dalmoro
# ----------------------------------------


####### Library and data sets #######

options(shiny.maxRequestSize=30*1024^2)

library(ggplot2)
library(shinyBS)


######## Funções apoio ########

# uma média, var conhecida
z.teste <-  function(mu1, mu0, var, n){
    zeta <- (mu1 - mu0) / (sqrt(var / n))
    return(zeta)
}

# uma média, var desconhecida
t.teste <- function(mu1, mu0, var, n){
    teta <- (mu1 - mu0) / (sqrt(var / n))
    return(teta)
}

# duas médias, independentes, vars conhecidas
z.teste2 <- function(mu1, mu2, var1, var2, n1, n2){
    zeta2 <- (mu1 - mu2) / sqrt( (var1/n1) + (var2/n2) )
    return(zeta2)
}

# duas médias, independentes, vars desconhecidas, iguais
t.teste.igual<- function(mu1, mu2, var1, var2, n1, n2){
    var <- (var1*(n1-1) + var2*(n2-1)) / ((n1-1) + (n2-1))
    teta.igual <- (mu1 - mu2) / sqrt( (1/n1 + 1/n2) * var )
    return(teta.igual)
}

# duas médias, independentes, vars desconhecidas, diferentes
t.teste.diferentes<- function(mu1, mu2, var1, var2, n1, n2){
    teta.diferente <- (mu1 - mu2) / sqrt( (var1/n1 + var2/n2))
    return(teta.diferente)
}

# duas médias, pareadas
t.teste.pareado <- function(dif, var, n){
    t.pareado <- dif / sqrt(var/n)
    return(t.pareado)
}




############ Shiny server ############

shinyServer(function(input, output) {
    
    # quando clica no botão p realizar teste
    v <- reactiveValues(doPlot = FALSE) #recebe FALSE
    
    observeEvent(input$th, {
        v$doPlot <- input$th
    }) #quando clicado, recebe TRUE
    
   ############### PLOT ###############
    
    output$teste.plot <- renderPlot({
        ################################################################
        ## TESTE PARA UMA MÉDIA
        ################################################################
        if(input$testemi == 1)
        {
            if(input$testevar == 1) #var conhecida
            {
                if(input$direcao == 1)      #bilateral
                {
                    # teste Z para uma média bilateral
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)
                    
                    rc <- qnorm(1-input$alpha/2)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dnorm(xv)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        z.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(z,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_segment(aes(x=z,y=0,xend=z,yend=max(dnorm(z),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                       print(z.plot) 
                    })
                    
                } else if(input$direcao == 2) #uni superior
                {
                    # teste Z para uma média uni superior
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)

                    rc <- qnorm(1-input$alpha)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dnorm(xv)
                    df <- data.frame(xv,yv)
                    
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()

                    isolate({

                        z.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(rc,2),round(z,2),0)) +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_segment(aes(x=z,y=0,xend=z,yend=max(dnorm(z),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))

                        print(z.plot)
                    })
                }
                else                        #uni inferior
                {
                    # teste Z para uma média uni inferior
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)
                    
                    rc <- qnorm(1-input$alpha)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dnorm(xv)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        z.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(z,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_segment(aes(x=z,y=0,xend=z,yend=max(dnorm(z),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(z.plot) 
                    })
                }
                
            } else if(input$testevar == 2) #var desconhecida
            {
                if(input$direcao == 1)      #bilateral
                {
                    # teste t para uma média bilateral
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    
                    rc <- qt(1-input$alpha/2, input$namos_1-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_1-1)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        t.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df=input$namos_1-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(t,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                            geom_segment(aes(x=t,y=0,xend=t,yend=max(dt(t, input$namos_1-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(t.plot) 
                    })
                    
                } else if(input$direcao == 2) #uni superior
                {
                    # teste t para uma média uni superior
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    
                    rc <- qt(1-input$alpha, input$namos_1-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_1-1)
                    df <- data.frame(xv,yv)
                    
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        t.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df = input$namos_1-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(rc,2),round(t,2),0)) +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                            geom_segment(aes(x=t,y=0,xend=t,yend=max(dt(t, input$namos_1-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(t.plot) 
                    })
                }
                else                        #uni inferior
                {
                    # teste t para uma média uni inferior
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    
                    rc <- qt(1-input$alpha, input$namos_1-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_1-1)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        t.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df = input$namos_1-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(t,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_segment(aes(x=t,y=0,xend=t,yend=max(dt(t, input$namos_1-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(t.plot) 
                    })
                }
                
            }
        ################################################################
        ## TESTE PARA DUAS MÉDIAS
        ################################################################
        } else if(input$testemi == 2)
        {
            ######## AMOSTRAS INDEPENDENTES ########
            if(input$testeamostra == 1)
            {
                if(input$testeindep == 1) #var conhecidas
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        
                        rc <- qnorm(1-input$alpha/2)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dnorm(xv)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            z2.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(z2,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=z2,y=0,xend=z2,yend=max(dnorm(z2),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(z2.plot) 
                        })
                        
                    } else if(input$direcao == 2) #uni superior
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        
                        rc <- qnorm(1-input$alpha)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dnorm(xv)
                        df <- data.frame(xv,yv)
                        
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            z2.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(rc,2),round(z2,2),0)) +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=z2,y=0,xend=z2,yend=max(dnorm(z2),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(z2.plot) 
                        })
                    }
                    else                        #uni inferior
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        
                        rc <- qnorm(1-input$alpha)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dnorm(xv)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            z2.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(z2,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_segment(aes(x=z2,y=0,xend=z2,yend=max(dnorm(z2),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(z2.plot) 
                        })
                    }
                } else if(input$testeindep == 2) #var desconhecidas iguais
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha/2, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            ti.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(ti,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=ti,y=0,xend=ti,yend=max(dt(ti,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(ti.plot) 
                        })
                        
                    } else if(input$direcao == 2) #uni superior
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            ti.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(rc,2),round(ti,2),0)) +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=ti,y=0,xend=ti,yend=max(dt(ti,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(ti.plot) 
                        })
                    }
                    else                        #uni inferior
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            ti.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(ti,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_segment(aes(x=ti,y=0,xend=ti,yend=max(dt(ti,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(ti.plot) 
                        })
                    }
                } else if(input$testeindep == 3) #var desconhecidas diferentes
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha/2, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            td.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(td,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=td,y=0,xend=td,yend=max(dt(td,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(td.plot) 
                        })
                    }
                    else if(input$direcao == 2) #uni superior
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            td.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(rc,2),round(td,2),0)) +
                                geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                                geom_segment(aes(x=td,y=0,xend=td,yend=max(dt(td,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(td.plot) 
                        })
                    }
                    else                        #uni inferior
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        rc <- qt(1-input$alpha, gl)
                        
                        xv <- seq(-4,4,0.01)
                        yv <- dt(xv, gl)
                        df <- data.frame(xv,yv)
                        
                        shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            
                            td.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                                stat_function(fun = dt, n = 101, args = list(df = gl)) +
                                ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                                scale_x_continuous(breaks = c(round(-rc,2),round(td,2),0)) +
                                geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                                geom_segment(aes(x=td,y=0,xend=td,yend=max(dt(td,gl),0.01)), color = "red") +
                                geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                                theme(panel.background = element_rect(fill = NA),
                                      axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                            
                            print(td.plot) 
                        })
                    }
                }
                
                ######## AMOSTRAS DEPENDENTES ########
            } else if(input$testeamostra == 2)
            {
                if(input$direcao == 1)      #bilateral
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    
                    rc <- qt(1-input$alpha/2, input$namos_3-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_3-1)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        tp.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df = input$namos_3-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(rc,2),round(tp,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                            geom_segment(aes(x=tp,y=0,xend=tp,yend=max(dt(tp,input$namos_3-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(tp.plot) 
                    })
                }
                else if(input$direcao == 2) #uni superior
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    
                    rc <- qt(1-input$alpha, input$namos_3-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_3-1)
                    df <- data.frame(xv,yv)
                    
                    shade2 <- as.data.frame(rbind(c(rc, 0), subset(df, xv > rc), c(4,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        tp.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df = input$namos_3-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(rc,2),round(tp,2),0)) +
                            geom_polygon(data = shade2, aes(xv, yv), alpha = 0.5, fill="#ff7f50") + 
                            geom_segment(aes(x=tp,y=0,xend=tp,yend=max(dt(tp,input$namos_3-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(tp.plot) 
                    })
                }
                else                        #uni inferior
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    
                    rc <- qt(1-input$alpha, input$namos_3-1)
                    
                    xv <- seq(-4,4,0.01)
                    yv <- dt(xv, input$namos_3-1)
                    df <- data.frame(xv,yv)
                    
                    shade1 <- as.data.frame(rbind(c(-4, 0), subset(df, xv < -rc), c(-rc,0)))
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        
                        tp.plot <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
                            stat_function(fun = dt, n = 101, args = list(df = input$namos_3-1)) +
                            ylab("") + xlab("") + scale_y_continuous(breaks = NULL) +
                            scale_x_continuous(breaks = c(round(-rc,2),round(tp,2),0)) +
                            geom_polygon(data = shade1, aes(xv, yv), alpha = 0.5, fill="#ff7f50") +
                            geom_segment(aes(x=tp,y=0,xend=tp,yend=max(dt(tp,input$namos_3-1),0.01)), color = "red") +
                            geom_segment(aes(x=-4,y=0,xend=4,yend=0)) +
                            theme(panel.background = element_rect(fill = NA),
                                  axis.text.x  = element_text(angle=90, vjust=0.5, size = 15))
                        
                        print(tp.plot) 
                    })
                }
            }
            
    }})
    
    ############### RESULTADO ###############
    output$tabteste <- renderTable({
        ################################################################
        ## TESTE PARA UMA MÉDIA
        ################################################################
        if(input$testemi == 1)
        {
            if(input$testevar == 1) #var conhecida
            {
                if(input$direcao == 1)      #bilateral
                {
                    # teste Z para uma média bilateral
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)
                    pv <- 2*pnorm(-abs(z))
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(z,pv,res)
                        colnames(tab) <- c("Z do teste","Valor p", "Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                    
                } else if(input$direcao == 2) #uni superior
                {
                    # teste Z para uma média uni superior
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)
                    pv <- pnorm(-z)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(z,pv,res)
                        colnames(tab) <- c("Z do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                else                        #uni inferior
                {
                    # teste Z para uma média uni inferior
                    
                    z <- z.teste(input$mi1_1, input$mi0, input$varpop_1, input$namos_1)
                    pv <- pnorm(z)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(z,pv,res)
                        colnames(tab) <- c("Z do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                
            } else if(input$testevar == 2) #var desconhecida
            {
                if(input$direcao == 1)      #bilateral
                {
                    # teste t para uma média bilateral
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    gl <- input$namos_1-1
                    pv <- 2*pt(-abs(t),gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,t,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                    
                } else if(input$direcao == 2) #uni superior
                {
                    # teste t para uma média uni superior
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    gl <- input$namos_1-1
                    pv <- pt(-t,gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,t,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                else                        #uni inferior
                {
                    # teste t para uma média uni inferior
                    
                    t <- t.teste(input$mi1_1, input$mi0, input$varamo_1, input$namos_1)
                    gl <- input$namos_1-1
                    pv <- pt(t,gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,t,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                
            }
            ################################################################
            ## TESTE PARA DUAS MÉDIAS
            ################################################################
        } else if(input$testemi == 2)
        {
            ######## AMOSTRAS INDEPENDENTES ########
            if(input$testeamostra == 1)
            {
                if(input$testeindep == 1) #var conhecidas
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        pv <- 2*pnorm(-abs(z2))
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(z2,pv,res)
                            colnames(tab) <- c("Z do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                        
                    } else if(input$direcao == 2) #uni superior
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        pv <- pnorm(-z2)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(z2,pv,res)
                            colnames(tab) <- c("Z do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                    else                        #uni inferior
                    {
                        z2 <- z.teste2(input$mi1_2, input$mi2, input$varpop1_2, input$varpop2_2, input$namos1_2, input$namos2_2)
                        pv <- pnorm(z2)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(z2,pv,res)
                            colnames(tab) <- c("Z do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                } else if(input$testeindep == 2) #var desconhecidas iguais
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- 2*pt(-abs(ti),gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,ti,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                        
                    } else if(input$direcao == 2) #uni superior
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- pt(-ti,gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,ti,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                    else                        #uni inferior
                    {
                        ti <- t.teste.igual(input$mi1_2, input$mi2, input$varamo1_2, input$varamo2_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- pt(ti,gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,ti,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                } else if(input$testeindep == 3) #var desconhecidas diferentes
                {
                    if(input$direcao == 1)      #bilateral
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- 2*pt(-abs(td),gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,td,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                    else if(input$direcao == 2) #uni superior
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- pt(-td,gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,td,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                    else                        #uni inferior
                    {
                        td <- t.teste.diferentes(input$mi1_2, input$mi2, input$varamo3_2, input$varamo4_2, input$namos1_2, input$namos2_2)
                        gl <- (input$namos1_2-1) + (input$namos2_2-1)
                        pv <- pt(td,gl)
                        res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                        
                        if (v$doPlot == FALSE) return()
                        
                        isolate({
                            tab <- data.frame(gl,td,pv,res)
                            colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                            rownames(tab) <- "Valores"
                            tab
                        })
                    }
                }
                
                ######## AMOSTRAS DEPENDENTES ########
            } else if(input$testeamostra == 2)
            {
                if(input$direcao == 1)      #bilateral
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    gl <- input$namos_3-1
                    pv <- 2*pt(-abs(tp),gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,tp,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                else if(input$direcao == 2) #uni superior
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    gl <- input$namos_3-1
                    pv <- pt(-tp,gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,tp,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
                else                        #uni inferior
                {
                    tp <- t.teste.pareado(input$d_3, input$vard_3, input$namos_3)
                    gl <- input$namos_3-1
                    pv <- pt(tp,gl)
                    res <- ifelse(pv <= input$alpha, "Rejeita H0", "Não rejeita H0")
                    
                    if (v$doPlot == FALSE) return()
                    
                    isolate({
                        tab <- data.frame(gl,tp,pv,res)
                        colnames(tab) <- c("gl","t do teste","Valor p","Decisão")
                        rownames(tab) <- "Valores"
                        tab
                    })
                }
            }
            
        }})
})