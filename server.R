### Rubin-Rosenbaum meets Liublinska App Function ---------


library(shiny)
library(ggplot2)
library(reshape2)
library(magrittr)




function(input, output){
  
  Var1<-Var2<-value<-NULL
  
  steps <- 10
  
 
  ReacData1 <- reactive({
    
    steps <- 10
    
    
    validate(
      need(!is.null(input$alpha)|!is.null(input$beta)|!is.null(input$gamma)|!is.null(input$q),
           "Please move any slider to create your first sensitivity plot. Default data are taken from Rosenbaum and Rubin (1983).
             
              Remember:
                alpha - Log odds ratio of the confounder regarding the outcome in the treatment group 
                beta - Log odds ratio of the confounder regarding the outcome in the control group 
                gamma - Log odds ratio of the confounder regarding the treatment assignment 
                q - Prevelance of the binary confounder 

           ")
    )
    
    if (input$xAxis=="alpha"|input$yAxis=="alpha"){
      alpha1range <- input$alpha1range
      alpha1vector <- seq(alpha1range[1],alpha1range[2],length.out = steps)       
    }
    if (input$xAxis=="beta"|input$yAxis=="beta"){
      beta1range <- input$beta1range
      beta1vector <- seq(beta1range[1],beta1range[2],length.out = steps)
    }
    if (input$xAxis=="gamma"|input$yAxis=="gamma"){
      gamma1range <- input$gamma1range
      gamma1vector <- seq(gamma1range[1],gamma1range[2],length.out = steps)
    }
    if (input$xAxis=="q" | input$yAxis=="q"){
      qrange <- input$qrange
      qvector <- seq(qrange[1],qrange[2],length.out = steps)
    }
    
    
    
    
    if (input$xAxis!="alpha" & input$yAxis!="alpha"){
      alpha1range <- c(input$alpha,input$alpha)
      alpha1vector <- rep(input$alpha,steps)   
    }
    if (input$xAxis!="beta" & input$yAxis!="beta"){
      beta1range <- c(input$beta,input$beta)
      beta1vector <- rep(input$beta,steps) 
    }
    if (input$xAxis!="gamma" & input$yAxis!="gamma"){
      gamma1range <- c(input$gamma,input$gamma)
      gamma1vector <- rep(input$gamma,steps) 
    }
    if (input$xAxis!="q" & input$yAxis!="q"){
      qrange <- c(input$q,input$q)
      qvector <- rep(input$q,steps) 
    }
    
    
    
    
    
    print(alpha1vector)
    print(beta1vector)
    print(gamma1vector)
    print(qvector)
    
    
    
    
    DataSens<- data.frame(alpha1vector=alpha1vector,beta1vector=beta1vector,
                          gamma1vector= gamma1vector, qvector=qvector,
                          alpha1range=alpha1range,beta1range=beta1range,
                          gamma1range=gamma1range,qrange=qrange)
    
    
    
    
    
    
    ### 5-dimensional array for the parameters alpha0, beta0 and gamma0
    ### that we have to estimate via ML.
    
    
    
    
    validate(
      need(input$xAxis!=input$yAxis, 
           "Please choose two different parameters for the axes!")
    )
    
    
    inFile <- input$datafile
    
    if (is.null(inFile)) {
      # User has not uploaded a file yet
      Treatment<-c(rep(x = 0,925),rep(1,590))
      Outcome<-c(rep(1,333),rep(0,925-333),rep(1,395),rep(0,590-395))
      DataF<-data.frame(Treatment=Treatment,Y=Outcome)
    } else{
      
      infileDataFrame <- read.csv(inFile$datapath,
                                  header=T,
                                  sep=input$sep,
                                  dec = input$dec,
                                  row.names = NULL)
      
      
      validate(
        need(input$sep!=input$dec, 
             "Separator and decimal separator cannot be the same"),
        need(!is.null(infileDataFrame$Treatment), 
             "Please check if you have a column named 'Treatment' in your 
            dataset"),
        need(!is.null(infileDataFrame$Outcome), "Please check if you have a 
          column named 'Outcome' in your dataset"),
        
        
        need(is.numeric(infileDataFrame$Treatment), "Please check if your column
            'Treatment' is numeric"),
        need(is.numeric(infileDataFrame$Outcome), "Please check if your column
            'Outcome' is numeric"),
        
        
        need(length(infileDataFrame$Treatment)>4, "Please check if your column
            'Treatment' has at least length four"),
        need(length(infileDataFrame$Outcome)>4, "Please check if your column
            'Outcome' has at least length four"),
        need(sum(infileDataFrame$Treatment!=0&infileDataFrame$Treatment!=1)==0, 
             "Data entries in the Treatment Column have to be either 1 or 0."),
        need(sum(infileDataFrame$Outcome!=0&infileDataFrame$Outcome!=1)==0, 
             "Data entries in the Outcome Column have to be either 1 or 0."))
      
      validate(need(sum(infileDataFrame$Treatment==1&infileDataFrame$Outcome==1)>0&
                      sum(infileDataFrame$Treatment==1&infileDataFrame$Outcome==0)>0&
                      sum(infileDataFrame$Treatment==0&infileDataFrame$Outcome==1)>0&
                      sum(infileDataFrame$Treatment==0&infileDataFrame$Outcome==0)>0, 
                    "You need at least one observation from each of the four possible 
                    Treatment/Outcome combinations."))
      
      DataF <- data.frame(Treatment=infileDataFrame$Treatment,
                          Y=infileDataFrame$Outcome)
      
    } 
    
    
   
    # Depending on the choices the user did in the user interface, we
    # will need different twodimensional dataframe
    # We use the inputvalues to extract them from the four-dimensional
    # matrix
    
    
    
    pasteform <- "ATEmatrix["
    
    if (input$xAxis!="alpha"&input$yAxis!="alpha"){
      print(input$alpha)
      pasteform <- paste(pasteform,"1",sep="")
    }
    
    pasteform <- paste(pasteform,",",sep="")
    
    if (input$xAxis!="beta"&input$yAxis!="beta"){
      print(input$beta)
      pasteform <- paste(pasteform,"1",sep="")
    }
    
    pasteform <- paste(pasteform,",",sep="")
    
    if (input$xAxis!="gamma"&input$yAxis!="gamma"){
      print(input$gamma)
      pasteform <- paste(pasteform,"1",sep="")
    }
    
    pasteform <- paste(pasteform,",",sep="")
    
    if (input$xAxis!="q"&input$yAxis!="q"){
      print(input$q)
      pasteform <- paste(pasteform,"1",sep="")
    }
    
    pasteform <- paste(pasteform,"]",sep="")
    
    #check the text for the index (will be seen in the console output)
    
    if(input$xAxis=="alpha"){
      var1range<-alpha1range
    } else if(input$xAxis=="beta"){
      var1range<-beta1range
    } else if(input$xAxis=="gamma"){
      var1range<-gamma1range
    } else if(input$xAxis=="q"){
      var1range<-qrange
    }
    
    if(input$yAxis=="alpha"){
      var2range<-alpha1range
    } else if(input$yAxis=="beta"){
      var2range<-beta1range
    } else if(input$yAxis=="gamma"){
      var2range<-gamma1range
    } else if(input$yAxis=="q"){
      var2range<-qrange
    }
    
    fixedpara<-","
    if (input$xAxis!="alpha" & input$yAxis!="alpha"){
      fixedpara<-paste(fixedpara, paste("alpha_1 = ",input$alpha))}
    if (input$xAxis!="beta" & input$yAxis!="beta"){
      fixedpara<-paste(fixedpara,paste("beta_1 = ",input$beta))}
    if (input$xAxis!="gamma" & input$yAxis!="gamma"){
      fixedpara<-paste(fixedpara,paste("gamma_1 = ",input$gamma))}
    if (input$xAxis!="q" & input$yAxis!="q"){
      fixedpara<-paste(fixedpara,paste("q = ",input$q))}
    
    reacdata1 <- list(DataF,DataSens, pasteform, var1range, var2range, fixedpara)
    reacdata1
    
  }) %>% debounce(3000)
  
  # Not that we have the correct data, it's time for plotting the TippingPointPlot
  
  
  
  
  plotInput <- function(reacDat=ReacData1()){
    
    DataF <- reacDat[[1]]
    DataSens <- reacDat[[2]]
    pasteform <- reacDat[[3]]
    var1range <- reacDat[[4]]
    var2range <- reacDat[[5]]
    fixedpara <- reacDat[[6]]
    
    
    alpha1vector<-DataSens$alpha1vector
    beta1vector<-DataSens$beta1vector
    gamma1vector<- DataSens$gamma1vector 
    qvector<-DataSens$qvector
    
    alpha1range<-DataSens$alpha1range
    beta1range<-DataSens$beta1range
    gamma1range<-DataSens$gamma1range
    qrange<-DataSens$qrange
    
    arraypara <- array(dim = c(steps,steps,steps,steps,3))
    
    
    
    for (i in 1:steps){
      incProgress(i/12)
      for (j in 1:steps){
        for (k in 1:steps){
          for (l in 1:steps){
            
            alpha1 <- alpha1vector[i]
            beta1 <- beta1vector[j]
            gamma1 <- gamma1vector[k]
            q <- qvector[l]
            p <- mean(DataF$Treatment)
            
            # Gamma is extra
            
            # Function Option
            # Possibility of specifying the probability of getting into the
            # Treatment or Control group if U=1
            
            
            # Default: Specifying gamma1 and estimating gamma0
            
            
            # Estimate gamma0 (see page 501)
            f1 <- function(x){
              q*exp(x + gamma1)/(1 + exp(x + gamma1)) + (1-q)*exp(x)/(1 + exp(x))-p
            }
            
            gamma0 <- uniroot(f1,interval=c(-100,100), tol = .Machine$double.eps^0.5)$root
            
            
            # Function parts that we will need to estimate alpha0, beta0 etc.
            q1 <- exp(gamma0 + gamma1)/(1 + exp(gamma0 + gamma1))
            q2 <- exp(gamma0)/(1 + exp(gamma0))
            q3 <- 1/(1 + exp(gamma0 + gamma1))
            q4 <- 1/(1 + exp(gamma0))
            
            ptheo1 <- q*q1 + (1-q)*q2
            ptheo0 <- q*(q3) + (1-q)*(q4)
            
            # Estimate alpha0 (see page 502 RR)
            
            mittelwertT <- mean(DataF$Y[DataF$Treatment==1])
            
            f2 <- function(x){
              ((q*q1/(ptheo1))*exp(x + alpha1)/(1 + exp(x + alpha1)) + ((1-q)*q2/ptheo1)*exp(x)/(1 + exp(x)))-mittelwertT
            }
            
            alpha0 <- uniroot(f2,interval=c(-100,100), tol = .Machine$double.eps^0.5)$root
            
            # Estimate beta0 (see page 502 RR)
            
            mittelwertC <- mean(DataF$Y[DataF$Treatment==0])
            
            f3 <- function(x){
              (q*(q3)/(ptheo0))*exp(x + beta1)/(1 + exp(x + beta1)) + ((1-q)*(q4)/ptheo0)*exp(x)/(1 + exp(x))-mittelwertC
            }
            
            beta0 <- tryCatch(uniroot(f3,interval=c(0,1),extendInt = "yes", tol = .Machine$double.eps^0.5)$root, error=function(e) 0)
            
            # Save results
            
            arraypara[i,j,k,l,1] <- gamma0
            arraypara[i,j,k,l,2] <- alpha0
            arraypara[i,j,k,l,3] <- beta0
          }
        }
      }
    }
    
    ### Calculation of treatment effect (ATE)
    
    # Calcute mu_t1, mu_t0, mu_c1, mu_c0
    
    ATEmatrix <- array(dim = c(steps,steps,steps,steps))
    for (i in 1:steps){
      for (j in 1:steps){
        for (k in 1:steps){
          for (l in 1:steps){
            
            # Go through all the possible combinations
            
            alpha1 <- alpha1vector[i]
            beta1 <- beta1vector[j]
            gamma1 <- gamma1vector[k]
            q <- qvector[l]
            
            gamma0 <- arraypara[i,j,k,l,1]
            alpha0 <- arraypara[i,j,k,l,2]
            beta0 <- arraypara[i,j,k,l,3]
            
            
            # Function elements that will be used
            
            pOT01 <- exp(alpha0 + alpha1*1)/(1 + exp(alpha0 + alpha1*1))
            pOT00 <- exp(alpha0 + alpha1*0)/(1 + exp(alpha0 + alpha1*0))
            
            pOT11 <- exp(beta0 + beta1*1)/(1 + exp(beta0 + beta1*1))
            pOT10 <- exp(beta0 + beta1*0)/(1 + exp(beta0 + beta1*0))
            
            q1 <- exp(gamma0 + gamma1)/(1 + exp(gamma0 + gamma1))
            q2 <- exp(gamma0)/(1 + exp(gamma0))
            q3 <- 1/(1 + exp(gamma0 + gamma1))
            q4 <- 1/(1 + exp(gamma0))
            
            ptheo1 <- q*q1 + (1-q)*q2
            ptheo0 <- q*(q3) + (1-q)*(q4)
            
            # Calculation the four mus
            
            mut1 <- (q*q1/(ptheo1))*pOT01 + ((1-q)*q2/ptheo1)*pOT00
            muc0 <- (q*(q3)/(ptheo0))*pOT11 + ((1-q)*(q4)/ptheo0)*pOT10
            
            mut0 <- (q*(1-q1)/(ptheo0))*pOT01 + ((1-q)*(1-q2)/ptheo0)*pOT00
            muc1 <- (q*(q1)/(ptheo1))*pOT11 + ((1-q)*(q2)/ptheo1)*pOT10
            
            ATE <- ptheo1*(mut1-muc1) + (1-ptheo1)*(mut0-muc0)
            
            ATEmatrix[i,j,k,l] <- ATE
            if(beta0==0){
              ATEmatrix[i,j,k,l] <- NA
            }
            
          }
        }
      }
    }
    
    # We now have a big matrix with four dimensions from which
    # we will get the data for the App
    
    # Now we will build the subfunction to get the correct data for
    # our Tipping Point Plot
    
    
    
    
    ATEmatrixdf <- eval(parse(text=pasteform))
    
    
    
    

    
    
    SensPara <- DataSens
    alpha1vector <- SensPara$alpha1vector
    beta1vector <- SensPara$beta1vector
    gamma1vector <- SensPara$gamma1vector
    qvector <- SensPara$qvector
    
    alpha1range <- SensPara$alpha1range
    beta1range <- SensPara$beta1range
    gamma1range <- SensPara$gamma1range
    qrange <- SensPara$qrange
    
    
    
    
    if(input$yAxis=="alpha"|input$yAxis=="beta"&input$xAxis!="alpha"|
       input$yAxis=="gamma"&input$xAxis=="q"){
      ATEmatrixdf<-t(ATEmatrixdf)
    }
    print(ATEmatrixdf)
 
    
    
    ## Sensitivity Analaysis Plot (see Liublinska Tipping Point Code! only slightly adapted here)
    
    ATEmatrixdf <- melt(ATEmatrixdf)
    validate(need(nrow(ATEmatrixdf)<10000, "One moment please. Loading..."))
    print(nrow(ATEmatrixdf))
    ATEmatrixdf$Var1 <- rep(seq(from = var1range[1],
                                to = var1range[2],
                                length.out = steps),steps)
    ATEmatrixdf$Var2 <- sort(rep(seq(var2range[1],
                                     var2range[2],
                                     length.out = steps),
                                 steps))
    
    TippingPointPlot <- ggplot(ATEmatrixdf, 
                               aes(x=Var1, y= Var2))
    
    
    TippingPointPlot <- TippingPointPlot + 
      geom_tile(aes(fill = round(value,3))) # Tile
    if(input$filling=="range"){
      if(min(ATEmatrixdf$value)>0)
        TippingPointPlot <- TippingPointPlot + 
          scale_fill_gradient(low="white", 
                              high="orange", 
                              na.value = "black", 
                              space="rgb") #color gradient
      if(max(ATEmatrixdf$value)<0)
        TippingPointPlot <- TippingPointPlot + 
          scale_fill_gradient(low="steelblue", 
                              high="white", 
                              na.value = "black", 
                              space="rgb") #color gradietn
      if(max(ATEmatrixdf$value)>0 & min(ATEmatrixdf$value)<0)
        TippingPointPlot <- TippingPointPlot + 
          ggtitle("Average Treatment Effect (Sensitivity Analysis)") +
          scale_fill_gradient2(low="steelblue", 
                               high="orange", 
                               na.value = "black", 
                               space="rgb") #color gradietn
    }
    if(input$filling=="zerotomax")
      TippingPointPlot <- TippingPointPlot + 
      scale_fill_gradient2(low="steelblue", 
                           high="orange", 
                           mid="white", 
                           midpoint=0, 
                           na.value = "black", 
                           space="rgb") #color gradietn
    
    
    TippingPointPlot <- TippingPointPlot + 
      geom_text(aes(x = Var1,
                    y = Var2, 
                    label = round(value,3)), 
                color = "black", alpha = 1, size = 3)+
      guides(fill=guide_legend(title="Value"))
    
    
    TippingPointPlot <- TippingPointPlot + 
      scale_x_continuous(name = input$xAxis, 
                         breaks = round(seq(var1range[1],
                                            var1range[2],
                                            length.out = steps),2), 
                         labels = round(seq(var1range[1],
                                            var1range[2],
                                            length.out = steps),2)) +
      scale_y_continuous(name = input$yAxis, 
                         breaks = round(seq(var2range[1],
                                            var2range[2],
                                            length.out = steps),2), 
                         labels = round(seq(var2range[1],
                                            var2range[2],
                                            length.out = steps),2)) + theme_bw() + 
      theme(axis.text.x =  element_text(colour = "black"),
            axis.text.y = element_text(colour = "black"))
    
    # Parameter Information for the Static Plots

    
    
    TippingPointPlot <- TippingPointPlot + 
      ggtitle(paste0("Sensitivity Analysis of treatment effects",fixedpara))
    
    TippingPointPlot
  }
  
  #Create an outputPlot object
  
  output$SensPlot <- renderPlot({
    withProgress(message = "Loading, will take about 4 seconds",expr = {print(plotInput())})
  })
  
  
  
  output$slideralpha <-renderUI({
    sliderInput("alpha", "Value for alpha_1",
                -10, 10, -0.7, step = 0.01)
  })
  
  
  
  output$sliderbeta <-renderUI({
    sliderInput("beta", "Value for beta_1",
                -10, 10, 0, step = 0.01)
  })
  
  
  output$slidergamma <-renderUI({
    sliderInput("gamma", "Value for gamma_1",
                -10, 10, 0, step = 0.01)
  })
  
  
  output$sliderq <-renderUI({
    sliderInput("q", "Value for q",
                0, 1, 0.5, step = 0.01)
  })
  
  output$sliderrangealpha <-renderUI({
    sliderInput("alpha1range", "Range for alpha_1",
                -10, 10, value=c(-1,1), step = 0.01)
  })
  
  
  
  output$sliderrangebeta <-renderUI({
    sliderInput("beta1range", "Range for beta_1",
                -10, 10, value=c(-1,1), step = 0.01)
  })
  
  
  output$sliderrangegamma <-renderUI({
    sliderInput("gamma1range", "Range for gamma_1",
                -10, 10, value=c(-1,1), step = 0.01)
  })
  
  
  output$sliderrangeq <-renderUI({
    sliderInput("qrange", "Range for q",
                0, 1, value=c(0.4,0.6), step = 0.01)
  })
  
  
  
  output$downloadPlot <- downloadHandler(
    filename = "Shinyplot.png",
    content = function(file) {
      png(file, width = 1024/2, height = 768/2)
      print(plotInput())
      dev.off()
    })   
  
  
}


