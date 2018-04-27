### Rubin-Rosenbaum meets Liublinska App Function ---------


library(shiny)
library(ggplot2)
library(reshape2)




function(input, output){

    Var1<-Var2<-value<-NULL

    steps <- 10
    
    
   ReacData <- reactive({
    
      alpha1range <- c(-0.693,0.693)
      beta1range <- c(-0.693,0.693)
      gamma1range <- c(-0.693,0.693)
      qrange <- c(0.1,0.55)
      steps <- 10
      

    
    
    if (input$xAxis!="alpha" & input$yAxis!="alpha"){
      alpha1range <- c(input$alpha,input$alpha+0.05)
      validate(
        need(try(seq(alpha1range[1],alpha1range[2],length.out = steps)), "One moment please")
      )}
    if (input$xAxis!="beta" & input$yAxis!="beta"){
      beta1range <- c(input$beta,input$beta+0.05)
      validate(
        need(try(seq(beta1range[1],beta1range[2],length.out = steps)), " ")
      )}
    if (input$xAxis!="gamma" & input$yAxis!="gamma"){
      gamma1range <- c(input$gamma,input$gamma+0.05)
      validate(
        need(try(seq(gamma1range[1],gamma1range[2],length.out = steps)), " ")
        
      )
      validate(
        need(try(seq(gamma1range[1],gamma1range[2],length.out = steps)), " "),
        need((input$gamma> 0 & input$gamma< 1&
               input$torg == "probtreatment")|
               (input$gamma> 0 & input$gamma< 1& 
               input$torg == "probcontrol")|
               input$torg=="gamma", "No negative values for probability. Must be bigger than one and smaller than zero."))
      }
    if (input$xAxis!="q" & input$yAxis!="q"){
      qrange <- c(input$q,ifelse(input$q>0.5,input$q-0.0001,input$q+0.0001))
      validate(need(input$q<=1&input$q>=0, "No negative number or greater than one for proportion q."))
      validate(
        need(try(seq(gamma1range[1],gamma1range[2],length.out = steps)), " "),
        need(input$q<1&input$q>0&
               input$torg == "probtreatment"|
               input$q<1& input$q>0& 
               input$torg == "probcontrol"|
               input$torg=="gamma", "q must be smaller than one and bigger than zero when using probtreatment/probcontrol."))
      
           }
    
      
      rangefile <- input$rangefile
      
    if (!is.null(rangefile)) {
     
      infileRange <- read.csv(input$rangefile$datapath,
                              header=T,
                              sep=input$sep2,
                              dec = input$dec2,
                              row.names = NULL)
      validate(
        need(!is.null(input$alpha)|!is.null(input$beta)|!is.null(input$gamma)|!is.null(input$q),
             "One moment please..."),
        need(input$sep2!=input$dec2, 
             "Separator and decimal separator cannot be the same"),
        need(!is.null(infileRange$gamma1range), 
             "Please check if you have a column named 'gamma1range' in your 
             sensitivity parameter dataset"),
        need(!is.null(infileRange$alpha1range), "Please check if you have a 
             column named 'alpha1range' in your sensitivity parameter  dataset"),
        need(!is.null(infileRange$beta1range), "Please check if you have a 
             column named 'beta1range' in your sensitivity parameter  dataset"),
        need(!is.null(infileRange$qrange), "Please check if you have a column 
             named 'qrange' in your sensitivity parameter dataset"),
        
        need(infileRange$gamma1range[1]<infileRange$gamma1range[2], 
             "Please check if the range limits of gamma1 are in the right order: 
             frist lower limit, then higher limit."),
        need(infileRange$alpha1range[1]<infileRange$alpha1range[2], 
             "Please check if the range limits of alpha1 are in the right order: 
             frist lower limit, then higher limit."),
        need(infileRange$beta1range[1]<infileRange$beta1range[2], "Please check 
             if the range limits of beta1 are in the right order: frist lower 
             limit, then higher limit."),
        need(infileRange$qrange[1]<infileRange$qrange[2], "Please check if the 
             range limits of q are in the right order: frist lower limit, then 
             higher limit."),
        
        need(is.numeric(infileRange$gamma1range), "Please check if your column
             'gamma1range' is numeric"),
        need(is.numeric(infileRange$alpha1range), "Please check if your column
             'alpha1range' is numeric"),
        need(is.numeric(infileRange$beta1range), "Please check if your column
             'beta1range' is numeric"),
        need(is.numeric(infileRange$qrange), "Please check if your column
             'qrange' is numeric"),
        need(length(infileRange$gamma1range)==2, "Please check if your column
             'gamma1range' has length two"),
        need(length(infileRange$alpha1range)==2, "Please check if your column
             'alpha1range' has length two"),
        need(length(infileRange$beta1range)==2, "Please check if your column
             'beta1range' has length two"),
        need(length(infileRange$qrange)==2, "Please check if your column
             'qrange' has length two"),
        
        
        
        need(try(seq(alpha1range[1],alpha1range[2],length.out = steps)), "One moment please"),
        need(try(seq(beta1range[1],beta1range[2],length.out = steps)), " "),
        need(try(seq(gamma1range[1],gamma1range[2],length.out = steps)), " "),
        need(try(seq(qrange[1],qrange[2],length.out = steps)), " ")
        )

      if (input$xAxis=="gamma" | input$yAxis=="gamma"){
        validate(
          need((infileRange$gamma1range[1]> 0 & infileRange$gamma1range[2]< 1&
                 input$torg == "probtreatment")|
                 (infileRange$gamma1range[1]> 0 & infileRange$gamma1range[2]< 1& 
                 input$torg == "probcontrol")|
                 input$torg=="gamma", "No negative values for probability please. 
               Must be bigger than zero and smaller than one.  Check your range."))
        
      }
      if (input$xAxis=="q" | input$yAxis=="q"){
      validate(
        need((infileRange$qrange[2]<1&infileRange$qrange[1]>0&
               input$torg == "probtreatment")|
               (infileRange$qrange[2]<1& infileRange$qrange[1]>0)& 
               input$torg == "probcontrol"|
               input$torg=="gamma", "q must be smaller than one and bigger than 
             zero when using probtreatment/probcontrol for computational reasons."))
      }
      
      alpha1range <- infileRange$alpha1range
      beta1range <- infileRange$beta1range
      gamma1range <- infileRange$gamma1range
      qrange <- infileRange$qrange
      
      if (input$xAxis!="alpha" & input$yAxis!="alpha"){
        alpha1range <- c(input$alpha,input$alpha+0.05)
        validate(
          need(try(seq(alpha1range[1],alpha1range[2],length.out = steps)), "One moment please")
        )}
      if (input$xAxis!="beta" & input$yAxis!="beta"){
        beta1range <- c(input$beta,input$beta+0.05)
        validate(
          need(try(seq(beta1range[1],beta1range[2],length.out = steps)), " ")
        )}
      if (input$xAxis!="gamma" & input$yAxis!="gamma"){
        gamma1range <- c(input$gamma,input$gamma+0.05)
        validate(
          need(try(seq(gamma1range[1],gamma1range[2],length.out = steps)), " "),
          need(input$gamma>= 0 & input$gamma<= 1&
                 input$torg == "probtreatment"|
                 input$gamma>= 0 & input$gamma<= 1& 
                 input$torg == "probcontrol"|
                 input$torg=="gamma", "No negative values for probability. Please check the range."))
        
      }
      if (input$xAxis!="q" & input$yAxis!="q"){
        qrange <- c(input$q,ifelse(input$q>0.5,input$q-0.1,input$q+0.1))
        validate(need(input$q<=1&input$q>=0, "No negative number or greater than one for proportion q."))
      }
      
 
      
    }
    
   
    
    alpha1vector <- seq(alpha1range[1],alpha1range[2],length.out = steps)
    beta1vector <- seq(beta1range[1],beta1range[2],length.out = steps)
    gamma1vector <- seq(gamma1range[1],gamma1range[2],length.out = steps)
    qvector <- seq(qrange[1],qrange[2],length.out = steps)
    
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

    arraypara <- array(dim = c(steps,steps,steps,steps,3))

    
    
      
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
            'Treatment' at least length four"),
       need(length(infileDataFrame$Outcome)>4, "Please check if your column
            'Outcome' at least length four"),
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

    for (i in 1:steps){
      for (j in 1:steps){
        for (k in 1:steps){
          for (l in 1:steps){

            alpha1 <- alpha1vector[i]
            beta1 <- beta1vector[j]
            q <- qvector[l]
            p <- mean(DataF$Treatment)

            # Gamma is extra

            # Function Option
            # Possibility of specifying the probability of getting into the
            # Treatment or Control group if U=1

            if(input$torg == "probtreatment"){
            probtreat <- gamma1vector[k]
            probcontrol <- (q*probtreat-p)/(q-1)

            gamma0 <- log(-probcontrol/(probcontrol-1))
            gamma1 <- log(-probtreat/(probtreat-1))-gamma0
            }

            if(input$torg=="probcontrol"){
              probcontrol <- gamma1vector[k]
              probtreat <- (p + (q-1)*probcontrol)/q

              gamma0 <- log(-probcontrol/(probcontrol-1))
              gamma1 <- log(-probtreat/(probtreat-1))-gamma0
            }

            # Default: Specifying gamma1 and estimating gamma0
            if(input$torg == "gamma"){
            gamma1 <- gamma1vector[k]
            # Estimate gamma0 (see page 501)
            f1 <- function(x){
              q*exp(x + gamma1)/(1 + exp(x + gamma1)) + (1-q)*exp(x)/(1 + exp(x))-p
            }

            gamma0 <- uniroot(f1,interval=c(-100,100))$root
            }

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

            alpha0 <- uniroot(f2,interval=c(-100,100))$root

            # Estimate beta0 (see page 502 RR)

            mittelwertC <- mean(DataF$Y[DataF$Treatment==0])

            f3 <- function(x){
              (q*(q3)/(ptheo0))*exp(x + beta1)/(1 + exp(x + beta1)) + ((1-q)*(q4)/ptheo0)*exp(x)/(1 + exp(x))-mittelwertC
            }

            beta0 <- tryCatch(uniroot(f3,interval=c(0,1),extendInt = "yes")$root, error=function(e) 0)

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



      ### Optional error check used for troubleshooting
      # print(sum(exists("input$q"),exists("input$gamma"),exists("input$beta"),
      #          exists("input$alpha")))

      # Depending on the choices the user did in the user interface, we
      # will need different twodimensional dataframe
      # We use the inputvalues to extract them from the four-dimensional
      # matrix

      print(c(input$alpha,input$beta,input$gamma,input$q, input$xAxis,input$yAxis))

      pasteform <- "ATEmatrix["

      if (input$xAxis!="alpha"&input$yAxis!="alpha"){
        print(input$alpha)
        print(which(abs(input$alpha-alpha1vector)==min(abs(input$alpha-alpha1vector))))
        pasteform <- paste(pasteform,
                           which(abs(input$alpha-alpha1vector)==
                                   min(abs(input$alpha-alpha1vector))),sep="")
      }

      pasteform <- paste(pasteform,",",sep="")

      if (input$xAxis!="beta"&input$yAxis!="beta"){
        print(input$beta)
        print(which(abs(input$beta-beta1vector)==min(abs(input$beta-beta1vector))))
        pasteform <- paste(pasteform,
                           which(abs(input$beta-beta1vector)==
                                   min(abs(input$beta-beta1vector))),sep="")
      }

      pasteform <- paste(pasteform,",",sep="")

      if (input$xAxis!="gamma"&input$yAxis!="gamma"){
        print(input$gamma)
        print(which(abs(input$gamma-gamma1vector)==
                      min(abs(input$gamma-gamma1vector))))
        pasteform <- paste(pasteform,
                           which(abs(input$gamma-gamma1vector)==
                                    min(abs(input$gamma-gamma1vector))),sep="")
      }

      pasteform <- paste(pasteform,",",sep="")

      if (input$xAxis!="q"&input$yAxis!="q"){
        print(input$q)
        print(which(abs(input$q-qvector)==min(abs(input$q-qvector))))
        pasteform <- paste(pasteform,which(abs(input$q-qvector)==
                                             min(abs(input$q-qvector))),sep="")
      }

      pasteform <- paste(pasteform,"]",sep="")

      #check the text for the index (will be seen in the console output)
      print(pasteform)

      ATEmatrixdf <- eval(parse(text=pasteform))

      

      
      reacDat<-list(DataSens,ATEmatrixdf)
      reacDat
    })

    # We now have the correct data, now it's time for plotting the TippingPointPlot

    

    
    plotInput <- reactive({
      
      reacDat <- ReacData()
      SensPara <- reacDat[[1]]
      alpha1vector <- SensPara$alpha1vector
      beta1vector <- SensPara$beta1vector
      gamma1vector <- SensPara$gamma1vector
      qvector <- SensPara$qvector
      
      alpha1range <- SensPara$alpha1range
      beta1range <- SensPara$beta1range
      gamma1range <- SensPara$gamma1range
      qrange <- SensPara$qrange

      ATEmatrixdf <- reacDat[[2]]
      
 

      if(input$yAxis=="alpha"|input$yAxis=="beta"&input$xAxis!="alpha"|
         input$yAxis=="gamma"&input$xAxis=="q"){
        ATEmatrixdf<-t(ATEmatrixdf)
      }
      print(ATEmatrixdf)
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
                                 aes(x=Var1, y=Var2))


      TippingPointPlot <- TippingPointPlot + 
        geom_tile(aes(fill = value)) # Tile
      if(input$filling=="range"){
       if(min(ATEmatrixdf$value)>0)
         TippingPointPlot <- TippingPointPlot + 
           scale_fill_gradient(low="white", 
                               high="orange", 
                               na.value = "black", 
                               space="rgb") #color gradietn
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
                  color = "black", alpha = 0.9, size = 3)


      TippingPointPlot <- TippingPointPlot + 
        scale_x_continuous(name = input$xAxis) +
        scale_y_continuous(name = input$yAxis) 

      # Parameter Information for the Static Plots
      fixedpara<-","
      if (input$xAxis!="alpha" & input$yAxis!="alpha"){
        fixedpara<-paste(fixedpara, paste("alpha = ",input$alpha))}
      if (input$xAxis!="beta" & input$yAxis!="beta"){
        fixedpara<-paste(fixedpara,paste("beta = ",input$beta))}
      if (input$xAxis!="gamma" & input$yAxis!="gamma"){
        fixedpara<-paste(fixedpara,paste("gamma = ",input$gamma))}
      if (input$xAxis!="q" & input$yAxis!="q"){
        fixedpara<-paste(fixedpara,paste("q = ",input$q))}


       TippingPointPlot <- TippingPointPlot + 
         ggtitle(paste0("Sensitivity Analysis of treatment effects",fixedpara))

      TippingPointPlot
    })

    #Create an outputPlot object

    output$SensPlot <- renderPlot({
      print(plotInput())
    })

    
    output$slideralpha <-renderUI({
  
      numericInput("alpha", "Choose a value for alpha", 0.5, min = -10, max = 10)
    })
    
    
    
    output$sliderbeta <-renderUI({
      numericInput("beta", "Choose a value for beta", 0.5, min = -10, max = 10)
    })
    
    
    output$slidergamma <-renderUI({
      numericInput("gamma", "Choose a value for gamma (default) or probtreatment/probcontrol (if choosen below)", 0.5, min = -10, max = 10)
    })
    
    
    output$sliderq <-renderUI({
      numericInput("q", "Choose a value for q", 0.5, min = 0, max = 1)
    })
    

  }


