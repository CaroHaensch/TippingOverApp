library(shiny)
library(ggplot2)
library(reshape2)

# Default values
alpha1range <- c(-0.693,0.693)
beta1range <- c(-0.693,0.693)
gamma1range <- c(-0.693,0.693)
qrange <- c(0.1,1)
steps <- 10

alpha1vector <- seq(alpha1range[1],alpha1range[2],length.out = steps)
beta1vector <- seq(beta1range[1],beta1range[2],length.out = steps)
gamma1vector <- seq(gamma1range[1],gamma1range[2],length.out = steps)
qvector <- seq(qrange[1],qrange[2],length.out = steps)

fluidPage(
  

  column(12,
  # Title
  titlePanel("TippingSens App for Rosenbaum-Rubin Sensitivity Analyses")),
  column(12,wellPanel(
    plotOutput("SensPlot")
  )
  ),

  # Selection of the x-Axis and the y-Axis
  column(5, wellPanel(
    helpText(" Choose which sensitivity parameters should be displayed on the axes of the output (the other two parameters will be treated as fixed).
 "),

    selectInput("xAxis", " ",
                choices = c("alpha",
                            "beta","gamma","q"),
                selected = "alpha"
    ),
    selectInput("yAxis", " ",
                choices = c("alpha",
                            "beta","gamma","q"),
                selected = "beta"
    )),
    
   
    
    wellPanel(
      helpText("Choose a range for the parameters displayed on the axes."),
      conditionalPanel("input.xAxis == 'gamma'|input.yAxis == 'gamma'",
                       uiOutput("sliderrangegamma")),
      conditionalPanel("input.xAxis == 'alpha'|input.yAxis == 'alpha'",
                       uiOutput("sliderrangealpha")),
      conditionalPanel("input.xAxis == 'beta'|input.yAxis == 'beta'",
                       uiOutput("sliderrangebeta")),
      conditionalPanel("input.xAxis == 'q'|input.yAxis == 'q'",
                       uiOutput("sliderrangeq"))
    ),

    wellPanel(
      helpText("Chose a value for the parameters treated as fixed."),


      conditionalPanel("input.xAxis != 'gamma'&&input.yAxis != 'gamma'",
                       uiOutput("slidergamma")),
      conditionalPanel("input.xAxis != 'alpha'&&input.yAxis != 'alpha'",
                       uiOutput("slideralpha")),
      conditionalPanel("input.xAxis != 'beta'&&input.yAxis != 'beta'",
                       uiOutput("sliderbeta")),
      conditionalPanel("input.xAxis != 'q'&&input.yAxis != 'q'",
                       uiOutput("sliderq"))
    )),

  column(7, wellPanel(
       fileInput('datafile', 'Choose CSV File for the data (treatment and outcome). ',
              accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')), 
        helpText("Take care to specify column names correctly. The column names should be
            'Treatment' and 'Outcome'. No rownames or missing values are allowed. Separators are allowed to be comma, point or tab. Decimal separators are allowed to be comma or point. "),
       helpText("An example file can be found here (https://github.com/CaroHaensch/TSExample). "),
       
    tags$hr(),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t')),

    radioButtons('dec', 'Decimal separator',
                 c('Point'=".", 'Comma'=","))
    
    
  ),
  
  wellPanel(
    selectInput("filling", "The colour filling",
                choices = c("range", "zerotomax"),
                selected = "range"
    ),
    helpText("When choosing range the highest value will be orange and the lowest white, when choosing zerotomax negative values will be blue, positive ones orange and values near zero are white.")
    ),
  wellPanel(
    helpText("alpha - Log odds ratio of the confounder regarding the outcome in the treatment group "),
    helpText("beta - Log odds ratio of the confounder regarding the outcome in the control group "),
    helpText("gamma - Log odds ratio of the confounder regarding the treatment assignment "),
    helpText("q Prevelance of the binary confounder ")
  )
  )

  )
