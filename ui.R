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
  titlePanel("TippingSens App for Rosenbaum-Rubin Sensitivity Analyses"),

  helpText("Beware: Loading might take about ten seconds.")
  ),
  column(12,wellPanel(
    plotOutput("SensPlot")
  )
  ),

  # Selection of the x-Axis and the y-Axis
  column(5, wellPanel(
    helpText("You have to choose which sensitivity parameters shall be displayed on the x-Axis and y-Axis. The other two parameters will be fixed."),

    selectInput("xAxis", "xAxis",
                choices = c("alpha",
                            "beta","gamma","q"),
                selected = "alpha"
    ),
    selectInput("yAxis", "yAxis",
                choices = c("alpha",
                            "beta","gamma","q"),
                selected = "beta"
    )),

    wellPanel(
      helpText("Here you choose a value for the two other parameters that are not displayed on the axes."),


      conditionalPanel("input.xAxis != 'gamma'&&input.yAxis != 'gamma'",
                       uiOutput("slidergamma")),
      conditionalPanel("input.xAxis != 'alpha'&&input.yAxis != 'alpha'",
                       uiOutput("slideralpha")),
      conditionalPanel("input.xAxis != 'beta'&&input.yAxis != 'beta'",
                       uiOutput("sliderbeta")),
      conditionalPanel("input.xAxis != 'q'&&input.yAxis != 'q'",
                       uiOutput("sliderq"))
    ),


    wellPanel(
    selectInput("torg", "Gamma values or probability of treatment/control?",
                choices = c("gamma", "probtreatment", "probcontrol"),
                selected = "gamma"
    ),
    helpText("Instead of specifying a range of values for gamma1, one can also choose to specify a range of values for the probability of treatment OR control when U==1. 
             However, the default lower range is -0.7 for the gamma/probtreatment/probcontrol axis, so please upload your own sensitivity parameter ranges on the right.")),

    wellPanel(
    selectInput("filling", "The colour filling.",
                choices = c("range", "zerotomax"),
                selected = "range"
    ),
    helpText("When choosing range the lowest value will be white, when choosing zerotomax only cells with the value zero will be white.")

  )),

  column(7, wellPanel(
    helpText("Take care to specify corrects column names. \n For the data frame, the column names  have to
            'Treatment' and 'Outcome'. For the sensitivity parameter the column names have to be 'alpha1range', 'beta1range', 'gamma1range' and
             'qrange'."),
    helpText("Example files can be found here (https://github.com/CaroHaensch/TSExample)."),
    helpText("No rownames or missing values are allowed.
              Separators are allowed to be comma, point or tabular. Decimal separators are allowed to be comma or point."),
    fileInput('datafile', 'Choose CSV File for the data (treatment and outcome). ',
              accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv')),
    tags$hr(),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t')),

    radioButtons('dec', 'Decimal separator',
                 c('Point'=".", 'Comma'=","))
  ),
  wellPanel(
  fileInput('rangefile', 'Choose CSV File for the range of the sensitivity parameters (alpha, beta, gamma, qrange).',
            accept=c('text/csv', 'text/comma-separated- values,text/plain', '.csv'))
  ,
  tags$hr(),
  radioButtons('sep2', 'Separator',
               c(Comma=',',
                 Semicolon=';',
                 Tab='\t')),

  radioButtons('dec2', 'Decimal separator',
               c('Point'=".", 'Comma'=","))
  )
  )

  )
