# ui.R
install.packages("janitor")

installAndLoad <- function(package, always_install = FALSE){
  if(!requireNamespace(package, quietly=TRUE) | always_install) install.packages(package,quiet=TRUE)
  library(package,character.only=TRUE, logical.return = TRUE)
    
}

packages <- c( "shiny", "ggthemes", "tidyverse", "dplyr", "plyr", 
               "plotly", "janitor", "reshape", "corrplot", "devtools", "BiocManager", 
               "dataReporter", "knitr", "grid", "gridExtra", "ggpubr")

stopifnot(any(sapply(packages, installAndLoad, always_install=FALSE)))
devtools::install_github("taiyun/corrplot", force=TRUE)
devtools::install_github("ekstroem/dataReporter", force=TRUE)
BiocManager::install("variancePartition", force=TRUE)
library(variancePartition)
library(ggplot2); theme_set(theme_clean())


shinyUI(pageWithSidebar(
  headerPanel(""),
  
  sidebarPanel(
   
    textInput("title", "Set your study title:", "Study name"),

    textOutput("text"),
    
    # select file 1
    fileInput(
      "file1",
      "Choose File",
      accept = c(
        "text/tsv",
        "text/tab-separated-values,text/plain",
        ".tsv"
      )
    ),
    
    # select file 2
    fileInput(
      "file2",
      "Choose File",
      accept = c(
        "text/tsv",
        "text/tab-separated-values,text/plain",
        ".tsv"
      )
    ),
    
    tags$hr(),
    
    conditionalPanel(
      condition = "input.tabselected==1",
      uiOutput("choose_columns"),
      downloadButton('downloader1', 'Download plot'),
      fluidRow(column(
        6, radioButtons("xaxisGrp1", "Variable 1 of Interest:", c("1" = "1", "2" =
                                                                   "2"))
      ),
      column(
        6, checkboxGroupInput("yaxisGrp1", "Variable 2 of Interest:", c("1" = "1", "2" =
                                                                         "2"))
      ))
    ),
    
    textOutput("text1"),
    textOutput("version"),
    helpText("Written in R/Shiny by R. Naik."),
    downloadButton('downloadFile', 'Download merged file'),
    downloadButton('downloadDataReport', 'Download data report')
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(
      tabPanel("File", value = 0, tableOutput("contents")),
      tabPanel("Exploratory Bar Plots", value = 1, plotOutput("exploratory_plot")),
      tabPanel("Correlation Matrix", value = 2, plotOutput("correlations")),
      id = "tabselected"
    )
  )
))

# To execute this script from inside R, you need to have the ui.R and server.R files into the session's working directory. Then, type:
# runApp()
# To execute directly this Gist, from the Internet to your browser, type:
# shiny:: runGist(' ')