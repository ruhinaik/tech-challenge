# ui.R
library(janitor)
library(shiny)
library(reshape)
library(plotly)
library(plyr)
library(dplyr)
library(readxl)
#library(srvyr)
library(tidyverse)
#install.packages("corrplot")
library(corrplot)
library(devtools)
library(variancePartition)
#devtools::install_github("taiyun/corrplot")
library(ggthemes)
library(ggplot2); theme_set(theme_clean())
#install.packages("dataReporter")
#devtools::install_github("ekstroem/dataReporter")
library("dataReporter")
library(knitr)
library(grid)
library(gridExtra)
                   
# Define UI for slider demo application
shinyUI(pageWithSidebar(
  #  Application title
  headerPanel("Randomization for NGS experiments"),
  
  sidebarPanel(
   
    textInput("title", "Set your study title:", "Study name"),
    # numericInput("ncases", label = "Total number of samples:", value = 200),
    # numericInput("seed", label = "Random seed:", value = 43298),
    
    selectInput(
      "batches",
      label = "Number of batches:",
      choices = c("1", "2", "3", "4", "5", "6")
    ),
    
    # simple or stratified versions of randomization 
    selectInput(
      "randomization_type",
      label = "Specify the type of randomization:",
      choices = c("Simple", "Stratified"),
      inputId = "block_type"
    ),
    
    # select if you want 48/96 samples per batch
    selectInput(
      label = "Specify wells per plate:",
      choices = c("48", "96"),
      inputId = "plate_size"
    ),
    
    textOutput("text"),
    
    conditionalPanel(
      condition = "input.plate_size == 48 || input.plate_size == 96",
      selectInput("pat_columns", "Column containing patient IDs:", choices = NULL),
    ),
    
    
    # if stratified is selected, then you can choose from the columns in your file 
    # which variable to stratify on
    # and also which column represents the patient ids 
    conditionalPanel(
      condition = "input.block_type == 'Stratified'",
      uiOutput("block_vars"),
      selectInput("block_columns", "Block column", choices = NULL),
    ),
    
    # selecting a file to randomize on 
    fileInput(
      "file1",
      "Choose File",
      multiple = TRUE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv",
        ".tsv"
      )
    ),
    
    tags$hr(),
    
    
    conditionalPanel(
      condition = "input.tabselected==1",
      uiOutput("choose_columns"),
      downloadButton('downloader1', 'Download plot'),
      fluidRow(column(
        6, radioButtons("xaxisGrp", "Variable 1 of Interest:", c("1" = "1", "2" =
                                                                   "2"))
      ),
      column(
        6, checkboxGroupInput("yaxisGrp", "Variable 2 of Interest:", c("1" = "1", "2" =
                                                                         "2"))
      ))
    ),
    
    conditionalPanel(
      condition = "input.tabselected==2",
      uiOutput("choose_columns_randomized"),
      downloadButton('downloader2', 'Download plot'),
      fluidRow(column(
        6, radioButtons("xaxisGrp2", "Variable 1 of Interest:", c("1" = "1", "2" =
                                                                    "2"))
      ),
      column(
        6,
        checkboxGroupInput("yaxisGrp2", "Variable 2 of Interest:", c("1" = "1", "2" =
                                                                       "2"))
      ))
    ),
    
    conditionalPanel(
      condition = "input.tabselected==5",
      
      downloadButton('downloader3', 'Download plot'),
      radioButtons("xaxisGrpHist", "Variable of Interest:", c("1" = "1", "2" =
                                                                    "2"))
    ),
    
    textOutput("text1"),
    textOutput("version"),
    helpText("Randomized tables
             Written in R/Shiny by R. Naik."),
    downloadButton('downloadData', 'Download randomized file'),
    downloadButton('downloadDataReport', 'Download data report'),
    downloadButton('downloadPlots', 'Download plots')
  ),
  
  
  # Show a table summarizing the values entered
  mainPanel(
    tabsetPanel(
      tabPanel("File", value = 0, tableOutput("contents")),
      tabPanel("Randomized File",  value=4, tableOutput("contents3")),
      # tabPanel("Histogram", value=5, tableOutput("histogram")),
      tabPanel("Exploratory Plot", value = 1, plotOutput("exploratory_plot")),
      tabPanel(
        "Randomized Plot",
        value = 2,
        plotOutput("post_randomization_plot")
      ),
      tabPanel("Correlation Matrix", value = 3, plotOutput("heat_map")),
      # tabPanel("Disease Dist Plot", plotOutput("diseasedist_plot")),
      id = "tabselected"
    )
  )
))

# To execute this script from inside R, you need to have the ui.R and server.R files into the session's working directory. Then, type:
# runApp()
# To execute directly this Gist, from the Internet to your browser, type:
# shiny:: runGist(' ')