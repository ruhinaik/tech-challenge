# server.R

shinyServer(function(input, output, session) {
  
  #reading in files and merging them on first column
  file_data <- reactive({
    req(input$file1) # require that the input files are available
    req(input$file2) 
    inFile1 <- input$file1
    inFile2 <- input$file2
    file1_df <- read.delim(inFile1$datapath)
    file2_df <- read.delim(inFile2$datapath)
    merged_df <- merge(file1_df, file2_df, on="STUDYID")
    return(merged_df)
  })
  
  output$contents <- renderTable({
    file_data()
  })
  
 
  output$text1 <- renderText({
    df <- file_data()
    paste(
      "This is a table for a study involving ",
      nrow(df),
      "samples."
    )
  })

  
  info <- sessionInfo()
  
  output$version <- renderText({
    paste(info$R.version[c(13, 2)]$version.string,
          info$R.version[c(13, 2)]$arch,
          sep = ", ")
  })
  
  
  observe({
    if (input$tabselected == 1) {
      df <- file_data()
      df <- file_data() %>% clean_names()
      dsnames = names(df)
      cb_options <- list()
      cb_options[dsnames] <- dsnames
      updateRadioButtons(
        session,
        "xaxisGrp1",
        label = "Variable 1 of Interest:",
        choices = cb_options,
        selected = ""
      )
      updateCheckboxGroupInput(
        session,
        "yaxisGrp1",
        label = "Variable 2 of Interest:",
        choices = cb_options,
        selected = ""
      )
      message("Exploratory data analysis plot has been selected.")
    }
  })
  
  # rendering bar plots under tab 2 
  output$exploratory_plot <- renderPlot({
    print("Plotting exploratory plot.")
    df <- file_data()
    df <- df %>% clean_names()
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp1
      yv <- input$yaxisGrp1
      if (!is.null(xv) & !is.null(yv)) {
        if (sum(xv %in% names(df)) > 0) {
          # suppresses error when changing files
          
          # SAMPLE LEVEL PLOTS 
          mdf <- melt(df, id.vars = xv, measure.vars = yv)
          
          mdf <- cbind(mdf, count = 1)
          
          gp <- mdf %>%
            ggplot(aes_string(
              x = xv,
              y = "count",
              fill = "value",
              label = "count"
            )) +
            geom_col(position = "stack")
        }
      } 
    }
    return(gp)
  })

  
  # correlation plot
  correlation_plot <- reactive({
    df <- file_data()
    df <- file_data() %>% clean_names()
    
    # removing columns with all NA values
    df <- df[, colSums(is.na(df)) < nrow(df)]
    df[is.na(df)] <- 0
    
    # removing columns with less than 2 factor levels 
    df <- df[, sapply(df, function(col) length(unique(col))) > 1]
    names = colnames(df)
   
    
    formula = paste0(names, collapse = " + ")
    formula = paste("~", formula)

    # compute correlation for the variables in the names variable
    canCorPairs(formula, df)

    # make upper triangular heatmap
    corrmatrix <- corrplot(canCorPairs(formula, df), type = "upper")
    
    return(corrmatrix)
  })
  
  output$correlations <- renderPlot({
    correlation_plot()
  })
  
  output$choose_columns <- renderUI({
    if (is.null(input$dataset))
      return()
    colnames <- names(contents)
    checkboxGroupInput("columns",
                       "Choose columns",
                       choices  = colnames,
                       selected = colnames)
  })

  
  # bar plots
  exp_plot <- reactive ({
    print("Plotting bar plot.")
    df <- file_data()
    df <- df %>% clean_names()
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp1
      yv <- input$yaxisGrp1
      if (!is.null(xv) & !is.null(yv)) {
        if (sum(xv %in% names(df)) > 0) {
          # supresses error when changing files
          mdf <- melt(df, id.vars = xv, measure.vars = yv)
          
          mdf <- cbind(mdf, count = 1)

          gp <- mdf %>%
            ggplot(aes_string(
              x = xv,
              y = "count",
              fill = "value",
              label = "count"
            )) +
            geom_col(position = "stack")
        }
      }
    }
    return(gp)
  })
  
  output$exp_plot <- renderPlot({
    req(exp_plot())
    exp_plot()
  })
  

  # capability to download each plot you make as a PNG (under the 2nd tab)
  observe ({
    output$downloader1 <- downloadHandler(
      filename = function() {
        title_name <- make_clean_names(toString(input$title))
        paste(title_name, "_exploratory_plot.png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = exp_plot(), device = "png", width = 20, height = 10)
      }
    )
    
  })
  
   dataReport <- reactive({
    req(input$file1)
     inFile  <- input$file1
     df <- read.delim(inFile$datapath)
     makeDataReport(df, replace=TRUE)
  })
   

  output$downloadDataReport <- downloadHandler(
    filename = function(){
      title_name <- make_clean_names(toString(input$title))
      paste(title_name, "_report.pdf", sep = "")
    },
    
    content = function(file) {
      print(dataReport())
      file.copy('dataReporter_df.pdf', file, overwrite = TRUE)
    }
  )
  
  
  output$downloadFile <- downloadHandler(
    filename = function() {
      paste(input$title, 'merged_data.csv', sep = '-')
    },
    content = function(file) {
      write.csv(file_data(), file, row.names=FALSE)
    }
  )
  
  
})
