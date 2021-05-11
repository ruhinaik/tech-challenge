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

  
  observe({
    if (input$tabselected == 3) {
      df <- file_data()
      df <- file_data() %>% clean_names()
      dsnames = names(df)
      cb_options <- list()
      cb_options[dsnames] <- dsnames
      updateRadioButtons(
        session,
        "xaxisGrpHist",
        label = "Variable of Interest:",
        choices = cb_options,
        selected = ""
      )
    }
  })
  
  
  output$histogram <- renderTable({
    print("Plotting histogram.")
    df <- file_data()
    df <- df %>% clean_names()

    if (!is.null(df)) {
      xv <- input$xaxisGrpHist
      if (!is.null(xv)) {
        gp <- df %>%
          count(df$xv) 

      }
    }
   return(gp)
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
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$title, 'merged_data.csv', sep = '-')
    },
    content = function(file) {
      write.csv(file_data(), file, na = "")
    }
  )
  
  observe({
    if (input$tabselected == 1) {
      df <- file_data()
      df <- file_data() %>% clean_names()
      dsnames = names(df)
      cb_options <- list()
      cb_options[dsnames] <- dsnames
      updateRadioButtons(
        session,
        "xaxisGrp",
        label = "Variable 1 of Interest:",
        choices = cb_options,
        selected = ""
      )
      updateCheckboxGroupInput(
        session,
        "yaxisGrp",
        label = "Variable 2 of Interest:",
        choices = cb_options,
        selected = ""
      )
      message("Exploratory data analysis plot has been selected.")
    }
  })
  
  
  output$exploratory_plot <- renderPlot({
    print("Plotting exploratory plot.")
    df <- file_data()
    df <- df %>% clean_names()
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp
      yv <- input$yaxisGrp
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
  
  
  # only look at counts of selected variable
  observe({
    if (input$tabselected == 3) {
      df <- file_data()
      df <- file_data() %>% clean_names()
      dsnames = names(df)
      cb_options <- list()
      cb_options[dsnames] <- dsnames
      updateRadioButtons(
        session,
        "xaxisGrpHist",
        label = "Variable 1 of Interest:",
        choices = cb_options,
        selected = ""
      )
      message("Counting plot has been selected.")
    }
  })
  
  
  output$counts_plot <- renderPlot({
    df <- file_data()
    df <- subset(df, select = -c(STUDYID))
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp3
      if (!is.null(xv)) {
        if (sum(xv %in% names(df)) > 0) {
          # suppresses error when changing files
          mdf <- melt(df, id.vars = xv)

          mdf <- cbind(mdf, count = 1)
          print(mdf)
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
  
  # heatmap plot
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
  
  
  exp_plot <- reactive ({
    print("Plotting exploratory plot.")
    df <- file_data()
    df <- df %>% clean_names()
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp
      yv <- input$yaxisGrp
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
  
  rand_plot <- reactive ({
    df <- simple_randomized_data()
    
    df <- subset(df, select = -c(0))
    gp <- NULL
    if (!is.null(df)) {
      xv <- input$xaxisGrp2
      yv <- input$yaxisGrp2
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
  
  # download each plot you make as a PNG (under the exploratory tab)
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
     makeDataReport(df)
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
  
  gen_plots <- reactive({
    df <- file_data()
    df <- subset(df, select = -c(1))
    dsnames = colnames(df)
    print(names(df))
    cb_options <- list()
    cb_options[dsnames] <- dsnames
    patients <- toString(input$pat_columns)
    
    plot_list = list()
    len = length(cb_options) 
    
    for (i in 1:len){
      # for each column name, make a counts plot
      col_num1 <- which(colnames(df) == cb_options[[i]])
      col_num2 <- which(colnames(df) == toString(input$pat_columns))
      p <- df %>%
        count(df[col_num1], df[col_num2]) %>%
        ggplot() +
        geom_col(mapping = aes_string(x=cb_options[[i]], y="n", fill=cb_options[[i]]),  
                 position = "stack") +
        ylab("counts") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
      plot_list[[i]] = p
    }
    plot_list
  })
  
  
  output$downloadPlots <- downloadHandler(
    filename = "plots.pdf",
    
    content = function(file){
      pdf(file=file)
      print(gen_plots())
      
      dev.off()
    }
  )
  
})