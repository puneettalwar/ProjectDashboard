library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)
library(SmartEDA)
library(gtsummary)
library(DataExplorer)

ui <- dashboardPage(
  dashboardHeader(title = "IRONSLEEP Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tables", tabName = "tables", icon = icon("database")),
      menuItem("Column Selection", tabName = "columns", icon = icon("columns")),
      menuItem("Variable Types", tabName = "vartypes", icon = icon("sliders-h")),
      menuItem("Summary Table", tabName = "summary", icon = icon("table")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")),
      #menuItem("Plots", tabName = "plots", icon = icon("chart-line")),
      menuItem("Upload / Input", tabName = "upload", icon = icon("upload"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Data Input", width = 12, status = "primary", solidHeader = TRUE,
                    fileInput("file", "Upload CSV/Excel File", accept = c(".csv", ".xlsx")),
                    #textInput("fixed_path", "Or provide fixed file path (auto-refresh)", value = ""),
                    actionButton("load_data", "Load Data")
                )
              )
      ),
      tabItem(tabName = "columns",
              fluidRow(
                box(title = "Column Selection", width = 12, status = "info", solidHeader = TRUE,
                    uiOutput("select_columns_ui"),
                    actionButton("apply_column_selection", "Apply Column Selection")
                )
              )
      ),
      tabItem(tabName = "vartypes",
              fluidRow(
                box(title = "Variable Type Selection", width = 12, status = "info", solidHeader = TRUE,
                    uiOutput("factor_vars_ui"),
                    uiOutput("numeric_vars_ui"),
                    actionButton("apply_var_types", "Apply Variable Types")
                )
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary Table", width = 12, status = "primary", solidHeader = TRUE,
                    gt::gt_output("gtsummary_table")
                )
              )
      ),
      tabItem(tabName = "eda",
              fluidRow(
                box(title = "Dataset Overview", width = 12, status = "info", solidHeader = TRUE,
                    tableOutput("eda_intro")
                )
              ),
              fluidRow(
                box(title = "Missing Values", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("eda_missing")
                )
              ),
              fluidRow(
                box(title = "Distributions (Numeric)", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("eda_hist")
                )
              ),
              # fluidRow(
              #   box(title = "Bar Charts (Categorical)", width = 12, status = "primary", solidHeader = TRUE,
              #       plotOutput("eda_bar")
              #   )
              # ),
              fluidRow(
                box(
                  title = "Bar Charts (Categorical Variables)",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  uiOutput("barplot_page_selector"),
                  plotOutput("eda_bar")
                )
              ),
              
              fluidRow(
                box(title = "Correlation Heatmap", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("eda_corr")
                )
              )
      ),
      
      # tabItem(tabName = "eda",
      #         fluidRow(
      #           box(title = "Automated EDA Report", width = 12, status = "info", solidHeader = TRUE,
      #               htmlOutput("smarteda_report")
      #           )
      #         )
      # ),
      # tabItem(tabName = "plots",
      #         fluidRow(
      #           box(title = "Plot", width = 12, status = "primary", solidHeader = TRUE,
      #               selectInput("xvar", "X-axis variable", choices = NULL),
      #               selectInput("yvar", "Y-axis variable", choices = NULL),
      #               plotOutput("plot")
      #           )
      #         )
      # ),
      tabItem(tabName = "tables",
              fluidRow(
                box(title = "Data Table", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("datatable")
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  

  data_cache <- "data_cache.rds"  # cache file path
  
  rv <- reactiveValues(data = NULL, selected_data = NULL, fixed_path = NULL)
  
  isolate({
    if (file.exists(data_cache)) {
      df <- readRDS(data_cache)
      rv$data <- df
      rv$selected_data <- df
    }
  })
  
  
  # data_cache_path <- "data_cache.rds"  # cache file path
  # 
  # rv <- reactiveValues(data = NULL, selected_data = NULL, fixed_path = NULL)
  # 
  # isolate({
  #   if (file.exists(data_cache_path)) {
  #     df <- readRDS(data_cache_path)
  #     rv$data <- df
  #     rv$selected_data <- df
  #   }
  # })
  
  
  # Reactive file reader if fixed path provided
  observeEvent(input$load_data, {
    # User uploaded file
    if (!is.null(input$file)) {
      ext <- tools::file_ext(input$file$name)
      if (ext == "csv") {
        df <- read.csv(input$file$datapath)
      } else if (ext == "xlsx") {
        df <- readxl::read_excel(input$file$datapath)
      } else {
        showNotification("Unsupported file type", type = "error")
        return()
      }
      rv$data <- as.data.frame(df)
      rv$selected_data <- rv$data
      #rv$fixed_path <- NULL
      #saveRDS(rv$data, data_cache_path)
      saveRDS(rv$data, data_cache)
      showNotification("Data loaded from upload", type = "message")
    }
      else {
        showNotification("File not found", type = "error")
      }
  })

  
  # Column selection
  output$select_columns_ui <- renderUI({
    req(rv$data)
    checkboxGroupInput(
      "selected_columns", 
      "Select Columns to Keep:",
      choices = names(rv$data), 
      selected = names(rv$data)
    )
  })
  
  observeEvent(input$apply_column_selection, {
    req(rv$data, input$selected_columns)
    rv$selected_data <- rv$data[, input$selected_columns, drop = FALSE]
    showNotification("Column selection applied", type = "message")
  })
  
  # Variable type selectors
  output$factor_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput('factor_vars', 'Variables to treat as Categorical / Factor', choices = names(rv$selected_data), multiple = TRUE)
  })
  output$numeric_vars_ui <- renderUI({
    req(rv$selected_data)
    selectInput('numeric_vars', 'Variables to treat as Numeric', choices = names(rv$selected_data), multiple = TRUE)
  })
  
  # Apply variable types
  observeEvent(input$apply_var_types, {
    req(rv$selected_data)
    df <- rv$selected_data
    if (!is.null(input$factor_vars) && length(input$factor_vars) > 0) {
      for (v in input$factor_vars) {
        if (v %in% names(df)) {
          df[[v]] <- as.factor(df[[v]])
        }
      }
    }
    if (!is.null(input$numeric_vars) && length(input$numeric_vars) > 0) {
      for (v in input$numeric_vars) {
        if (v %in% names(df)) {
          df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
        }
      }
    }
    rv$selected_data <- df
    showNotification('Variable types applied', type = 'message')
  })
  
  # gtsummary table
  output$gtsummary_table <- gt::render_gt({
    df <- rv$selected_data
    req(df)
    df %>% tbl_summary(by = Participant,
                       missing = "no",
                       digits= everything() ~ 2,
                       # custom statistic formats ----
                       statistic = list(all_continuous() ~ "{mean} ({sd})",
                                        all_categorical() ~ "{p}% ({n} / {N})")
    ) %>% # don't list missing data separately
      add_n() %>% # add column with total number of non-missing observations
      #add_p() %>%  # for Wilcoxon rank sum test and chi square 
      add_p(
        test=list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test")
      ) %>%
      modify_header(label = "Variable") %>% # update the column header
      bold_labels() %>%
      as_gt()
  })
  
  
  
  # EDA overview table
  output$eda_intro <- renderTable({
    df <- rv$selected_data
    req(df)
    introduce(df)
  })
  
  # Missing data plot
  output$eda_missing <- renderPlot({
    df <- rv$selected_data
    req(df)
    plot_missing(df)
  })
  
  # Numeric distributions
  output$eda_hist <- renderPlot({
    df <- rv$selected_data
    req(df)
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) > 0) {
      plot_histogram(df, ncol = 3)
    }
  })
  
  # # Categorical distributions
  # output$eda_bar <- renderPlot({
  #   df <- rv$selected_data
  #   req(df)
  #   cat_vars <- names(df)[sapply(df, is.factor)]
  #   if (length(cat_vars) > 0) {
  #     plot_bar(df, ncol = 3)
  #   }
  # })
  
  
  # UI for barplot page selection
  output$barplot_page_selector <- renderUI({
    df <- rv$selected_data
    req(df)
    cat_vars <- names(df)[sapply(df, is.factor)]
    if (length(cat_vars) == 0) return(NULL)
    
    vars_per_page <- 6
    total_pages <- ceiling(length(cat_vars) / vars_per_page)
    
    if (total_pages > 1) {
      sliderInput(
        "barplot_page",
        "Barplot Page:",
        min = 1,
        max = total_pages,
        value = 1,
        step = 1,
        round = TRUE
      )
    }
  })
  
  # Render categorical barplots with pagination
  output$eda_bar <- renderPlot({
    df <- rv$selected_data
    req(df)
    cat_vars <- names(df)[sapply(df, is.factor)]
    if (length(cat_vars) == 0) return(NULL)
    
    vars_per_page <- 6
    total_pages <- ceiling(length(cat_vars) / vars_per_page)
    
    current_page <- input$barplot_page
    if (is.null(current_page)) current_page <- 1
    
    start_idx <- (current_page - 1) * vars_per_page + 1
    end_idx <- min(current_page * vars_per_page, length(cat_vars))
    
    vars_to_plot <- cat_vars[start_idx:end_idx]
    
    plot_bar(df[, vars_to_plot, drop = FALSE], ncol = 3)
  })
  
  
  # Correlation plot for numeric variables
  output$eda_corr <- renderPlot({
    df <- rv$selected_data
    req(df)
    num_vars <- df[sapply(df, is.numeric)]
    if (ncol(num_vars) > 1) {
      plot_correlation(num_vars, type = "continuous")
    }
  })
  
  
  
  
  
  # # SmartEDA report (no browser auto-open, with color theme)
  # output$smarteda_report <- renderUI({
  #   df <- rv$selected_data
  #   if (is.null(df)) return(HTML("<p>No data loaded</p>"))
  #   tmpfile <- tempfile(fileext = ".html")
  #   #SmartEDA::ExpReport(df, op_file = tmpfile, op_dir = tempdir())
  #   DataExplorer::create_report(df, output_file = tmpfile, output_dir = tempdir())
  #   includeHTML(tmpfile)
  # })
  # 
  # 
  # # Update plot selectors
  # observe({
  #   df <- rv$selected_data
  #   if (!is.null(df)) {
  #     updateSelectInput(session, "xvar", choices = names(df))
  #     updateSelectInput(session, "yvar", choices = names(df))
  #   }
  # })
  
  # # Plot
  # output$plot <- renderPlot({
  #   df <- rv$selected_data
  #   if (is.null(df) || input$xvar == "" || input$yvar == "") return(NULL)
  #   ggplot(df, aes_string(x = input$xvar, y = input$yvar)) +
  #     geom_point() +
  #     theme_minimal()
  # })
  
  # Data table with scroll
  output$datatable <- renderDT({
    df <- rv$selected_data
    if (is.null(df)) return(NULL)
    datatable(df, options = list(scrollX = TRUE, pageLength = 10))
  })
}

shinyApp(ui, server)