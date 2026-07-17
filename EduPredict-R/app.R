# ==============================================================================
# APPLICATION: EduPredict Hub (Fully Optimized & Premium UI Production Edition)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

# 1. BULLETPROOF DATA INGESTION ENGINE
file_name <- "Final_Student_Dataset.csv"

if (file.exists(file_name)) {
  raw_data <- read.csv(file_name, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(raw_data) <- trimws(colnames(raw_data))
  
  # Data Sanitization: Missing text fields ko clean karna
  student_data <- as.data.frame(lapply(raw_data, function(x) {
    x[is.na(x) | x == "null" | x == "NULL"] <- ""
    return(as.character(x))
  }), stringsAsFactors = FALSE)
  
  colnames(student_data) <- colnames(raw_data)
  file_error <- FALSE
} else {
  file_error <- TRUE
  student_data <- data.frame(
    studentId = character(0), class = character(0), firstName = character(0),
    percentage = character(0), passFailStatus = character(0), attendancePercentage = character(0),
    grade = character(0), stringsAsFactors = FALSE
  )
}

# 2. ROLE-BASED ACCESS CONTROL
users_db <- data.frame(
  username = c("admin", "teacher1"),
  password = c("admin123", "teacher123"),
  role = c("Admin", "Teacher"),
  assigned_class = c("All", "Class 1"),
  stringsAsFactors = FALSE
)

# 3. PREMIUM USER INTERFACE (UI ARCHITECTURE)
ui <- dashboardPage(
  skin = "red", 
  dashboardHeader(title = "EduPredict Hub", titleWidth = 260),
  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview Insights", tabName = "dashboard_panel", icon = icon("chart-pie"))
    )
  ),
  dashboardBody(
    # ADVANCED PREMIUM UI STYLING RULES (CSS OVERRIDES)
    tags$head(tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
      
      /* Global Layout Clean Up */
      body, .content-wrapper, .right-side { background-color: #f8fafc !important; font-family: 'Inter', sans-serif !important; }
      .main-header .logo { font-family: 'Inter', sans-serif !important; font-weight: 700 !important; background-color: #0f172a !important; color: #ffffff !important; border-bottom: 1px solid #1e293b; }
      .main-header .navbar { background-color: #0f172a !important; }
      .left-side, .main-sidebar { background-color: #0f172a !important; padding-top: 15px; }
      .sidebar-menu > li.active > a { border-left-color: #0ea5e9 !important; background: #1e293b !important; color: #ffffff !important; }
      .sidebar-menu > li > a { color: #94a3b8 !important; font-weight: 500; }
      
      /* Secure Portal Component */
      .login-wrapper { display: flex; justify-content: center; align-items: center; padding-top: 120px; }
      .login-box-custom { width: 420px; background: #ffffff; padding: 40px; border-radius: 12px; box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.1); border-top: 5px solid #0ea5e9; }
      .login-title { font-weight: 700; color: #0f172a; margin-bottom: 25px; text-align: center; font-size: 24px; letter-spacing: -0.025em; }
      .form-control { border-radius: 6px !important; border: 1px solid #cbd5e1 !important; padding: 10px 12px !important; height: auto !important; box-shadow: none !important; }
      .form-control:focus { border-color: #0ea5e9 !important; box-shadow: 0 0 0 2px rgba(14, 165, 233, 0.2) !important; }
      
      /* Premium Executive Metric Cards */
      .premium-card { background: #ffffff; padding: 22px 24px; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); display: flex; align-items: center; justify-content: space-between; border: 1px solid #e2e8f0; margin-bottom: 20px; transition: transform 0.2s; }
      .premium-card:hover { transform: translateY(-2px); box-shadow: 0 4px 6px -1px rgba(0,0,0,0.05); }
      .premium-card-meta { display: flex; flex-direction: column; }
      .premium-card-value { font-size: 28px; font-weight: 700; color: #0f172a; letter-spacing: -0.05em; line-height: 1.2; }
      .premium-card-label { font-size: 13px; font-weight: 500; color: #64748b; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.05em; }
      .premium-card-icon { font-size: 32px; padding: 12px; border-radius: 10px; }
      .bg-icon-blue { color: #0ea5e9; background: #f0f9ff; }
      .bg-icon-green { color: #10b981; background: #ecfdf5; }
      .bg-icon-yellow { color: #f59e0b; background: #fffbeb; }
      .bg-icon-purple { color: #8b5cf6; background: #f5f3ff; }
      
      /* Modular Container Layouts */
      .box { background: #ffffff !important; border: 1px solid #e2e8f0 !important; border-top: none !important; border-radius: 12px !important; box-shadow: 0 1px 3px rgba(0,0,0,0.05) !important; margin-bottom: 24px !important; overflow: hidden; }
      .box-header.with-border { border-bottom: 1px solid #f1f5f9 !important; padding: 16px 20px !important; background: #ffffff; }
      .box-title { font-size: 16px !important; font-weight: 600 !important; color: #0f172a !important; font-family: 'Inter', sans-serif !important; }
      
      /* Data System Ledger Grid */
      .dt-container { background: #ffffff; padding: 20px; border-radius: 12px; border: 1px solid #e2e8f0; }
      table.dataTable thead th { background-color: #f8fafc !important; color: #475569 !important; font-weight: 600 !important; text-transform: uppercase; font-size: 11px !important; letter-spacing: 0.05em; padding: 12px 16px !important; border-bottom: 1px solid #e2e8f0 !important; }
      table.dataTable tbody td { padding: 12px 16px !important; color: #334155 !important; font-size: 13px !important; border-bottom: 1px solid #f1f5f9 !important; }
    "))),
    uiOutput("page_content")
  )
)

# 4. SERVER SYSTEM BUSINESS LOGIC
server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE, Role = NULL, Name = NULL, AssignedClass = NULL)
  
  # Secure Portal Template View
  login_ui <- function() {
    div(class = "login-wrapper",
        div(class = "login-box-custom",
            div(class = "login-title", "EduPredict System Portal"),
            textInput("userName", "Institutional Email / Username:"),
            passwordInput("password", "Security Access Key:"),
            br(),
            actionButton("login_btn", "Secure Sign In", class = "btn-info btn-block btn-lg", style = "background-color: #0ea5e9 !important; border:none; font-weight:600; border-radius:6px; height: 45px;"),
            uiOutput("error_msg")
        )
    )
  }
  
  # Session Validation Interceptor
  observeEvent(input$login_btn, {
    req(input$userName, input$password)
    row_match <- users_db[users_db$username == input$userName & users_db$password == input$password, ]
    
    if (nrow(row_match) == 1) {
      USER$Logged <- TRUE
      USER$Role <- row_match$role
      USER$Name <- row_match$username
      USER$AssignedClass <- row_match$assigned_class
    } else {
      output$error_msg <- renderUI({ 
        div(style = "color: #ef4444; margin-top: 15px; font-weight: 600; text-align: center; font-size: 13px;", 
            icon("circle-exclamation"), " Authentication failed. Check credentials.") 
      })
    }
  })
  
  # Interactive Dashboard View Canvas
  output$page_content <- renderUI({
    if (!USER$Logged) return(login_ui())
    
    class_choices <- "All Classes"
    if (!file_error && "class" %in% colnames(student_data)) {
      class_choices <- c("All Classes", sort(unique(as.character(student_data[["class"]]))))
    }
    
    fluidPage(
      fluidRow(
        style = "padding: 10px 5px 0px 5px; margin-bottom: 15px;",
        column(8, 
               h2("Academic Performance Dashboard", style = "margin: 0; font-weight: 700; color: #0f172a; letter-spacing: -0.025em;"),
               p("Institutional predictive analysis framework & intelligence center.", style = "color: #64748b; margin-top: 4px; font-size: 14px;")
        ),
        column(4, actionButton("logout_btn", "Disconnect", class = "btn-default", style = "float: right; margin-top: 10px; font-weight: 500; border-radius: 6px; border: 1px solid #cbd5e1; color: #334155; padding: 8px 16px;"))
      ),
      
      if (file_error) {
        fluidRow(box(width = 12, status = "danger", "Database linkage down. 'Final_Student_Dataset.csv' unresolved."))
      },
      
      # PREMIUM EXECUTIVE SUMMARY CARDS MAP
      if (!file_error && USER$Role == "Admin") {
        fluidRow(
          column(3, uiOutput("ui_card_students")),
          column(3, uiOutput("ui_card_pass")),
          column(3, uiOutput("ui_card_attendance")),
          column(3, uiOutput("ui_card_class"))
        )
      },
      
      # Strategy Filter Elements
      if (!file_error) {
        fluidRow(
          box(title = "System Workspace Scoping Filters", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(6, 
                       if (USER$Role == "Admin") {
                         selectInput("selected_class", "Selected Academic Stream:", choices = class_choices)
                       } else {
                         selectInput("selected_class", "Selected Academic Stream:", choices = USER$AssignedClass)
                       }
                ),
                column(6, 
                       textInput("search_input", "Target Record Registry (Student ID Lookup):", value = "", placeholder = "e.g. STD982837")
                )
              )
          )
        )
      },
      
      # CHARTS VISUALIZATION LAYER
      if (!file_error && USER$Role == "Admin") {
        fluidPage(
          style = "padding: 0;",
          fluidRow(
            box(title = "Pass/Fail Distribution Index", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("plot_pass_fail", height = "320px")),
            box(title = "Stream Performance Comparison Analytics", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("plot_class_perf", height = "320px"))
          ),
          fluidRow(
            box(title = "Co-relation Study: Attendance Velocity vs Score Metrics", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("plot_attendance_marks", height = "320px")),
            box(title = "Grade Densities Stacked Matrix", status = "primary", solidHeader = TRUE, width = 6,
                plotlyOutput("plot_grade_dist", height = "320px"))
          )
        )
      },
      
      # Data Ledger Frame
      fluidRow(
        column(12,
               h3("Global Data Records Ledger", style = "color: #0f172a; font-weight: 700; font-size: 18px; letter-spacing: -0.025em; margin: 15px 0 15px 5px;"),
               div(class = "dt-container",
                   DTOutput("student_table")
               )
        )
      )
    )
  })
  
  observeEvent(input$logout_btn, { USER$Logged <- FALSE })
  
  # Reactive Data Context Stream
  filtered_data <- reactive({
    req(USER$Logged)
    df <- student_data
    if (file_error || nrow(df) == 0) return(df)
    
    if (!is.null(input$selected_class) && input$selected_class != "All Classes" && "class" %in% colnames(df)) {
      df <- df[df[["class"]] == input$selected_class, ]
    }
    
    if (!is.null(input$search_input) && input$search_input != "" && "studentId" %in% colnames(df)) {
      search_term <- tolower(trimws(input$search_input))
      df <- df[tolower(as.character(df[["studentId"]])) == search_term, ]
    }
    
    return(df)
  })
  
  # ---------------- CORE EXECUTIVE METRIC ENGINE ----------------
  
  output$ui_card_students <- renderUI({
    div(class = "premium-card",
        div(class = "premium-card-meta",
            div(class = "premium-card-value", nrow(filtered_data())),
            div(class = "premium-card-label", "Active Registries")
        ),
        div(class = "premium-card-icon bg-icon-blue", icon("users"))
    )
  })
  
  output$ui_card_pass <- renderUI({
    df <- filtered_data()
    pass_pct <- "0.0%"
    if (nrow(df) > 0 && "passFailStatus" %in% colnames(df)) {
      pass_count <- sum(tolower(trimws(df[["passFailStatus"]])) == "pass", na.rm = TRUE)
      pass_pct <- paste0(round((pass_count / nrow(df)) * 100, 1), "%")
    }
    div(class = "premium-card",
        div(class = "premium-card-meta",
            div(class = "premium-card-value", pass_pct),
            div(class = "premium-card-label", "Pass Validation Rate")
        ),
        div(class = "premium-card-icon bg-icon-green", icon("circle-check"))
    )
  })
  
  # ADVANCED FIXED ATTENDANCE PROCESSOR (Removes % Signs Automatically)
  output$ui_card_attendance <- renderUI({
    df <- filtered_data()
    avg_att <- "0.0%"
    
    if (nrow(df) > 0 && "attendancePercentage" %in% colnames(df)) {
      # 1. Text data mein se % nikal kar string clean karna
      clean_strings <- gsub("%", "", df[["attendancePercentage"]])
      # 2. Convert to real numbers securely
      num_attendance <- as.numeric(clean_strings)
      # 3. Handle mathematical mean safely
      mean_att <- mean(num_attendance, na.rm = TRUE)
      avg_att <- paste0(round(ifelse(is.nan(mean_att), 0, mean_att), 1), "%")
    }
    
    div(class = "premium-card",
        div(class = "premium-card-meta",
            div(class = "premium-card-value", avg_att),
            div(class = "premium-card-label", "Mean Attendance")
        ),
        div(class = "premium-card-icon bg-icon-yellow", icon("clock"))
    )
  })
  
  output$ui_card_class <- renderUI({
    df <- student_data
    top_class_name <- "N/A" # Yahan par := ko badal kar <- kar diya hai
    if (nrow(df) > 0 && "class" %in% colnames(df) && "percentage" %in% colnames(df)) {
      df$numeric_pct <- as.numeric(df[["percentage"]])
      class_means <- aggregate(numeric_pct ~ class, data = df, FUN = mean, na.rm = TRUE)
      top_class_name <- if(nrow(class_means) > 0) class_means$class[which.max(class_means$numeric_pct)] else "N/A"
    }
    div(class = "premium-card",
        div(class = "premium-card-meta",
            div(class = "premium-card-value", top_class_name),
            div(class = "premium-card-label", "Peak Alpha Stream")
        ),
        div(class = "premium-card-icon bg-icon-purple", icon("award"))
    )
  })
  
  # ---------------- CORE PLATFORM PLOTLY GRAPHIC RENDERS ----------------
  
  output$plot_pass_fail <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "passFailStatus" %in% colnames(df))
    
    status_counts <- as.data.frame(table(df[["passFailStatus"]]))
    colnames(status_counts) <- c("Status", "Count")
    
    plot_ly(status_counts, labels = ~Status, values = ~Count, type = 'pie', hole = 0.6,
            textinfo = 'label+percent',
            marker = list(colors = c('#10b981', '#ef4444'), line = list(color = '#ffffff', width = 2))) %>%
      layout(showlegend = FALSE, 
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(l=20, r=20, t=20, b=20))
  })
  
  output$plot_class_perf <- renderPlotly({
    df <- raw_data 
    req(nrow(df) > 0, "class" %in% colnames(df), "percentage" %in% colnames(df))
    
    df$percentage <- as.numeric(df$percentage)
    class_perf <- aggregate(percentage ~ class, data = df, FUN = mean, na.rm = TRUE)
    class_perf$percentage <- round(class_perf$percentage, 1)
    
    plot_ly(class_perf, x = ~class, y = ~percentage, type = 'bar',
            marker = list(color = '#0ea5e9')) %>%
      layout(xaxis = list(title = "", tickfont = list(color = '#64748b')),
             yaxis = list(title = "Performance Yield (%)", gridcolor = '#f1f5f9', tickfont = list(color = '#64748b')),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(l=40, r=20, t=20, b=40))
  })
  
  output$plot_attendance_marks <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "attendancePercentage" %in% colnames(df), "percentage" %in% colnames(df))
    
    plot_df <- data.frame(
      Attendance = as.numeric(gsub("%", "", df[["attendancePercentage"]])),
      Percentage = as.numeric(df[["percentage"]]),
      ID = df[["studentId"]],
      stringsAsFactors = FALSE
    )
    
    plot_ly(plot_df, x = ~Attendance, y = ~Percentage, type = 'scatter', mode = 'markers',
            text = ~paste("ID:", ID, "<br>Attendance:", Attendance, "%<br>Score:", Percentage, "%"),
            hoverinfo = 'text', marker = list(size = 7, color = '#f59e0b', opacity = 0.6, line = list(color = '#ffffff', width = 1))) %>%
      layout(xaxis = list(title = "Attendance Matrix (%)", gridcolor = '#f1f5f9', tickfont = list(color = '#64748b')),
             yaxis = list(title = "Result Yield (%)", gridcolor = '#f1f5f9', tickfont = list(color = '#64748b')),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(l=40, r=20, t=20, b=40))
  })
  
  output$plot_grade_dist <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "grade" %in% colnames(df))
    
    grade_counts <- as.data.frame(table(df[["class"]], df[["grade"]]))
    colnames(grade_counts) <- c("Class", "Grade", "Count")
    
    plot_ly(grade_counts, x = ~Class, y = ~Count, type = 'bar', color = ~Grade,
            colors = c("#0ea5e9", "#10b981", "#f59e0b", "#64748b", "#ef4444")) %>%
      layout(barmode = 'stack',
             xaxis = list(title = "", tickfont = list(color = '#64748b')),
             yaxis = list(title = "Volume Pool", gridcolor = '#f1f5f9', tickfont = list(color = '#64748b')),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)',
             margin = list(l=40, r=20, t=20, b=40))
  })
  
  # Enterprise Safe Table Platform
  output$student_table <- renderDT({
    req(filtered_data())
    datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        autoWidth = FALSE,
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100),
        dom = "lrtip"
      )
    )
  })
}

shinyApp(ui, server)