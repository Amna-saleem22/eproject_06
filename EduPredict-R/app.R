# ==============================================================================
# APPLICATION: EduPredict Hub (100% Functional & Fixed Reactive Version)
# ==============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)

# ------------------------------------------------------------------------------
# 1. DATA INGESTION ENGINE WITH SAMPLE FALLBACK
# ------------------------------------------------------------------------------
file_name <- "Final_Student_Dataset.csv"

normalize_cols <- function(df) {
  if (nrow(df) == 0) return(df)
  cnames <- colnames(df)
  cnames_lower <- tolower(gsub("[_ .]", "", cnames))
  
  col_map <- c(
    "studentid" = "studentId", "class" = "class", "firstname" = "firstName",
    "gender" = "gender", "percentage" = "percentage", "passfailstatus" = "passFailStatus",
    "attendancepercentage" = "attendancePercentage", "grade" = "grade",
    "maths" = "maths", "science" = "science", "english" = "english",
    "history" = "history", "geography" = "geography"
  )
  
  for (i in seq_along(cnames_lower)) {
    if (cnames_lower[i] %in% names(col_map)) {
      colnames(df)[i] <- col_map[[cnames_lower[i]]]
    }
  }
  return(df)
}

if (file.exists(file_name)) {
  raw_data <- read.csv(file_name, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(raw_data) <- trimws(colnames(raw_data))
  
  student_data <- as.data.frame(lapply(raw_data, function(x) {
    x[is.na(x) | x == "null" | x == "NULL"] <- ""
    return(as.character(x))
  }), stringsAsFactors = FALSE)
  
  student_data <- normalize_cols(student_data)
} else {
  student_data <- data.frame(
    studentId = paste0("STD", 1001:1010),
    class = c("Class 1", "Class 1", "Class 2", "Class 2", "Class 1", "Class 3", "Class 3", "Class 2", "Class 1", "Class 3"),
    firstName = c("Ali", "Sara", "Usman", "Ayesha", "Zain", "Hamza", "Fatima", "Bilal", "Hassan", "Sana"),
    gender = c("Male", "Female", "Male", "Female", "Male", "Male", "Female", "Male", "Male", "Female"),
    percentage = c("88", "92", "45", "78", "32", "65", "95", "54", "81", "38"),
    passFailStatus = c("Pass", "Pass", "Pass", "Pass", "Fail", "Pass", "Pass", "Pass", "Pass", "Fail"),
    attendancePercentage = c("90%", "95%", "60%", "85%", "50%", "75%", "98%", "68%", "88%", "55%"),
    grade = c("A", "A+", "D", "B", "F", "C", "A+", "D", "A", "F"),
    maths = c("85", "90", "40", "75", "30", "60", "98", "50", "80", "35"),
    science = c("90", "94", "48", "80", "35", "68", "92", "58", "82", "40"),
    english = c("88", "91", "45", "76", "31", "64", "96", "52", "80", "38"),
    history = c("86", "89", "42", "79", "33", "66", "94", "55", "81", "36"),
    geography = c("91", "95", "46", "82", "30", "67", "95", "55", "83", "41"),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------------
# 2. ROLE-BASED ACCESS CONTROL DATABASE
# ------------------------------------------------------------------------------
users_db <- data.frame(
  username = c("admin", "teacher1"),
  password = c("admin123", "teacher123"),
  role = c("Admin", "Teacher"),
  assigned_class = c("All", "Class 1"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 3. USER INTERFACE ARCHITECTURE
# ------------------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
    
    body, .content-wrapper, .right-side { background-color: #f8fafc !important; font-family: 'Inter', sans-serif !important; }
    .main-header .logo { font-family: 'Inter', sans-serif !important; font-weight: 700 !important; background-color: #0f172a !important; color: #ffffff !important; border-bottom: 1px solid #1e293b; }
    .main-header .navbar { background-color: #0f172a !important; }
    .left-side, .main-sidebar { background-color: #0f172a !important; padding-top: 15px; }
    .sidebar-menu > li.active > a { border-left-color: #0ea5e9 !important; background: #1e293b !important; color: #ffffff !important; }
    .sidebar-menu > li > a { color: #94a3b8 !important; font-weight: 500; }
    
    .login-wrapper { display: flex; justify-content: center; align-items: center; min-height: 80vh; }
    .login-box-custom { width: 420px; background: #ffffff; padding: 40px; border-radius: 12px; box-shadow: 0 10px 25px -5px rgba(0, 0, 0, 0.1); border-top: 5px solid #0ea5e9; }
    .login-title { font-weight: 700; color: #0f172a; margin-bottom: 25px; text-align: center; font-size: 24px; letter-spacing: -0.025em; }
    .form-control { border-radius: 6px !important; border: 1px solid #cbd5e1 !important; padding: 8px 12px !important; height: auto !important; box-shadow: none !important; }
    
    .premium-card { background: #ffffff; padding: 22px 24px; border-radius: 12px; box-shadow: 0 1px 3px rgba(0,0,0,0.05); display: flex; align-items: center; justify-content: space-between; border: 1px solid #e2e8f0; margin-bottom: 20px; }
    .premium-card-meta { display: flex; flex-direction: column; }
    .premium-card-value { font-size: 26px; font-weight: 700; color: #0f172a; letter-spacing: -0.05em; line-height: 1.2; }
    .premium-card-label { font-size: 12px; font-weight: 600; color: #64748b; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.05em; }
    .premium-card-icon { font-size: 28px; padding: 12px; border-radius: 10px; }
    .bg-icon-blue { color: #0ea5e9; background: #f0f9ff; }
    .bg-icon-green { color: #10b981; background: #ecfdf5; }
    .bg-icon-yellow { color: #f59e0b; background: #fffbeb; }
    .bg-icon-purple { color: #8b5cf6; background: #f5f3ff; }
    
    .status-badge { display: inline-block; padding: 8px 18px; border-radius: 20px; font-weight: 700; font-size: 13px; text-align: center; }
    .badge-safe { background-color: #d1fae5; color: #065f46; }
    .badge-attention { background-color: #fef3c7; color: #92400e; }
    .badge-risk { background-color: #fee2e2; color: #991b1b; }
    
    .box { background: #ffffff !important; border: 1px solid #e2e8f0 !important; border-top: none !important; border-radius: 12px !important; box-shadow: 0 1px 3px rgba(0,0,0,0.05) !important; margin-bottom: 24px !important; }
    .box-header.with-border { border-bottom: 1px solid #f1f5f9 !important; padding: 16px 20px !important; }
    .box-title { font-size: 16px !important; font-weight: 600 !important; color: #0f172a !important; }
    
    .dt-container { background: #ffffff; padding: 20px; border-radius: 12px; border: 1px solid #e2e8f0; }
    table.dataTable thead th { background-color: #f8fafc !important; color: #475569 !important; font-weight: 600 !important; text-transform: uppercase; font-size: 11px !important; padding: 12px 16px !important; }
    table.dataTable tbody td { padding: 12px 16px !important; color: #334155 !important; font-size: 13px !important; }
  "))),
  uiOutput("app_container")
)

# ------------------------------------------------------------------------------
# 4. SERVER SYSTEM BUSINESS LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  USER <- reactiveValues(Logged = FALSE, Role = NULL, Name = NULL, AssignedClass = NULL)
  
  # Login UI View
  login_ui <- function() {
    div(class = "login-wrapper",
        div(class = "login-box-custom",
            div(class = "login-title", "EduPredict System Portal"),
            textInput("userName", "Username:", value = "admin"),
            passwordInput("password", "Password:", value = "admin123"),
            br(),
            actionButton("login_btn", "Secure Sign In", class = "btn-info btn-block btn-lg", 
                         style = "background-color: #0ea5e9 !important; border:none; font-weight:600; border-radius:6px; height: 45px;"),
            uiOutput("error_msg")
        )
    )
  }
  
  # Main Dashboard View
  main_dashboard_ui <- function() {
    class_choices <- "All Classes"
    gender_choices <- c("All Genders", "Male", "Female")
    grade_choices <- "All Grades"
    
    if ("class" %in% colnames(student_data)) class_choices <- c("All Classes", sort(unique(as.character(student_data[["class"]]))))
    if ("grade" %in% colnames(student_data)) grade_choices <- c("All Grades", sort(unique(as.character(student_data[["grade"]]))))
    
    dashboardPage(
      skin = "red",
      dashboardHeader(title = "EduPredict Hub", titleWidth = 260),
      dashboardSidebar(
        width = 260,
        sidebarMenu(
          id = "tabs",
          menuItem("Overview Insights", tabName = "dashboard_panel", icon = icon("chart-pie")),
          menuItem("Student 360° Profile", tabName = "profile_panel", icon = icon("user-graduate")),
          menuItem("Advanced Analytics", tabName = "analytics_panel", icon = icon("chart-line")),
          menuItem("Risk & Top Performers", tabName = "tables_panel", icon = icon("users-viewfinder"))
        )
      ),
      dashboardBody(
        fluidRow(
          style = "padding: 10px 5px 0px 5px; margin-bottom: 15px;",
          column(8, 
                 h2("Academic Performance Intelligence Center", style = "margin: 0; font-weight: 700; color: #0f172a;"),
                 p("Institutional predictive analysis framework & executive reporting hub.", style = "color: #64748b; margin-top: 4px;")
          ),
          column(4, actionButton("logout_btn", "Disconnect", class = "btn-default", style = "float: right; margin-top: 10px; border-radius: 6px; border: 1px solid #cbd5e1;"))
        ),
        
        fluidRow(
          box(title = "Dynamic Filters Workspace", status = "primary", solidHeader = TRUE, width = 12,
              fluidRow(
                column(3, 
                       if (USER$Role == "Admin") selectInput("selected_class", "Academic Stream:", choices = class_choices)
                       else selectInput("selected_class", "Academic Stream:", choices = USER$AssignedClass)
                ),
                column(3, selectInput("filter_gender", "Gender Scope:", choices = gender_choices)),
                column(3, selectInput("filter_grade", "Grade Bucket:", choices = grade_choices)),
                column(3, textInput("search_input", "Student ID Registry Lookup:", value = "", placeholder = "e.g. STD1001"))
              )
          )
        ),
        
        tabItems(
          # TAB 1: OVERVIEW DASHBOARD
          tabItem(
            tabName = "dashboard_panel",
            if (USER$Role == "Admin") {
              fluidRow(
                column(3, uiOutput("ui_card_students")),
                column(3, uiOutput("ui_card_pass")),
                column(3, uiOutput("ui_card_attendance")),
                column(3, uiOutput("ui_card_class"))
              )
            },
            fluidRow(
              box(title = "Pass/Fail Distribution Index", status = "primary", width = 6, plotlyOutput("plot_pass_fail", height = "300px")),
              box(title = "Class Performance Benchmarks", status = "primary", width = 6, plotlyOutput("plot_class_perf", height = "300px"))
            ),
            fluidRow(
              box(title = "Grade Density Matrix", status = "primary", width = 12, plotlyOutput("plot_grade_dist", height = "300px"))
            )
          ),
          
          # TAB 2: INDIVIDUAL STUDENT 360° PROFILE
          tabItem(
            tabName = "profile_panel",
            fluidRow(
              box(
                title = "🎯 Select Student Profile", status = "primary", width = 12,
                fluidRow(
                  column(6, selectInput("profile_student_id", "Search or Select Student ID:", choices = sort(unique(as.character(student_data[["studentId"]]))), width = "100%")),
                  column(6, style = "margin-top: 25px; text-align: right;", uiOutput("ui_risk_badge"))
                )
              )
            ),
            fluidRow(
              column(3, uiOutput("ui_profile_att")),
              column(3, uiOutput("ui_profile_score")),
              column(3, uiOutput("ui_profile_grade")),
              column(3, uiOutput("ui_profile_class"))
            ),
            fluidRow(
              box(title = "🕸️ Subject Strength & Weakness Radar", status = "primary", width = 7, plotlyOutput("plot_student_radar", height = "380px")),
              box(title = "📋 Personal Details & Status", status = "primary", width = 5, tableOutput("table_student_info"))
            )
          ),
          
          # TAB 3: ADVANCED ANALYTICS
          tabItem(
            tabName = "analytics_panel",
            fluidRow(
              box(title = "Correlation: Attendance vs Scores", status = "primary", width = 6, 
                  plotlyOutput("plot_attendance_marks", height = "300px"),
                  uiOutput("correlation_text")),
              box(title = "Gender Performance Comparison", status = "primary", width = 6, plotlyOutput("plot_gender_comp", height = "300px"))
            ),
            fluidRow(
              box(title = "Subject-Wise Score Breakdown", status = "primary", width = 6, plotlyOutput("plot_subject_comp", height = "300px")),
              box(title = "Academic Performance Trend Lines", status = "primary", width = 6, plotlyOutput("plot_trend_analysis", height = "300px"))
            )
          ),
          
          # TAB 4: RISK & TOP PERFORMERS LEDGER
          tabItem(
            tabName = "tables_panel",
            fluidRow(
              box(title = "🏆 Top 10 High Performers", status = "success", width = 6, DTOutput("top_10_table")),
              box(title = "⚠️ Lowest Attendance At-Risk Watchlist", status = "danger", width = 6, DTOutput("lowest_attendance_table"))
            ),
            fluidRow(
              column(12,
                     h3("Global Data Ledger", style = "color: #0f172a; font-weight: 700; font-size: 18px;"),
                     div(class = "dt-container", DTOutput("student_table"))
              )
            )
          )
        )
      )
    )
  }
  
  # Dynamic Layout Switcher
  output$app_container <- renderUI({
    if (!USER$Logged) {
      login_ui()
    } else {
      main_dashboard_ui()
    }
  })
  
  # Login Authentication Logic
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
  
  # Logout Event
  observeEvent(input$logout_btn, { USER$Logged <- FALSE })
  
  observeEvent(USER$Logged, {
    if (USER$Logged && nrow(student_data) > 0 && "studentId" %in% colnames(student_data)) {
      id_choices <- sort(unique(as.character(student_data[["studentId"]])))
      updateSelectInput(session, "profile_student_id", choices = id_choices, selected = id_choices[1])
    }
  })
  
  # Reactive Filter Engine
  filtered_data <- reactive({
    req(USER$Logged)
    df <- student_data
    if (nrow(df) == 0) return(df)
    
    if (!is.null(input$selected_class) && input$selected_class != "All Classes" && "class" %in% colnames(df)) {
      df <- df[df[["class"]] == input$selected_class, ]
    }
    
    if (!is.null(input$filter_gender) && input$filter_gender != "All Genders" && "gender" %in% colnames(df)) {
      df <- df[tolower(df[["gender"]]) == tolower(input$filter_gender), ]
    }
    
    if (!is.null(input$filter_grade) && input$filter_grade != "All Grades" && "grade" %in% colnames(df)) {
      df <- df[df[["grade"]] == input$filter_grade, ]
    }
    
    if (!is.null(input$search_input) && trimws(input$search_input) != "" && "studentId" %in% colnames(df)) {
      search_term <- tolower(trimws(input$search_input))
      df <- df[tolower(as.character(df[["studentId"]])) == search_term, ]
    }
    
    return(df)
  })
  
  # Single Selected Student Record
  selected_student <- reactive({
    req(input$profile_student_id)
    df <- student_data
    if (nrow(df) == 0) return(NULL)
    
    res <- df[as.character(df[["studentId"]]) == as.character(input$profile_student_id), ]
    if (nrow(res) > 0) return(res[1, ]) else return(NULL)
  })
  
  # ------------------------------------------------------------------------------
  # STUDENT 360° PROFILE OUTPUT LOGIC
  # ------------------------------------------------------------------------------
  output$ui_risk_badge <- renderUI({
    st <- selected_student()
    if (is.null(st)) return(NULL)
    
    att <- suppressWarnings(as.numeric(gsub("%", "", st[["attendancePercentage"]])))
    pct <- suppressWarnings(as.numeric(st[["percentage"]]))
    status <- tolower(trimws(st[["passFailStatus"]]))
    
    if (is.na(att)) att <- 0
    if (is.na(pct)) pct <- 0
    
    if (status == "fail" || att < 65 || pct < 40) {
      div(class = "status-badge badge-risk", icon("triangle-exclamation"), " HIGH RISK STUDENT")
    } else if (att < 80 || pct < 60) {
      div(class = "status-badge badge-attention", icon("circle-exclamation"), " NEEDS ATTENTION")
    } else {
      div(class = "status-badge badge-safe", icon("shield-halved"), " SAFE / HIGH PERFORMER")
    }
  })
  
  output$ui_profile_att <- renderUI({
    st <- selected_student()
    val <- if(!is.null(st) && "attendancePercentage" %in% colnames(st)) st[["attendancePercentage"]] else "0%"
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", val), div(class = "premium-card-label", "Attendance Rate")),
        div(class = "premium-card-icon bg-icon-blue", icon("clock"))
    )
  })
  
  output$ui_profile_score <- renderUI({
    st <- selected_student()
    val <- if(!is.null(st) && "percentage" %in% colnames(st)) paste0(st[["percentage"]], "%") else "0%"
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", val), div(class = "premium-card-label", "Overall Marks")),
        div(class = "premium-card-icon bg-icon-green", icon("chart-simple"))
    )
  })
  
  output$ui_profile_grade <- renderUI({
    st <- selected_student()
    val <- if(!is.null(st) && "grade" %in% colnames(st)) st[["grade"]] else "N/A"
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", val), div(class = "premium-card-label", "Final Grade")),
        div(class = "premium-card-icon bg-icon-purple", icon("award"))
    )
  })
  
  output$ui_profile_class <- renderUI({
    st <- selected_student()
    val <- if(!is.null(st) && "class" %in% colnames(st)) st[["class"]] else "N/A"
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", val), div(class = "premium-card-label", "Assigned Class")),
        div(class = "premium-card-icon bg-icon-yellow", icon("school"))
    )
  })
  
  output$plot_student_radar <- renderPlotly({
    st <- selected_student()
    req(!is.null(st))
    
    subj_cols <- intersect(c("maths", "science", "english", "history", "geography"), tolower(colnames(st)))
    
    if (length(subj_cols) > 0) {
      scores <- sapply(subj_cols, function(col) suppressWarnings(as.numeric(st[[col]])))
      scores[is.na(scores)] <- 0
      subjects <- toupper(subj_cols)
      
      subjects <- c(subjects, subjects[1])
      scores <- c(scores, scores[1])
      
      plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        r = scores,
        theta = subjects,
        fill = 'toself',
        fillcolor = 'rgba(14, 165, 233, 0.25)',
        line = list(color = '#0ea5e9', width = 2)
      ) %>%
        layout(
          polar = list(radialaxis = list(visible = TRUE, range = c(0, 100))),
          showlegend = FALSE,
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
    } else {
      plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = "Subject columns not found in dataset")
    }
  })
  
  output$table_student_info <- renderTable({
    st <- selected_student()
    req(!is.null(st))
    
    data.frame(
      Property = c("Student ID", "First Name", "Gender", "Status"),
      Value = c(
        ifelse("studentId" %in% colnames(st), st[["studentId"]], "-"),
        ifelse("firstName" %in% colnames(st), st[["firstName"]], "-"),
        ifelse("gender" %in% colnames(st), st[["gender"]], "-"),
        ifelse("passFailStatus" %in% colnames(st), st[["passFailStatus"]], "-")
      )
    )
  }, colnames = FALSE, striped = TRUE, hover = TRUE, width = "100%")
  
  # ------------------------------------------------------------------------------
  # EXECUTIVE OVERVIEW & METRICS RENDERS
  # ------------------------------------------------------------------------------
  output$ui_card_students <- renderUI({
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", nrow(filtered_data())), div(class = "premium-card-label", "Active Registries")),
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
        div(class = "premium-card-meta", div(class = "premium-card-value", pass_pct), div(class = "premium-card-label", "Pass Rate")),
        div(class = "premium-card-icon bg-icon-green", icon("circle-check"))
    )
  })
  
  output$ui_card_attendance <- renderUI({
    df <- filtered_data()
    avg_att <- "0.0%"
    if (nrow(df) > 0 && "attendancePercentage" %in% colnames(df)) {
      clean_strings <- gsub("%", "", df[["attendancePercentage"]])
      num_attendance <- suppressWarnings(as.numeric(clean_strings))
      mean_att <- mean(num_attendance, na.rm = TRUE)
      avg_att <- paste0(round(ifelse(is.nan(mean_att) | is.na(mean_att), 0, mean_att), 1), "%")
    }
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", avg_att), div(class = "premium-card-label", "Mean Attendance")),
        div(class = "premium-card-icon bg-icon-yellow", icon("clock"))
    )
  })
  
  output$ui_card_class <- renderUI({
    df <- student_data
    top_class_name <- "N/A"
    if (nrow(df) > 0 && "class" %in% colnames(df) && "percentage" %in% colnames(df)) {
      df$numeric_pct <- suppressWarnings(as.numeric(df[["percentage"]]))
      class_means <- aggregate(numeric_pct ~ class, data = df, FUN = mean, na.rm = TRUE)
      top_class_name <- if(nrow(class_means) > 0) class_means$class[which.max(class_means$numeric_pct)] else "N/A"
    }
    div(class = "premium-card",
        div(class = "premium-card-meta", div(class = "premium-card-value", top_class_name), div(class = "premium-card-label", "Top Stream")),
        div(class = "premium-card-icon bg-icon-purple", icon("award"))
    )
  })
  
  # ------------------------------------------------------------------------------
  # ANALYTICS & DASHBOARD PLOTS
  # ------------------------------------------------------------------------------
  output$plot_pass_fail <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "passFailStatus" %in% colnames(df))
    status_counts <- as.data.frame(table(df[["passFailStatus"]]))
    colnames(status_counts) <- c("Status", "Count")
    
    plot_ly(status_counts, labels = ~Status, values = ~Count, type = 'pie', hole = 0.6,
            marker = list(colors = c('#10b981', '#ef4444'))) %>%
      layout(showlegend = TRUE, paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$plot_class_perf <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "class" %in% colnames(df), "percentage" %in% colnames(df))
    df$percentage <- suppressWarnings(as.numeric(df$percentage))
    class_perf <- aggregate(percentage ~ class, data = df, FUN = mean, na.rm = TRUE)
    req(nrow(class_perf) > 0)
    
    plot_ly(class_perf, x = ~class, y = ~percentage, type = 'bar', marker = list(color = '#0ea5e9')) %>%
      layout(yaxis = list(title = "Mean Marks (%)"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$plot_attendance_marks <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "attendancePercentage" %in% colnames(df), "percentage" %in% colnames(df))
    
    plot_df <- data.frame(
      Attendance = suppressWarnings(as.numeric(gsub("%", "", df[["attendancePercentage"]]))),
      Percentage = suppressWarnings(as.numeric(df[["percentage"]])),
      ID = if ("studentId" %in% colnames(df)) df[["studentId"]] else seq_len(nrow(df))
    )
    
    plot_ly(plot_df, x = ~Attendance, y = ~Percentage, type = 'scatter', mode = 'markers',
            marker = list(size = 8, color = '#f59e0b', opacity = 0.7)) %>%
      layout(xaxis = list(title = "Attendance (%)"), yaxis = list(title = "Score (%)"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$correlation_text <- renderUI({
    df <- filtered_data()
    if (nrow(df) > 0 && "attendancePercentage" %in% colnames(df) && "percentage" %in% colnames(df)) {
      att <- suppressWarnings(as.numeric(gsub("%", "", df[["attendancePercentage"]])))
      pct <- suppressWarnings(as.numeric(df[["percentage"]]))
      r_val <- suppressWarnings(cor(att, pct, use = "complete.obs"))
      valid_r <- if(is.na(r_val) || is.nan(r_val)) 0 else round(r_val, 2)
      p(style = "text-align: center; font-weight: 600; color: #0f172a; margin-top: 5px;",
        paste("Pearson Correlation Coefficient (r):", valid_r))
    }
  })
  
  output$plot_gender_comp <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "gender" %in% colnames(df), "percentage" %in% colnames(df))
    df$percentage <- suppressWarnings(as.numeric(df$percentage))
    gender_perf <- aggregate(percentage ~ gender, data = df, FUN = mean, na.rm = TRUE)
    req(nrow(gender_perf) > 0)
    
    plot_ly(gender_perf, x = ~gender, y = ~percentage, type = 'bar', color = ~gender, colors = c('#8b5cf6', '#0ea5e9')) %>%
      layout(yaxis = list(title = "Average Percentage (%)"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$plot_subject_comp <- renderPlotly({
    df <- filtered_data()
    subj_cols <- intersect(c("maths", "science", "english", "history", "geography"), tolower(colnames(df)))
    if (length(subj_cols) > 0) {
      subj_means <- sapply(subj_cols, function(col) mean(suppressWarnings(as.numeric(df[[col]])), na.rm = TRUE))
      subj_df <- data.frame(Subject = toupper(subj_cols), Score = subj_means)
      
      plot_ly(subj_df, x = ~Subject, y = ~Score, type = 'bar', marker = list(color = '#10b981')) %>%
        layout(yaxis = list(title = "Mean Score"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    } else {
      plot_ly(type = 'scatter', mode = 'markers') %>% layout(title = "No Subject Specific Columns Found")
    }
  })
  
  output$plot_trend_analysis <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "percentage" %in% colnames(df))
    df$percentage <- suppressWarnings(as.numeric(df$percentage))
    df <- df[order(df$percentage), ]
    
    plot_ly(df, x = seq_len(nrow(df)), y = ~percentage, type = 'scatter', mode = 'lines+markers', line = list(color = '#0ea5e9')) %>%
      layout(xaxis = list(title = "Sorted Student Index"), yaxis = list(title = "Score Curve (%)"), paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  output$plot_grade_dist <- renderPlotly({
    df <- filtered_data()
    req(nrow(df) > 0, "grade" %in% colnames(df), "class" %in% colnames(df))
    
    grade_counts <- as.data.frame(table(df[["class"]], df[["grade"]]))
    colnames(grade_counts) <- c("Class", "Grade", "Count")
    
    plot_ly(grade_counts, x = ~Class, y = ~Count, type = 'bar', color = ~Grade) %>%
      layout(barmode = 'stack', paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  # ------------------------------------------------------------------------------
  # DATA TABLES
  # ------------------------------------------------------------------------------
  output$top_10_table <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0, "percentage" %in% colnames(df))
    df$num_pct <- suppressWarnings(as.numeric(df$percentage))
    top10 <- df %>% arrange(desc(num_pct)) %>% head(10)
    
    cols <- intersect(c("studentId", "class", "percentage", "grade"), colnames(top10))
    datatable(top10[, cols, drop = FALSE], options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
  
  output$lowest_attendance_table <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0, "attendancePercentage" %in% colnames(df))
    df$num_att <- suppressWarnings(as.numeric(gsub("%", "", df[["attendancePercentage"]])))
    low_att <- df %>% arrange(num_att) %>% head(10)
    
    cols <- intersect(c("studentId", "class", "attendancePercentage", "passFailStatus"), colnames(low_att))
    datatable(low_att[, cols, drop = FALSE], options = list(dom = 't', pageLength = 10), rownames = FALSE)
  })
  
  output$student_table <- renderDT({
    df <- filtered_data()
    req(nrow(df) > 0)
    datatable(df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10, dom = "lrtip"))
  })
}

# ------------------------------------------------------------------------------
# 5. EXECUTION POINT
# ------------------------------------------------------------------------------
shinyApp(ui, server)