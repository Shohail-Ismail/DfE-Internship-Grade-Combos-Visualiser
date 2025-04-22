library(shiny)
library(dplyr)
library(readr)
library(DT)
library(shinyalert)
library(plotly)

data <- read_csv("SubjectComb_Final_RANDOMISED.csv")
grade_levels <- c("A*", "A and above", "B and above", "C and above", "D and above", "E and above", "U and above")

subject_input <- function(id) {
  selectizeInput(id, label = NULL,
                 choices = c("", sort(unique(data$subject_1))),
                 selected = "",
                 options = list(create = FALSE))
}

ui <- function(request) {
  fluidPage(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('toggle-dark', function(message) {
        if (message) {
          document.body.classList.add('dark-mode');
        } else {
          document.body.classList.remove('dark-mode');
        }
      });
    ")),
    
    uiOutput("dynamic_css"),
    
    tags$head(
      tags$style(HTML("
        body.dark-mode {
          background-color: #121212 !important;
          color: #f5f5f5 !important;
        }

        body.dark-mode h1, 
        body.dark-mode h2, 
        body.dark-mode .title {
          color: #f5f5f5 !important;
        }

        body.dark-mode .nav-tabs > li > a {
          background-color: #2a2a2a !important;
          color: #cccccc !important;
        }

        body.dark-mode .nav-tabs > li.active > a {
          background-color: #1e1e1e !important;
          color: #ffffff !important;
          border-color: #444 !important;
        }

        body.dark-mode .form-control, 
        body.dark-mode .selectize-input,
        body.dark-mode .selectize-dropdown,
        body.dark-mode .shiny-input-container,
        body.dark-mode .btn,
        body.dark-mode .shiny-download-link {
          background-color: #1e1e1e !important;
          color: #f5f5f5 !important;
          border-color: #555 !important;
        }

        body.dark-mode .selectize-input::after {
          border-top: 5px solid #f5f5f5 !important;
        }

        body.dark-mode table.dataTable,
        body.dark-mode table.dataTable thead,
        body.dark-mode table.dataTable tbody,
        body.dark-mode table.dataTable td,
        body.dark-mode table.dataTable th {
          background-color: #1e1e1e !important;
          color: #f5f5f5 !important;
        }

        body.dark-mode .dataTables_info,
        body.dark-mode .dataTables_filter label,
        body.dark-mode .dataTables_length label,
        body.dark-mode .paginate_button,
        body.dark-mode .explanation-box,
        body.dark-mode #footer {
          color: #f5f5f5 !important;
        }

        body.dark-mode .fixed-footer {
          background-color: #1e1e1e;
          color: #f5f5f5;
          border-top: 1px solid #444;
        }

        body.dark-mode a {
          color: #81caff !important;
        }

        .swal-button {
          background-color: #005a9c !important;
          color: white !important;
        }

        .sr-only {
          position: absolute;
          left: -10000px;
          top: auto;
          width: 1px;
          height: 1px;
          overflow: hidden;
        }

        .fixed-footer {
          position: fixed;
          bottom: 0;
          width: 100%;
          background-color: #f0f0f0;
          padding: 10px;
          border-top: 1px solid #ccc;
          text-align: center;
          z-index: 9999;
        }
      "))
    ),
    
    titlePanel(div("DfE Grade Combinations Viewer", class = "title")),
    checkboxInput("dark_mode", "Enable dark mode", value = FALSE),
    
    tabsetPanel(
      tabPanel("Grade Distribution",
               sidebarLayout(
                 sidebarPanel(subject_input("subject_dist")),
                 mainPanel(plotlyOutput("subject_grade_plot"))
               )
      ),
      tabPanel("Subject Combinations (All grades)",
               sidebarLayout(
                 sidebarPanel(
                   subject_input("subject_all"),
                   downloadButton("download_all", "Download table as CSV")
                 ),
                 mainPanel(
                   uiOutput("top_pairings_all"),
                   DTOutput("subject_combinations_table_all")
                 )
               )
      ),
      tabPanel("Subject Combinations (Filter by grade)",
               sidebarLayout(
                 sidebarPanel(
                   subject_input("subject_grade"),
                   uiOutput("grade_filter_ui"),
                   downloadButton("download_filtered", "Download table as CSV")
                 ),
                 mainPanel(
                   uiOutput("grade_filter_warning"),
                   DTOutput("subject_combinations_table_filtered")
                 )
               )
      )
    ),
    
    tags$div(
      id = "footer",
      class = "fixed-footer",
      tags$p("This tool was developed using preprocessed data to support internal exploration of A-Level subject combinations and performance. For full national results and methodological notes, visit the official publication:"),
      tags$a(
        href = "https://explore-education-statistics.service.gov.uk/find-statistics/a-level-and-other-16-to-18-results/2023-24",
        "Explore Education Statistics – A level and other 16 to 18 results (2023/24)",
        target = "_blank"
      )
    )
  )
}

server <- function(input, output, session) {
  observe({
    session$sendCustomMessage("toggle-dark", input$dark_mode)
  })
  
  output$dynamic_css <- renderUI({ NULL })
  
  output$grade_filter_ui <- renderUI({
    req(input$subject_grade != "")
    selectInput("grade_filter", "Select grade:",
                choices = c("", grade_levels),
                selected = "")
  })
  
  output$subject_grade_plot <- renderPlotly({
    req(input$subject_dist != "")
    df <- data %>%
      filter(subject_1 == input$subject_dist, !is.na(Grade_1)) %>%
      mutate(
        grade = recode(Grade_1, "*" = "A*"),
        grade = factor(grade, levels = c("A*", "A", "B", "C", "D", "E", "U")),
        display_label = case_when(
          grade == "A*" ~ "A*",
          grade == "A"  ~ "A and above",
          grade == "B"  ~ "B and above",
          grade == "C"  ~ "C and above",
          grade == "D"  ~ "D and above",
          grade == "E"  ~ "E and above",
          grade == "U"  ~ "U and above",
          TRUE ~ as.character(grade)
        )
      ) %>%
      group_by(display_label) %>%
      summarise(count = sum(number_students, na.rm = TRUE), .groups = "drop") %>%
      mutate(display_label = factor(display_label, levels = grade_levels)) %>%
      arrange(display_label)
    
    plot_ly(
      data = df,
      x = ~display_label,
      y = ~count,
      type = 'bar',
      text = ~paste0("Grade: ", display_label, "<br>Students: ", count),
      hoverinfo = 'text',
      marker = list(color = 'steelblue')
    ) %>%
      layout(
        title = paste("Grade distribution for", input$subject_dist),
        xaxis = list(title = "Grade band"),
        yaxis = list(title = "Number of students"),
        plot_bgcolor = "#1e1e1e",
        paper_bgcolor = "#1e1e1e",
        font = list(color = "white")
      )
  })
  
  filtered_all_data <- reactive({
    req(input$subject_all)
    data %>%
      filter(subject_1 == input$subject_all) %>%
      mutate(
        PPE_TakingSubj2 = suppressWarnings(as.numeric(trimws(PPE_TakingSubj2))),
        PPE_NOTTakingSubj2 = as.numeric(trimws(PPE_NOTTakingSubj2))
      ) %>%
      group_by(subject_1, subject_2) %>%
      summarise(
        number_students = sum(number_students, na.rm = TRUE),
        PPE_TakingSubj2 = mean(PPE_TakingSubj2, na.rm = TRUE),
        PPE_NOTTakingSubj2 = mean(PPE_NOTTakingSubj2, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(raw_diff = PPE_TakingSubj2 - PPE_NOTTakingSubj2) %>%
      arrange(desc(number_students))
  })
  
  output$top_pairings_all <- renderUI({
    df <- filtered_all_data() %>% filter(!is.na(raw_diff))
    if (nrow(df) == 0) return(NULL)
    
    top_positive <- df %>% arrange(desc(raw_diff)) %>% slice(1)
    top_negative <- df %>% arrange(raw_diff) %>% slice(1)
    
    tags$div(
      class = "explanation-box",
      style = "padding: 10px; margin-bottom: 10px;",
      tags$p(tags$b("Top pairings for "), input$subject_all, ":"),
      tags$ul(
        tags$li(
          tags$span("Highest performance gap: "),
          tags$b(top_positive$subject_1), " + ", tags$b(top_positive$subject_2),
          " (", sprintf("%+.1f PTS", top_positive$raw_diff), ")"
        ),
        tags$li(
          tags$span("Lowest performance gap: "),
          tags$b(top_negative$subject_1), " + ", tags$b(top_negative$subject_2),
          " (", sprintf("%+.1f PTS", top_negative$raw_diff), ")"
        )
      )
    )
  })
  
  output$subject_combinations_table_all <- renderDT({
    df <- filtered_all_data()
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No combinations found.")))
    
    df <- df %>%
      mutate(`Performance diff (PTS)` = ifelse(is.na(raw_diff), NA, sprintf("%+.1f PTS", raw_diff))) %>%
      select(`Subject 1` = subject_1,
             `Subject 2` = subject_2,
             `Number of students` = number_students,
             `Performance diff (PTS)`,
             raw_diff)
    
    datatable(df,
              selection = "single",
              options = list(
                pageLength = 10,
                columnDefs = list(
                  list(visible = FALSE, targets = 4),
                  list(orderData = 4, targets = 3)
                )
              ),
              rownames = FALSE)
  })
  
  observeEvent(input$subject_combinations_table_all_rows_selected, {
    row_index <- input$subject_combinations_table_all_rows_selected
    df <- filtered_all_data()
    if (length(row_index) == 0 || row_index > nrow(df)) return()
    row <- df[row_index, ]
    
    explanation <- if (is.na(row$PPE_TakingSubj2) || is.na(row$PPE_NOTTakingSubj2)) {
      "Data unavailable for these subjects."
    } else {
      paste0("Average PPE (students who took ", row$subject_2, "): ",
             round(row$PPE_TakingSubj2, 1), "\n",
             "Average PPE (students who didn’t): ",
             round(row$PPE_NOTTakingSubj2, 1))
    }
    
    shinyalert(
      title = paste(row$subject_1, "+", row$subject_2),
      text = paste0("Number of students: ", row$number_students, "\n\n", explanation),
      type = "info"
    )
  })
  
  filtered_grade_data <- reactive({
    req(input$subject_grade)
    df <- data %>% filter(subject_1 == input$subject_grade)
    
    if (!is.null(input$grade_filter) && input$grade_filter != "") {
      grade_lookup <- case_when(
        input$grade_filter == "A*" ~ "*",
        input$grade_filter == "A and above" ~ "A",
        input$grade_filter == "B and above" ~ "B",
        input$grade_filter == "C and above" ~ "C",
        input$grade_filter == "D and above" ~ "D",
        input$grade_filter == "E and above" ~ "E",
        input$grade_filter == "U and above" ~ "U",
        TRUE ~ NA_character_
      )
      df <- df %>% filter(Grade_1 == grade_lookup)
    }
    
    df %>%
      group_by(subject_1, subject_2) %>%
      summarise(number_students = sum(number_students, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(number_students))
  })
  
  output$grade_filter_warning <- renderUI({
    req(input$grade_filter != "")
    tags$div(
      class = "explanation-box",
      style = "padding: 10px; margin-bottom: 10px;",
      tags$p(
        tags$b("Note: "),
        "Performance differences are hidden in this view because comparisons are only valid across all grade levels."
      )
    )
  })
  
  output$subject_combinations_table_filtered <- renderDT({
    df <- filtered_grade_data()
    if (nrow(df) == 0) return(datatable(data.frame(Message = "No combinations found.")))
    
    df <- df %>%
      select(`Subject 1` = subject_1,
             `Subject 2` = subject_2,
             `Number of students` = number_students)
    
    datatable(df,
              selection = "none",
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("subject_combinations_", input$subject_all, ".csv")
    },
    content = function(file) {
      write.csv(filtered_all_data(), file, row.names = FALSE)
    }
  )
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("subject_combinations_filtered_", input$subject_grade, ".csv")
    },
    content = function(file) {
      write.csv(filtered_grade_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)