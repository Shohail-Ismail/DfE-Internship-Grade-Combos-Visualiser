library(shiny)
library(dplyr)
library(readr)

# #fast
# dummy_data <- data.frame(
#   subject_1 = rep(c("Maths", "Biology", "History"), each = 3),
#   subject_2 = rep(c("Physics", "Chemistry", "Economics"), 3),
#   Grade_1 = rep(c("A", "B", "C"), 3),
#   number_students = sample(50:150, 9),
#   PPE_TakingSubj2 = runif(9, 10, 60),
#   PPE_NOTTakingSubj2 = runif(9, 10, 60)
# )

data <- read_csv("C:/Users/shoha/Downloads/SubjectComb_Final_RANDOMISED.csv") %>%
  filter(!is.na(Grade_1), Grade_1 != "z") %>%
  rename(
    grade = Grade_1
  )

grade_levels <- c("A*", "A or above", "B or above", "C or above", "D or above", "E or above", "U or above")

ui <- fluidPage(
  titlePanel("DfE SPC internship proj"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("subject1", "Select subject 1: ", 
                  choices = c("", unique(data$subject_1)),
                  selected = ""),
      uiOutput("grade_filter_ui")
    ),
    
    mainPanel(
      h4("Top Subject Combinations"),
      div(
        id = "subject-combo-list",
        style = "max-height: 400px; overflow-y: scroll;",
        uiOutput("subject_combinations_ui")
      ),
      br(),
      plotOutput("grade_distribution_plot"),
      textOutput("ppe_comparison_text")
    )
  )
)

server <- function(input, output, session) {
  
  output$grade_filter_ui <- renderUI({
    req(input$subject1 != "")
    selectInput("grade1", "Select grade in subject:",
                choices = c("", grade_levels),
                selected = "")
  })
  
  filtered_data <- reactive({
    req(input$subject1)
    df <- data %>% filter(subject_1 == input$subject1)
    if (input$grade1 != "") {
      df <- df %>% filter(grade == input$grade1)
    }
    df
  })
  
  output$subject_combinations_ui <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(tags$p("Nobody has that grade in this subject! :O"))
    }
    tagList(
      lapply(1:nrow(df), function(i) {
        combo <- df[i, ]
        label_text <- paste0(combo$subject_1, " + ", combo$subject_2, 
                             " (", combo$number_students, " students)")
        actionButton(inputId = paste0("combo_", i),
                     label = label_text,
                     class = "combo-btn",
                     style = "margin-bottom: 5px; width: 100%; text-align: left;")
      })
    )
  })
  
  output$grade_distribution_plot <- renderPlot({})
  output$ppe_comparison_text <- renderText({ "" })
}

shinyApp(ui = ui, server = server)