library(shiny)
library(dplyr)
library(readr)

data <- read_csv("C:/Users/shoha/Downloads/SubjectComb_Final_RANDOMISED.csv") %>%
  filter(!is.na(Grade_1), Grade_1 != "z") %>%
  rename(grade = Grade_1)

grade_levels <- c("All", "A*", "A", "B", "C", "D", "E", "U",
                  "A* or above", "A or above", "B or above", "C or above", "D or above", "E or above", "U or above")

ui <- fluidPage(
  titlePanel("DfE SPC internship proj"),
  
  tabsetPanel(
    tabPanel("Grade Distribution",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject_dist", "Select subject:", 
                             choices = c("", sort(unique(data$subject_1))),
                             selected = "")
               ),
               mainPanel(
                 plotOutput("subject_grade_plot")
               )
             )
    ),
    
    tabPanel("Subject Combinations",
             sidebarLayout(
               sidebarPanel(
                 selectInput("subject1", "Select subject:", 
                             choices = c("", sort(unique(data$subject_1))),
                             selected = ""),
                 uiOutput("grade_filter_ui")
               ),
               mainPanel(
                 uiOutput("loading_msg"),
                 uiOutput("subject_combinations_ui")
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  output$grade_filter_ui <- renderUI({
    req(input$subject1 != "")
    selectInput("grade1", "Select grade:",
                choices = c("", grade_levels),
                selected = "")
  })
  
  output$loading_msg <- renderUI({
    req(input$subject1 != "")
    tags$p("Loading...")
  })
  
  output$subject_combinations_ui <- renderUI({
    tags$p("Combinations will appear here.")
  })
  
  output$subject_grade_plot <- renderPlot({})
}

shinyApp(ui = ui, server = server)
