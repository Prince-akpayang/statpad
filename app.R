# libraries ---------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(shinyBS)




# Load the fellows_data ---------------------------------------------------

# Directory containing all the data files
data_directory <- "data/"


fellows_data_file <- paste0(data_directory, "Fellows and Classroom Information (Responses).csv")
print(paste("Loading fellows data from:", fellows_data_file))
fellows_data <- read.csv(fellows_data_file)


# Define UI ---------------------------------------------------------------


ui <- dashboardPage(
  dashboardHeader(title = "Learner Performance Analytics"),

# side bar  ---------------------------------------------------------------

  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Select Fellow", tabName = "pick", icon = icon("image")),
      menuItem("Summary Tables", tabName = "tables", icon = icon("table")),
      menuItem("Plots", tabName = "plots", icon = icon("chart-bar"))
    )
  ),


# main panel --------------------------------------------------------------



  dashboardBody(
    tabItems(
      
# data upload -------------------------------------------------------------
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload Dataset", status = "primary", solidHeader = TRUE, footer = "having issues write princeakpayang@gmail.com", 
                    fileInput("file", "Choose CSV File", 
                              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                    helpText("Please upload a CSV file in the format: S/N, name, Gender, Test, Exam Score, total, arm, term"),
                    tableOutput("fellow_info")
                )
              )
      ),
      

# fellow info -------------------------------------------------------------

      tabItem(tabName = "pick",
              fluidRow(
                box(title = "Select Fellow", status = "primary", solidHeader = TRUE, footer = "having issues write princeakpayang@gmail.com",width = 12, 
                    selectInput("fellow", "Select Fellow:", choices = fellows_data$Full_name),
                    tableOutput("fellow_info")
                )
              )
      ),


# table info --------------------------------------------------------------
      
      tabItem(tabName = "tables", uiOutput("term_selector1"),
        fluidRow(
          box(title = "Summary Tables", status = "info", solidHeader = TRUE, width = 12, footer = "having issues write princeakpayang@gmail.com",
              selectInput("table_choice", "Choose Summary Table:", 
                          choices = c("Average Score per Term and Arm", 
                                      "Performance summary across terms",
                                      "Test vs. Exam Performance by Term and Arm",
                                      "Gender Performance Analysis by Term and Arm",
                                      "Students data (test and exams)",
                                      "Top Performers by Term and Arm",
                                      "Pass/Fail Rate by Term and Arm",
                                      "Consistency of Performance by Term and Arm",
                                      "Score Distribution by Term and Arm",
                                      "HIgh performers")),
              DTOutput("summary_table")
          )
        )
      ),

# plot info ---------------------------------------------------------------
      
      tabItem(tabName = "plots", uiOutput("term_selector2"),
        fluidRow(
          box(title = "Plots", status = "success", solidHeader = TRUE, width = 12, footer = "having issues write princeakpayang@gmail.com",
              selectInput("plot_choice", "Choose Plot:", 
                          choices = c("Average Score per Term and Arm", 
                                      "Test vs. Exam Performance by Term and Arm",
                                      "class average",
                                      "Gender Performance Analysis by Term and Arm",
                                      # "Improvement or Decline in Scores by Term and Arm",
                                      # "Top Performers by Term and Arm",
                                      "Pass/Fail Rate by Term and Arm"
                                      # "Consistency of Performance by Term and Arm",
                                      # "Score Distribution by Term and Arm"
                                      )),
              plotOutput("summary_plot"),
              bsTooltip(id = "Pass/Fail Rate by Term and Arm",
                        title = "Correlation measures the strength and direction of a linear relationship between two variables. A correlation close to 1 or -1 indicates a strong relationship, while a correlation close to 0 indicates a weak relationship. Remember, correlation does not imply causation.",
                        placement = "top", trigger = "hover")
              
          )
        )
      )
    )
  )
)

# Define server logic -----------------------------------------------------

# reactive data reading ---------------------------------------------------
 
server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })


# main data filter --------------------------------------------------------

  # Reactive expression to filter the main dataset based on selected fellow
  filteredData <- reactive({
    req(input$fellow)
    fellows_data %>% filter(Full_name == input$fellow)
  })


# fellow info output ------------------------------------------------------
  
  output$fellow_info <- renderPrint({
    req(data())
    df <- data()
  })
    
  output$fellow_info <- renderTable({
    # Reactive expression to filter the main dataset based on selected fellow
    filteredData <- reactive({
      req(input$fellow)
      fellows_data %>% filter(Full_name == input$fellow)
    })
    
  })
  
  # Display the selected fellow's name
  output$selected_fellow <- renderText({
    paste("You have selected:", input$fellow)
  })
  
  # Display detailed information about the selected fellow
  output$fellow_info <- renderTable({
    filteredData()
  })


# term selector -----------------------------------------------------------
      
  # Dynamically populate the term selector based on the selected dataset
  output$term_selector1 <- renderUI({
    req(data())
    df <- data()
    terms_available <- unique(df$term)
    selectInput("term", "Select Term:", choices = terms_available, selected = terms_available[1])
  })
  
  # Dynamically populate the term selector based on the selected dataset
  output$term_selector2 <- renderUI({
    req(data())
    df <- data()
    terms_available <- unique(df$term)
    selectInput("term", "Select Term:", choices = terms_available, selected = terms_available[1])
  })

# table output ------------------------------------------------------------

  output$summary_table <- renderDT({
    req(data())
    df <- data()
    req(input$term)
    
    summary_table <- switch(input$table_choice,


# average score -----------------------------------------------------------
                            "Average Score per Term and Arm" = df %>%
                              filter(term==input$term) %>% 
                              group_by(arm) %>%
                              summarise(Average_Score = round(mean(total, na.rm = TRUE),0)),


# performance summary -----------------------------------------------------
                            "Performance summary across terms" <- df %>%
                              filter(term==input$term) %>% 
                              mutate(
                                  range = case_when(
                                    total >= 70 ~ "70 and above",
                                    total >= 50 & total < 70 ~ "50 to 69",
                                    total >= 40 & total < 50 ~ "40 to 49",
                                    TRUE ~ "0 to 39"
                                  )
                                ) %>%
                                group_by(arm, range) %>%
                                summarise("number of learners" = n()),

# Test vs. exams ----------------------------------------------------------
                            "Test vs. Exam Performance by Term and Arm" = df %>%
                              filter(term==input$term) %>% 
                              group_by( arm) %>%
                              summarise(Test_Avg = round(mean(Test, na.rm = TRUE),0), 
                                        Exam_Avg = round(mean(Exam.Score, na.rm = TRUE),0)),

# Gender performance anals ------------------------------------------------
                            "Gender Performance Analysis by Term and Arm" = df %>%
                              filter(term==input$term) %>% 
                              group_by(Gender, arm) %>%
                              summarise(Average_Score = round(mean(total, na.rm = TRUE),0)),

# students data -----------------------------------------------------------
                            "Students data (test and exams)" = df %>%
                              filter(term==input$term) %>% 
                              group_by(name) %>%
                              summarise(Total = round(sum(Exam.Score,Test),0)),

# Top performer -----------------------------------------------------------
                            "Top Performers by Term and Arm" = df %>%
                              filter(term==input$term) %>% 
                              group_by( arm) %>%
                              arrange(desc(total)) %>%
                              slice_head(n = 10),

# pass/fail ---------------------------------------------------------------
                            "Pass/Fail Rate by Term and Arm" = df %>%
                              filter(term==input$term) %>% 
                              mutate(Pass = ifelse(total >= 50, "Pass", "Fail")) %>%
                              group_by(Pass, term, arm) %>%
                              summarise(Count = n()),

# consistency table -------------------------------------------------------
                            "Consistency of Performance by Term and Arm" = df %>%
                              group_by(name, term, arm) %>%
                              summarise(Consistency = sd(total, na.rm = TRUE)),

# score distribution ------------------------------------------------------
                            "Score Distribution by Term and Arm" = df %>%
                              filter(term == input$term, total>0) %>%
                              mutate(Score_Range = cut(total, breaks = c(0, 50, 60, 70, 100))) %>%
                              group_by(Score_Range, term, arm) %>%
                              summarise(Count = n()),

# high performers ---------------------------------------------------------
                            "HIgh performers" = df %>%
                              filter(term == input$term, total >= 70) %>%
                              select(name, arm, total) %>%
                              arrange(desc(total)) %>%
                              group_by(arm)
    )
    
    datatable(summary_table, options = list(pageLength = 25))
  })

  

# plot output -------------------------------------------------------------

    
  output$summary_plot <- renderPlot({
    req(data())
    df <- data()
    req(input$term)
    
    summary_plot <- switch(input$plot_choice,

# average score -----------------------------------------------------------
                           "Average Score per Term and Arm" = ggplot(df, aes(x = term, y = total, fill = arm)) +
                             geom_boxplot() + 
                             labs(title = "Average Score per Term and Arm", x = "Term", y = "Total Score") +
                             facet_wrap(~arm),

# class average -----------------------------------------------------------
                           "class average" = df %>%
                             filter(Test != 0 & Exam.Score != 0 & total != 0) %>% 
                           group_by(term, arm) %>%
                             summarise(class_avg = mean(total, na.rm = TRUE)) %>% 
                           ggplot( aes(x = term, y = class_avg, group = arm, color = arm)) +
                             geom_line() +
                             geom_point() +
                             labs(title = "Class Average Scores by Term and Arm"),

# test vs exams -----------------------------------------------------------
                           "Test vs. Exam Performance by Term and Arm" = df %>%
                             group_by(arm) %>%
                             filter(term==input$term) %>% 
                             summarise(Test_Avg = mean(Test, na.rm = TRUE), 
                                       Exam_Avg = mean(Exam.Score, na.rm = TRUE)) %>%
                             gather(key = "Type", value = "Average", Test_Avg, Exam_Avg) %>%
                             ggplot(aes(x = Type, y = Average, fill = arm)) +
                             geom_bar(stat = "identity", position = "dodge") +
                             labs(title = "Test vs. Exam Performance by Term and Arm", x = "Type", y = "Average Score"),

# gender based ------------------------------------------------------------
                           "Gender Performance Analysis by Term and Arm" = ggplot(df, aes(x = Gender, y = total, fill = arm)) +
                             geom_boxplot() + 
                             labs(title = "Gender Performance Analysis by Term and Arm", x = "Gender", y = "Total Score") +
                             facet_wrap(~term),
                           
                           # "Improvement or Decline in Scores by Term and Arm" = df %>%
                           #   group_by(name, term, arm) %>%
                           #   summarise(Score_Change = diff(range(total))) %>%
                           #   ggplot(aes(x = name, y = Score_Change, fill = arm)) +
                           #   geom_bar(stat = "identity", position = "dodge") +
                           #   labs(title = "Improvement or Decline in Scores by Term and Arm", x = "Name", y = "Score Change") +
                           #   facet_wrap(~term),
                            
                           # "Top Performers by Term and Arm" = df %>%
                           #   group_by(term, arm) %>%
                           #   arrange(desc(total)) %>%
                           #   slice_head(n = 10) %>%
                           #   ggplot(aes(x = reorder(name, total), y = total, fill = arm)) +
                           #   geom_bar(stat = "identity") +
                           #   labs(title = "Top Performers by Term and Arm", x = "Name", y = "Total Score") +
                           #   coord_flip() +
                           #   facet_wrap(~term),

# pass and fail -----------------------------------------------------------
                           "Pass/Fail Rate by Term and Arm" = df %>%
                             filter(term==input$term,total>0) %>% 
                             mutate(Pass = ifelse(total >= 40, "Pass", "Fail")) %>%
                             group_by(Pass, term, arm) %>%
                             ggplot(aes(x = Pass, fill = arm)) +
                             geom_bar(position = "dodge") +
                             labs(title = "Pass/Fail Rate by Term and Arm", x = "Result", y = "Count"),

# consistency plot --------------------------------------------------------
                           "Consistency of Performance by Term and Arm" = df %>%
                             group_by(name, term, arm) %>%
                             summarise(Consistency = sd(total, na.rm = TRUE)) %>%
                             ggplot(aes(x = name, y = Consistency, fill = arm)) +
                             geom_bar(stat = "identity", position = "dodge") +
                             labs(title = "Consistency of Performance by Term and Arm", x = "Name", y = "Score Standard Deviation") +
                             coord_flip() +
                             facet_wrap(~term),

# score distribution ------------------------------------------------------
                           "Score Distribution by Term and Arm" = df %>%
                             mutate(Score_Range = cut(total, breaks = c(0, 50, 60, 70, 100))) %>%
                             group_by(Score_Range, term, arm) %>%
                             ggplot(aes(x = Score_Range, fill = arm)) +
                             geom_bar(position = "dodge") +
                             labs(title = "Score Distribution by Term and Arm", x = "Score Range", y = "Count")
    )
    
    print(summary_plot)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

