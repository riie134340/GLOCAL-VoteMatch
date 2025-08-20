
library(shiny)
library(shinythemes)
library(xgboost)
library(ggplot2)
library(shinydashboard)

choices_agree <- c(
  "Strongly disagree", 
  "Somewhat disagree", 
  "Neither agree nor disagree", 
  "Somewhat agree", 
  "Strongly agree"
)

choices_yes_no <- c(
  "Yes", "No"
)

choices_province <- c(
  "Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
  "Northwest Territories","Nova Scotia","Nunavut","Ontario","Prince Edward Island",
  "Quebec","Saskatchewan","Yukon"
)

age_input <- sliderInput(
  inputId = "q1",
  label = "Your age (in years):",
  min = 18,
  max = 100,
  value = 30
)

question_texts <- c(
  "Were you born in Canada?",
  "In which province or territory are you currently living?",
  "Are you presently married, living with a partner, divorced, separated, widowed, or have you never been married?",
  "The government does not care much about what people like me think.",
  "How satisfied are you with the performance of the federal government under Justin Trudeau?",
  "We have gone too far in pushing equal rights in this country.",
  "How much do you think should be done for racial minorities?",
  "Should abortion be banned?",
  "This country would have many fewer problems if there was more emphasis on traditional family values.",
  "We have gone too far in pushing bilingualism in Canada.",
  "Newer lifestyles are contributing to the breakdown of our society.",
  "Too many recent immigrants just don't want to fit in to Canadian society.",
  "Immigrants take jobs away from other Canadians.",
  "Is income inequality a big problem in Canada?",
  "The government should leave it entirely to the private sector to create jobs.",
  "When there is a conflict between protecting the environment and creating jobs, jobs should come first."
)

choices_list <- list(
  choices_yes_no,
  choices_province,
  c("Married", "Living with a partner", "Divorced", "Separated", "Widowed", "Never Married"),
  choices_agree,
  c("Very satisfied", "Fairly satisfied", "Not very satisfied", "Not at all satisfied"),
  choices_agree,
  c("Much more", "Somewhat more", "About the same as now", "Somewhat less", "Much less"),
  c("Yes", "In some circumstances", "No"),
  choices_agree,
  choices_agree,
  choices_agree,
  choices_agree,
  choices_agree,
  c("Definitely yes", "Probably yes", "Not sure", "Probably not", "Definitely not"),
  choices_agree,
  choices_agree
)

input_list <- lapply(seq_along(question_texts), function(i) {
  selectInput(
    inputId = paste0("q", i + 1),  # q2 ~ q17
    label = question_texts[i],
    choices = choices_list[[i]]
  )
})

all_inputs <- c(list(age_input), input_list)

ui <- dashboardPage(
  dashboardHeader(title = "VoterMatch"),
  
  dashboardSidebar(
    sidebarMenu( id = "sidebarMenuID",
                 menuItem("Form", tabName = "form", icon = icon("edit")),
                 menuItem("Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "form",
              fluidRow(
                box(
                  title = "Project Description", width = 12, status = "info", solidHeader = TRUE,
                  p("This project predicts the likelihood of a voter supporting each Canadian political party based on their responses to 16 questions. Select your answers below and click Predict to see probabilities.")
                ),
                box(
                  title = "Input", width = 12, status = "primary", solidHeader = TRUE,
                  do.call(tagList, all_inputs),
                  actionButton("goButton", "Predict")
                )
              )
      ),
      
      tabItem(tabName = "results",
              h3("Your Personalized Voting Prediction"),
              p("Based on your responses, the model has estimated how likely you are to support each major Canadian political party."),
              p("These probabilities reflect how voters with similar characteristics to yours have voted in past federal elections."),
              fluidRow(
                valueBoxOutput("predictedParty", width = 6),
                valueBoxOutput("probability", width = 6)
              ),
              fluidRow(
                box(title = "Prediction Table", width = 6, tableOutput("pred")),
                box(title = "Prediction Plot", width = 6, plotOutput("predPlot", height = "300px"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  # load model
  artifacts <- readRDS("modeling.rds")
  model <- artifacts$model
  features <- artifacts$features
  class_levels <- artifacts$class_levels
  n_class <- artifacts$n_class
  
  observeEvent(input$goButton, {
    updateTabItems(session, "sidebarMenuID", "results")
  })
  
  get_prediction <- eventReactive(input$goButton, {
    print("Predict button clicked!")  # ← Debug 用
    
    input_values <- list(
      cps21_age             = input$q1,
      cps21_bornin_canada   = input$q2,
      pes21_province        = input$q3,
      cps21_marital         = input$q4,
      pes21_govtcare        = input$q5,
      cps21_fed_gov_sat     = input$q6,
      pes21_equalrights     = input$q7,
      pes21_donerm          = input$q8,
      pes21_abort2          = input$q9,
      pes21_famvalues       = input$q10,
      pes21_bilingualism    = input$q11,
      pes21_newerlife       = input$q12,
      pes21_fitin           = input$q13,
      pes21_immigjobs       = input$q14,
      pes21_inequal         = input$q15,
      pes21_privjobs        = input$q16,
      pes21_envirojob       = input$q17
    )
    
    # convert to data.frame
    df <- as.data.frame(input_values)[features]
    levels_list <- artifacts$levels_list
    
    for (col in names(levels_list)) {
      if (col %in% names(df) && col != "cps21_age") {
        df[[col]] <- factor(df[[col]], levels = levels_list[[col]])
      }
    }
    
    # convert to num
    for (col in names(df)) {
      if (col != "cps21_age" && is.factor(df[[col]])) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
    
    # prediction
    pred <- predict(model, as.matrix(df))
    pred_mat <- matrix(pred, ncol = n_class, byrow = TRUE)
    
    pred_df <- data.frame(
      Party = class_levels,
      Probability = round(pred_mat[1, ], 4)
    )
    pred_df <- pred_df[order(-pred_df$Probability), ]
    
    return(pred_df)
  })
  
  # output result
  output$pred <- renderTable({
    get_prediction()
  })
  
  output$predPlot <- renderPlot({
    req(get_prediction())
    pred_df <- get_prediction()
    pred_df$Party <- factor(pred_df$Party, levels = pred_df$Party[order(pred_df$Probability)])
    
    ggplot(pred_df, aes(x = Party, y = Probability)) +
      geom_col(fill = "#4DAF4A", width = 0.6) +
      geom_text(aes(label = scales::percent(Probability, accuracy = 0.1)), 
                vjust = -0.5, size = 4) +
      ylim(0, 1) +
      labs(title = "Predicted Support Probability by Party",
           y = "Probability",
           x = "Party") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
  })
}

shinyApp(ui, server)