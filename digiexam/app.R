# Install necessary packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("readr")) install.packages("readr")

# Load the required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Load the data using relative path
data <- read_csv("diabetics.csv")

# Clean the data
cleaned_data <- data %>%
  mutate(
    gender = as.factor(tolower(gender)),  # Convert gender to lowercase and factor
    age = as.numeric(gsub("[^0-9.]", "", age)),  # Clean age column by extracting numeric values
    hypertension = as.factor(ifelse(hypertension == "NO", 0, 1)),  # Recode hypertension to numeric
    heart_disease = as.factor(heart_disease),
    smoking_history = as.factor(smoking_history),
    bmi = as.numeric(bmi),
    HbA1c_level = as.numeric(HbA1c_level),
    blood_glucose_level = as.numeric(blood_glucose_level),
    diabetes = as.factor(ifelse(diabetes == "no", 0, 1))  # Recode diabetes to numeric
  ) %>%
  filter(complete.cases(.))  # Remove rows with missing values

# Define UI for application
ui <- fluidPage(
  
  # App title
  titlePanel("Diabetes Data Visualization"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Input: Select variables for x and y axes
      selectInput(inputId = "x_var",
                  label = "Select X-axis Variable:",
                  choices = colnames(cleaned_data),
                  selected = "bmi"),
      
      selectInput(inputId = "y_var",
                  label = "Select Y-axis Variable:",
                  choices = colnames(cleaned_data),
                  selected = "HbA1c_level")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      
      # Output: Plot
      plotOutput(outputId = "diabetes_plot")
      
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to filter data based on selected variables
  filtered_data <- reactive({
    cleaned_data %>% 
      select(!!input$x_var, !!input$y_var)
  })
  
  # Render plot based on selected variables
  output$diabetes_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      labs(title = paste("Scatter Plot of", input$y_var, "against", input$x_var),
           x = input$x_var,
           y = input$y_var)
  })
}

# Combine UI and server into a single object
shinyApp(ui = ui, server = server)

