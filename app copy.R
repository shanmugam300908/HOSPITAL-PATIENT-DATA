library(shiny)
library(ggplot2)
library(dplyr)

# -----------------------------
# 1. HOSPITAL PATIENT DATA
# -----------------------------

patients <- data.frame(
  Patient_ID = c("P001", "P002", "P003", "P004", "P005"),
  Name = c("Arun", "Priya", "Rahul", "Meena", "Kavin"),
  Age = c(25, 42, 35, 58, 30),
  Gender = c("Male", "Female", "Male", "Female", "Male"),
  Disease = c(
    "Fever",
    "Diabetes",
    "Hypertension",
    "Asthma",
    "Fever"
  ),
  Blood_Pressure = c(120, 140, 150, 135, 118),
  Heart_Rate = c(78, 82, 88, 90, 75),
  Treatment_Days = c(3, 7, 10, 8, 4)
)

# -----------------------------
# 2. PATIENT RISK LEVEL
# -----------------------------

patients$Risk_Level <- ifelse(
  patients$Blood_Pressure >= 140,
  "High",
  ifelse(
    patients$Blood_Pressure >= 120,
    "Medium",
    "Low"
  )
)

# -----------------------------
# 3. USER INTERFACE
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Hospital Patient Analyzer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "patient",
        "Select Patient:",
        choices = patients$Name
      )
      
    ),
    
    mainPanel(
      
      h3("Patient Information"),
      
      textOutput("patient_id"),
      textOutput("patient_name"),
      textOutput("age"),
      textOutput("gender"),
      textOutput("disease"),
      textOutput("blood_pressure"),
      textOutput("heart_rate"),
      textOutput("treatment_days"),
      textOutput("risk_level"),
      
      br(),
      
      plotOutput("health_plot"),
      
      br(),
      
      plotOutput("disease_plot")
    )
  )
)

# -----------------------------
# 4. SERVER
# -----------------------------

server <- function(input, output) {
  
  selected_patient <- reactive({
    
    patients %>%
      filter(Name == input$patient)
    
  })
  
  # Patient ID
  output$patient_id <- renderText({
    
    paste(
      "Patient ID:",
      selected_patient()$Patient_ID
    )
    
  })
  
  # Patient Name
  output$patient_name <- renderText({
    
    paste(
      "Patient:",
      selected_patient()$Name
    )
    
  })
  
  # Age
  output$age <- renderText({
    
    paste(
      "Age:",
      selected_patient()$Age
    )
    
  })
  
  # Gender
  output$gender <- renderText({
    
    paste(
      "Gender:",
      selected_patient()$Gender
    )
    
  })
  
  # Disease
  output$disease <- renderText({
    
    paste(
      "Disease:",
      selected_patient()$Disease
    )
    
  })
  
  # Blood Pressure
  output$blood_pressure <- renderText({
    
    paste(
      "Blood Pressure:",
      selected_patient()$Blood_Pressure,
      "mmHg"
    )
    
  })
  
  # Heart Rate
  output$heart_rate <- renderText({
    
    paste(
      "Heart Rate:",
      selected_patient()$Heart_Rate,
      "bpm"
    )
    
  })
  
  # Treatment Days
  output$treatment_days <- renderText({
    
    paste(
      "Treatment Duration:",
      selected_patient()$Treatment_Days,
      "days"
    )
    
  })
  
  # Risk Level
  output$risk_level <- renderText({
    
    paste(
      "Risk Level:",
      selected_patient()$Risk_Level
    )
    
  })
  
  # Blood Pressure and Heart Rate Graph
  output$health_plot <- renderPlot({
    
    patient <- selected_patient()
    
    health_data <- data.frame(
      Measurement = c(
        "Blood Pressure",
        "Heart Rate"
      ),
      Value = c(
        patient$Blood_Pressure,
        patient$Heart_Rate
      )
    )
    
    ggplot(
      health_data,
      aes(
        x = Measurement,
        y = Value
      )
    ) +
      geom_col() +
      labs(
        title = "Patient Health Measurements",
        x = "Measurement",
        y = "Value"
      )
    
  })
  
  # Disease Distribution Graph
  output$disease_plot <- renderPlot({
    
    disease_count <- patients %>%
      count(Disease)
    
    ggplot(
      disease_count,
      aes(
        x = Disease,
        y = n
      )
    ) +
      geom_col() +
      labs(
        title = "Disease Distribution",
        x = "Disease",
        y = "Number of Patients"
      )
    
  })
  
}

# -----------------------------
# 5. RUN SHINY APPLICATION
# -----------------------------

shinyApp(
  ui = ui,
  server = server
)