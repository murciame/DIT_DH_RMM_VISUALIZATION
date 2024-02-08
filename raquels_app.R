#Because of way I built this code (with all the various containers and data modifications), the app takes a while to load
#Please be patient, thank you - Raquel


# Load required libraries
pacman::p_load(
  rio,          # for importing data
  here,         # for file paths
  janitor,      # for data cleaning
  tidyverse,    # for data management
  shiny,
  shinythemes, 
  dplyr,        # for modifying column names
  ggplot2,      # for plotting and making graphs
  plotly        # adding pie chart and other plot functions 
)

# Create an absolute file path using here
absolute_path <- here("health_dataset.xlsx")

# Storing the names of the excel file's sheets in their own container 
sheet_14_1 <- "Question14_1"
sheet_14_2 <- "Question14_2"
sheet_14_4 <- "Question14_4"
sheet_14_5 <- "Question14_5"
sheet_14_6 <- "Question14_6"
sheet_14_7 <- "Question14_7"
sheet_14_8 <- "Question14_8"
sheet_14_10 <- "Question14_10"
sheet_14_11 <- "Question14_11"
sheet_14_12 <- "Question14_12"
sheet_14_13 <- "Question14_13"
sheet_14_14 <- "Question14_14"
sheet_14_16 <- "Question14_16"
sheet_41_1 <- "Question41_1"
sheet_41_2 <- "Question41_2"
sheet_41_3 <- "Question41_3"
sheet_41_4 <- "Question41_4"
sheet_41_5 <- "Question41_5"
sheet_41_6 <- "Question41_6"
sheet_41_7 <- "Question41_7"

# Load data based on excel file and sheet names
# Each data set corresponds to a specific question and responses
data_14_1 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_1)
data_14_2 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_2)
data_14_4 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_4)
data_14_5 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_5)
data_14_6 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_6)
data_14_7 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_7)
data_14_8 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_8)
data_14_10 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_10)
data_14_11 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_11)
data_14_12 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_12)
data_14_13 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_13)
data_14_14 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_14)
data_14_16 <- readxl::read_xlsx(absolute_path, sheet = sheet_14_16)
data_41_1 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_1)
data_41_2 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_2)
data_41_3 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_3)
data_41_4 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_4)
data_41_5 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_5)
data_41_6 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_6)
data_41_7 <- readxl::read_xlsx(absolute_path, sheet = sheet_41_7)

# Define the questions for each question set
# These will be displayed as options in the dropdown menus in the UI
question_14_1 <- "Accessing Personal Health Information Electronically"
question_14_2 <- "Making Electronic Appointments with Regular Doctors"
question_14_4 <- "Viewing Electronic Referrals to Specialists"
question_14_5 <- "Virtual Visits with Healthcare Providers"
question_14_6 <- "Consulting About Health Issues Electronically"
question_14_7 <- "Telephone Consultations with Healthcare Providers"
question_14_8 <- "Participating in Remote Patient Monitoring Programs"
question_14_10 <- "Electronic Prescription Transmission to Pharmacies"
question_14_11 <- "Requesting Prescription Renewals Electronically"
question_14_12 <- "Accessing Technology for Mental Health and Wellness"
question_14_13 <- "Monitoring Health and Well-being Electronically"
question_14_14 <- "Receiving Electronic Reminders and Notifications"
question_14_16 <- "Accessing Clinical Notes from Medical Encounters"
question_41_1 <- "Notification of Health Risks"
question_41_2 <- "Communication Issues or Delays Between Healthcare Providers"
question_41_3 <- "Missing Health Record Information for Healthcare Provider"
question_41_4 <- "Redoing Diagnostic Procedures Due to Unavailable Results"
question_41_5 <- "Wasted Time Due to Ineffective Communication Among Providers"
question_41_6 <- "Compromised Health Due to Ineffective Communication"
question_41_7 <- "Frustration Due to Repeating Information to Different Providers"

# Combine all questions for each question set into lists
questions_14 <- c(question_14_1, question_14_2, question_14_4, question_14_5, 
                  question_14_6, question_14_7, question_14_8, question_14_10, 
                  question_14_11, question_14_12, question_14_13, question_14_14, 
                  question_14_16)
questions_41 <- c(question_41_2, question_41_3, question_41_4, 
                  question_41_5, question_41_6, question_41_7)



# Colors for the pie chart 1
response_colors_1 <- c("lightgreen", "pink", "lightblue")

# Create a function to process data based on the selected question, so that each sheet doesnt have to have its own code set to modfiy the contents
process_data_1 <- function(selected_question_1) {
  # Extract the corresponding data based on the selected question
  if (selected_question_1 == questions_14[1]) {
    data <- data_14_1
  } else if (selected_question_1 == questions_14[2]) {
    data <- data_14_2
  } else if (selected_question_1 == questions_14[3]) {
    data <- data_14_4
  } else if (selected_question_1 == questions_14[4]) {
    data <- data_14_5
  } else if (selected_question_1 == questions_14[5]) {
    data <- data_14_6
  } else if (selected_question_1 == questions_14[6]) {
    data <- data_14_7
  } else if (selected_question_1 == questions_14[7]) {
    data <- data_14_8
  } else if (selected_question_1 == questions_14[8]) {
    data <- data_14_10
  } else if (selected_question_1 == questions_14[9]) {
    data <- data_14_11
  } else if (selected_question_1 == questions_14[10]) {
    data <- data_14_12
  } else if (selected_question_1 == questions_14[11]) {
    data <- data_14_13
  } else if (selected_question_1 == questions_14[12]) {
    data <- data_14_14
  } else if (selected_question_1 == questions_14[13]) {
    data <- data_14_16
  } else {
    # Handle the case where the selected question is not recognized
    stop("Selected question not recognized.")
  }
  
  # Use the new row 1 as column headings
  colnames(data) <- data[2, ]
  # Remove the first two rows
  data_modified <- data[-(1:2), ]
  # Keep only the first two columns
  data_modified <- data_modified[, 1:2]
  colnames(data_modified) <- str_to_title(gsub(" ", "_", colnames(data_modified)))
  
  # Rename the first column to "Response"
  colnames(data_modified)[1] <- "Response"
  
  # Filter data based on responses
  filtered_data <- data_modified[trimws(data_modified$Response) %in% c("Yes", "No", "Don't know"), ]
  new_row <- c(data_modified[7, 1], data_modified[7, 2])
  filtered_data <- rbind(filtered_data, new_row)
  
  return(filtered_data) #the final form of the data when function is called
}

# Colors for the pie chart 1
response_colors_2 <- c("lightgreen", "pink", "grey", "lightblue", "orange")

# Create a function to process data based on the selected question (for second set), so that each sheet doesn't have to have its own code set to modify the contents
process_data_2 <- function(selected_question_2) {
  # Extract the corresponding data based on the selected question
  if (selected_question_2 == questions_41[1]) {
    data <- data_41_2
  } else if (selected_question_2 == questions_41[2]) {
    data <- data_41_3
  } else if (selected_question_2 == questions_41[3]) {
    data <- data_41_4
  } else if (selected_question_2 == questions_41[4]) {
    data <- data_41_5
  } else if (selected_question_2 == questions_41[5]) {
    data <- data_41_6
  } else if (selected_question_2 == questions_41[6]) {
    data <- data_41_7
  } else {
    # Handle the case where the selected question is not recognized
    stop("Selected question not recognized.")
  }
  
  # Use the new row 1 as column headings
  colnames(data) <- data[2, ]
  # Remove the first two rows
  data_modified <- data[-(1:2), ]
  # Keep only the first two columns
  data_modified <- data_modified[, 1:2]
  colnames(data_modified) <- str_to_title(gsub(" ", "_", colnames(data_modified)))
  
  # Rename the first column to "Response"
  colnames(data_modified)[1] <- "Response"
  
  # Filter data based on responses
  filtered_data <- data_modified[trimws(data_modified$Response) %in% c("Yes", "No", "Not applicable", "Don't know", "Prefer not to answer"), ]
  new_row <- c(data_modified[7, 1], data_modified[7, 2])
  filtered_data <- rbind(filtered_data, new_row)
  
  return(filtered_data)
}

#code for displaying the ui
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  titlePanel("From Data to Insight: Visualizing the Canadian Health System"),
  mainPanel(
    navbarPage(
      "",  # Navigation bar title is set to blank
      tabPanel("Intro", # first tab with jsut a text box
               fluidPage(tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                         textOutput("maintext"))),
      tabPanel("Question Set 14",  # second tab with its own drop down menu, graph, and text block
               fluidRow(
                 column(6, selectInput("question_select_1", "Select Question", choices = questions_14)),    ),
               column(8,  
                      plotlyOutput("pieChart_1", height = "500px", width = "800px")),  # Adjust height and width as needed
               column(12,  
                      fluidRow(tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                        textOutput("question14text")
                      ))  # Add text output
      ),
      tabPanel("Question Set 41",  # third tab with its own drop down menu, graph, and text block
               fluidRow(
                 column(6, selectInput("question_select_2", "Select Question", choices = questions_41)),    ),
               column(8,  
                      plotlyOutput("pieChart_2", height = "500px", width = "800px")),  # Adjust height and width as needed
               column(12,  
                      fluidRow(tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                               textOutput("question41text")
                      ))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  
  # The text to be displayed in the "Intro" tab
  output$maintext <- renderPrint({
    cat(paste0("The visualizations presented here serve as a window into the patient experiences and challenges within the existing healthcare communication and coordination landscape. These insights are useful for identifying key pain points that individuals encounter in their interactions with healthcare providers. By depicting instances such as notifications of health risks, communication issues, missing health record information, and the need for repeated diagnostic procedures, these visualizations aim to shed light on the gaps and inefficiencies in the current healthcare system. The overarching goal of a Canadian Electronic Health System (EHS) is to enhance the overall quality and effectiveness of healthcare delivery. By understanding these specific challenges, the EHS project can strategically focus on implementing solutions that address communication breakdowns, streamline information-sharing, and ultimately optimize the patient experience. These visualizations play a role in aligning the project's objectives with the real-world experiences of patients, guiding the development of a more patient-centric and efficient healthcare ecosystem.")
  )})
  
  # Container for the the selected question menu for question set 14
  selected_question_1 <- reactive({
    input$question_select_1
  })
  
  # Define what happens to data based on the selected question for question set 14
  filtered_data_1 <- reactive({
    process_data_1(selected_question_1())
  })
  
  # Create a pie chart for question set 14
  output$pieChart_1 <- renderPlotly({
    
    plot_ly(
      labels = filtered_data_1()$Response,
      values = filtered_data_1()$Total,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hoverinfo = "none",  # Set hoverinfo to "none" to disable hover text
      marker = list(colors = response_colors_1)
    ) %>%
      layout(title = "Responses", showlegend = FALSE, font = list(family = "Arial") ) %>%
      config(displayModeBar = FALSE) # Remove the plotly modebar
    
  })
  
  # The text to be displayed for question set 14
  output$question14text <- renderPrint({
    cat(paste0("Q14 Set - Digitally Enabled Health Services: The Q14 set of questions delves into the preferences and interests of survey respondents regarding digitally enabled health services. Covering a spectrum of services from electronic access to personal health information, virtual visits with healthcare providers, prescription management, and mental health support, these questions collectively paint a picture of the respondents' inclination towards adopting technology-driven solutions in their healthcare journeys. The overarching theme revolves around understanding the level of interest and acceptance of modern, digitally facilitated healthcare services among the surveyed individuals. Insights from this set guide the Electronic Health System (EHS) project in tailoring its digital infrastructure to align with the preferences and needs of healthcare consumers, fostering a more accessible and patient-friendly healthcare landscape."
)
    )})
  
  
  # Container for the the selected question menu for question set 41
  selected_question_2 <- reactive({
    input$question_select_2
  })
  
  # Define what happens to data based on the selected question for question set 41
  filtered_data_2 <- reactive({
    process_data_2(selected_question_2())
  })
  
  # Create a pie chart for question set 41
  output$pieChart_2 <- renderPlotly({
    
    plot_ly(
      labels = filtered_data_2()$Response,
      values = filtered_data_2()$Total,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      hoverinfo = "none",  # Set hoverinfo to "none" to disable hover text
      marker = list(colors = response_colors_2)
    ) %>%
      layout(title = "Responses", showlegend = FALSE, font = list(family = "Arial") ) %>%
      config(displayModeBar = FALSE)  # Remove the plotly modebar
  })
  
  
  # The text to be displayed for question set 41
  output$question41text <- renderPrint({
    cat(paste0("Q41 Set - Healthcare Communication and Coordination: The Q41 set of questions addresses the crucial aspects of healthcare communication and coordination. These questions shed light on the challenges faced by individuals in their interactions with healthcare providers, ranging from notification of health risks to communication breakdowns and compromised health due to ineffective information sharing. The overarching theme here revolves around the examination of the existing gaps in communication and coordination within the healthcare system. By understanding these challenges, the EHS project aims to implement solutions that bridge these gaps, ensuring seamless communication among healthcare providers and optimizing the overall patient experience. This set of questions contributes valuable insights to the project's objective of creating a more cohesive and efficient healthcare ecosystem.")
    )})
}

# Run the Shiny app
shinyApp(ui, server)