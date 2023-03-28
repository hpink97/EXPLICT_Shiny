library(shiny)
library(readr)
library(dplyr)
library(DT)
library(ggplot2)

# load data
path <- "G:/Shared drives/Denby Lab Team Drive/Lab members/Harry/R functions/Data/Geng2021_TF_target_prediction.Rds"
data <- readRDS(path)

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("EXPLICIT (Expression Prediction via Log-linear Combination of Transcription Factors) Arabidopsis Model"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      
      # Input selection
      selectInput(inputId = "input_col",
                  label = "Choose a column to filter by:",
                  choices = c("TF", "Target_gene")),
      
      # Input text box
      textInput(inputId = "input_val",
                label = "Enter an Arabidopsis gene id (e.g. AT4G00480)"),
      
      # Thank you/error message
      verbatimTextOutput(outputId = "thankyou_text")
      
    ),
    
    # Main panel
    mainPanel(
      # Button
      downloadButton("downloadData", "Download"),
      # Filtered data table
      DT::dataTableOutput(outputId = "filtered_data"),
      
      # Density plot
      plotOutput(outputId = "density_plot")
      
    )
    
  )
  
)

# Define server logic
server <- function(input, output) {
  
  # Filtered data
  filtered_data <- reactive({
    if (!is.na(input$input_val)) {
      if (input$input_val %in% data[[input$input_col]]) {
        data[data[input$input_col]==input$input_val,] %>% arrange(p.value)
      } else {
        return(NULL)
      }
    } else {
      data
    }
  })
  
  # Thank you/error message
  output$thankyou_text <- renderText({
    if(!is.null(filtered_data())) {
      paste("Thank you for choosing", input$input_val,"as your",input$input_col ,"!")
    } else {
      paste("Error: chosen",input$input_val, "is not valid, please try of;",paste(unique(data[[input$input_col]])[1:5],collapse = ', ' ))
    }
  })
  
  # Filtered data table
  output$filtered_data <- DT::renderDataTable({
    filtered_data()
  })
  
  # Density plot
  output$density_plot <- renderPlot({
    if (!is.null(filtered_data())) {
      ggplot(data = data %>% mutate(group = 'All Predictions') %>% 
               bind_rows(filtered_data() %>% mutate(group = paste0(input$input_val, 'Predictions') )),
             mapping = aes(Coefficient, fill=group)) + 
        geom_density(alpha=0.35) +
        labs(x = "Prediction Coefficient", y = "Density", title = "Comparision of prediction coefficients ") +
        xlim(c(min(filtered_data()$Coefficient)*1.2,
               max(filtered_data()$Coefficient)*1.2))+
        theme_minimal()+
        theme(legend.position = 'bottom',
              legend.text=element_text(size=14),
              axis.text = element_text(size=13),
              axis.title = element_text(size=15))
    }
  })
  
  
  # Observer for download button
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$input_val,'_EXPLICT_predictions', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
