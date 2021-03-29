#### Load packages and settings ####
# Load require packages
library(shiny)
library(tidyverse)
library(lubridate)
library(readxl)
library(readr)
library(stringr)
library(shinyWidgets)

# Settings: Increase maximum file upload size to 30 megabytes
options(shiny.maxRequestSize = 30*1024^2)

#### UI Function ####
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Input settings"),
    
    # Sidebar (settings)
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput(inputId = "Import_Data", 
                      label = "Step 1: Import CSV file",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Input: Select separator ----
            helpText("Optional: Choose what is separating the values in the file"),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            ## Select box: choose the column with clients' name
            #selectInput(inputId = "Name_col", 
            #            label = "Step 2: Select the column with clients' names:",
            #            choices = ""),
            
            # Select box: choose the column with transaction date
            selectInput(inputId = "Date_col", 
                        label = "Step 2: Select the column with the transaction date",
                        choices = ""),
            # Radio buttons: choose the periodicity of the transactions
            selectInput(inputId = "Format",
                        label = "Step 3: Select the format of the date",
                        choices = c("Day/Month/Year" = 1,
                                    "Month/Day/Year" = 2,
                                    "Year/Month/Day" = 3,
                                    "Year/Day/Month" = 4,
                                    "Month/Year/Day" = 5,
                                    "Day/Year/Month" = 6),
                        selected = 1),
            # Select box: choose the column with the price amount
            selectInput(inputId = "Amount_col", 
                        label = "Step 4: Select the column with the payment amount",
                        choices = ""),
            
            # Radio buttons: choose the periodicity of the transactions
            radioButtons(inputId = "Period",
                         label = "Step 5: Select the period of the current data",
                         choices = c("Annual" = 12,
                                     "Semi-annual" = 6,
                                     "Quarterly" = 4),
                         selected = 12),
            # Button to check that the sum of input values is equal to the sum of export values
            strong("Step 6: Check that input values are equal to export values"),
            actionButton(inputId = "Check",
                         label = "Check",
                         icon = icon(name = "check"),
                         class = "fas fa-check"),
            # New line
            div(),
            # Warning output whether the computation was correct
            textOutput(outputId = "Warning"),
            # Button: Download the transformed data
            strong("Step 7: Download the converted data "),
            downloadButton(outputId ="downloadData",
                           label = "Download"),
            
            #        pickerInput(
            #            inputId = "myPicker", 
            #            label = "Select/deselect columns to be included in Export", 
            #            choices = "", 
            #            options = list(
            #                `actions-box` = TRUE,
            #                `liveSearch` = TRUE,
            #                size = 10,
            #                `selected-text-format` = "count > 3"
            #            ), 
            #            multiple = TRUE)
        ),
        
        
        # Main panel display (display)
        mainPanel(
            # Output: Tabs with tables showing data before and after ----
            tabsetPanel(type = "tabs",
                        # Create the "Import Data" tab and puts the table in it
                        tabPanel("Imported Data", dataTableOutput("Import_Table")),
                        # Create the "Export Data" tab and puts the table in it
                        tabPanel("Exported Data", dataTableOutput("Export_Table")))
        )
    )
)

#### Server Function ####
# Define server logic to display and download selected file ----
server <- function(input, output, session) {
    
    Raw_Data <- reactive({
        
        req(input$Import_Data)
        
        df <- read.csv(input$Import_Data$datapath,
                       header = TRUE,
                       sep = input$sep)
        
    })
    
    # Reactive data table for imported data
    output$Import_Table <- renderDataTable({
        
        # input$Import_Data will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$Import_Data)
        
        return(Raw_Data())
        
    })
    
    # Reactively change the column name input labels
    observe({
        
        updateSelectInput(inputId = "Name_col",
                          choices = colnames(Raw_Data())
        )
    })
    
    # Reactively change the date input labels
    observe({
        
        updateSelectInput(inputId = "Date_col",
                          choices = colnames(Raw_Data())
        )
    })
    
    # Reactively change the amount input labels
    observe({
        
        updateSelectInput(inputId = "Amount_col",
                          choices = colnames(Raw_Data())
        )
    })
    
    observeEvent(input$Import_Data, {
        
        updatePickerInput(inputId = "myPicker",
                          choices = colnames(Raw_Data()),
                          session = session,
        )
        
    }, ignoreInit = TRUE)
    
    
    Clean_Data <- reactive({
        
        Clean_Data <- Raw_Data() %>% 
            mutate(Original_Amount = parse_number(as.character(.data[[input$Amount_col]])),
                   Accrual_Amount = Original_Amount)
        
    })
    
    
    # Code to clean the data and split it into monthly transactions
    Export_Data <- reactive({
        
        # If statements to choose the proper way to format the date based on input$Date_col
        if (input$Format == 1){
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = dmy(.data[[input$Date_col]]))    
            
        } else if (input$Format == 2) {
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = mdy(.data[[input$Date_col]]))    
            
        } else if (input$Format == 3) {
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = ymd(.data[[input$Date_col]]))
            
        } else if (input$Format == 4) {
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = ydm(.data[[input$Date_col]]))
            
        } else if (input$Format == 5) {
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = myd(.data[[input$Date_col]]))
            
        } else if (input$Format == 6) {
            
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = dym(.data[[input$Date_col]]))
            
        }
        
        # Create a repeating sequence of numbers for each month of accrual
        Accrual_month <- rep(0:(as.numeric(input$Period) - 1), nrow(Filtered_Data))
        
        # Duplicate the data based on input$Period (frequency of transactions)
        Computed_Data <- Filtered_Data %>% 
            slice(rep(1:n(), each = as.numeric(input$Period))) %>% 
            # Divide the payment amount by the frequency of transactions
            mutate(Accrual_Amount = Accrual_Amount / as.numeric(input$Period),
                   Date_Round = ymd(paste0(year(Date),"-",month(Date),"-",1))) %>% 
            cbind(Accrual_month) %>%
            # Create new variable "Accrual Date" by spreading costs over the next x months
            mutate(Date_Accrual = Date_Round + (Accrual_month*months(1))) %>% 
            select(!c(Date_Round, Accrual_month))
    }) 
    
    # Reactive data table for exported data
    output$Export_Table <- renderDataTable({
        
        return(Export_Data())
        
    })
    
    # Event to check whether the sum of input data is equal to the sum of export data
    Warning <- eventReactive(input$Check, ignoreInit = TRUE, {
        
        # Calculate the sum of data before computations
        Import_Sum <- Clean_Data() %>% 
            summarise(sum = sum(Original_Amount))
        
        # Calculate the sum of data after computations
        Export_Sum <- Export_Data() %>%
            summarise(sum = sum(Accrual_Amount))
        
        if (Import_Sum == Export_Sum) {
            print("Correct: proceed to download")
        } else {
            print("Error: Make sure that the settings above are correct")
        }
    })
    
    output$Warning <- renderText({
        
        return(Warning())
        
    })
    
    # Download csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(word(input$Import_Data),"_converted.csv")
        },
        content = function(file) {
            write.csv(Export_Data(), file, row.names = FALSE)
        }
    )
}

#### Run the application ####
shinyApp(ui = ui, server = server)