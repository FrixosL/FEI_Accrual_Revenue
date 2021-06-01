#### Load packages and settings ####
# Load require packages
library(shiny) # Framework for web apps
library(tidyverse) # Colletion of multiple plugins for various applications
library(lubridate) # Plugin to manipulate "date" data
library(readxl) # Plugin to load Excel worksheets
library(readr) # Plugin to load CSV files
library(stringr) # Plugin to manipulate "strings"
library(shinyWidgets) # Plugin for web application widgets

# Settings: Increase maximum file upload size to 30 megabytes
options(shiny.maxRequestSize = 30*1024^2)

#### UI Function ####
# Define UI for application. Anything in this section dictates what the 
# application will display
ui <- fluidPage(
    
    # Application title
    titlePanel("Input settings"),
    
    # Sidebar (settings)
    sidebarLayout(
        sidebarPanel(
            
            # Input: Select a file ----
            # inputID allows you to link the input/output with computation part
            fileInput(inputId = "Import_Data",
                      # Label of the input widget
                      label = "Step 1: Import CSV file",
                      # Doesn't allow multiple files uploaded at once
                      multiple = FALSE,
                      # Describes what file types are accepted (only CSVs atm)
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Input: Select separator ----
            helpText("Optional: Choose what is separating the values in the file"),
            # Select the separator for the CSV files (should be left as a comma
            #  unless very specific case)
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
            # This will decide how many times each transaction is split into
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
            
            # New line (just adding spacing)
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
        
        
        # Main panel display (right-hand section)
        mainPanel(
            # Output: Tabs with tables showing data before and after ----
            tabsetPanel(type = "tabs",
                        # Create the "Import Data" tab and puts the table in it
                        # Shows the imported data in their raw format
                        tabPanel("Imported Data", dataTableOutput("Import_Table")),
                        # Create the "Export Data" tab and puts the table in it
                        # Shows the data after being transformed
                        tabPanel("Exported Data", dataTableOutput("Export_Table")))
        )
    )
)

#### Server Function ####
# Define server logic to display and download selected file ----
server <- function(input, output, session) {
    
    # Names the uploaded files as "Raw_Data"
    Raw_Data <- reactive({
        
        # Makes this a required input
        req(input$Import_Data)
        
        # Reads the CSV file uploaded before
        df <- read.csv(input$Import_Data$datapath,
                       header = TRUE,
                       sep = input$sep)
        
    })
    
    # Reactive data table for imported data (first tab)
    output$Import_Table <- renderDataTable({
        
        # input$Import_Data will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$Import_Data)
        
        return(Raw_Data())
        
    })
    
    # Reactively change the column name input labels. After uploading the data
    # the input options change to include all columns in the file.
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
    
    # Saves the data to "Clean_Data"
    Clean_Data <- reactive({
        
        Clean_Data <- Raw_Data() %>% 
            # Creates a new variable called "Original_Amount" which is the clean
            # number from the column which was selected from the left panel
            mutate(Original_Amount = parse_number(as.character(.data[[input$Amount_col]])),
                   # Placeholder for "Accrual_Amount"
                   Accrual_Amount = Original_Amount)
    })
    
    # Code to clean the data and split it into monthly transactions
    Export_Data <- reactive({
        
        # If statements to choose the proper way to format 
        # the date based on input$Date_col.
        # Changes the way the "date" is read based on the format which the user
        # specifies on the settings panel (left-hand column).
        
        if (input$Format == 1){
            
            # Day/Month/Year
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = dmy(.data[[input$Date_col]]))    
            
        } else if (input$Format == 2) {
            
            # Month/Day/Year
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = mdy(.data[[input$Date_col]]))    
            
        } else if (input$Format == 3) {
            
            # Year/Month/Date
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = ymd(.data[[input$Date_col]]))
            
        } else if (input$Format == 4) {
            
            # Year/Date/Month
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = ydm(.data[[input$Date_col]]))
            
        } else if (input$Format == 5) {
            
            # Month/Year/Date
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = myd(.data[[input$Date_col]]))
            
        } else if (input$Format == 6) {
            
            # Day/Year/Month
            Filtered_Data <- Clean_Data() %>%
                mutate(Date = dym(.data[[input$Date_col]]))
            
        }
        
        # Create a repeating sequence of numbers for each month of accrual.#
        # For annual transactions this would simply be a count from zero to eleven
        # repeatdly based on the number of initial transactions.
        Accrual_month <- rep(0:(as.numeric(input$Period) - 1), nrow(Filtered_Data))
        
        # Duplicate the data based on input$Period (frequency of transactions)
        Computed_Data <- Filtered_Data %>% 
            slice(rep(1:n(), each = as.numeric(input$Period))) %>% 
            # Divide the payment amount by the frequency of transactions
            mutate(Accrual_Amount = Accrual_Amount / as.numeric(input$Period),
                   # Rounds the date to the first of the month to avoid 2 months
                   # worth of the transaction falls into a single month.
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
        
        # Checks whether the sum of inputs is equal to the sum of outputs
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