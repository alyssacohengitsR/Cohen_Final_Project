# MCT-LTER Benthic Cover app

# Libraries
library(shiny)
library(markdown)

# Data

# Define UI for data upload app ----
ui <- 
    navbarPage("Nav",
               tabPanel("Home",
                        fluidRow(
                            column(6,
                                   includeMarkdown("Home.Rmd")
                            ),
                            column(3,
                                   img(class="img-polaroid",
                                       src=paste0("http://upload.wikimedia.org/",
                                                  "wikipedia/commons/9/92/",
                                                  "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                   tags$small(
                                       "Source: Photographed at the Bay State Antique ",
                                       "Automobile Club's July 10, 2005 show at the ",
                                       "Endicott Estate in Dedham, MA by ",
                                       a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                         "User:Sfoskett")
                                   )
                            )
                        )
               ),
                                   
               tabPanel("Data",
                        # Sidebar layout with input and output definitions ----
                        sidebarLayout(
                            # Sidebar panel for inputs ----
                            sidebarPanel(
                                # Input: Select a file ----
                                fileInput("file1", "Choose CSV File",
                                          multiple = FALSE,
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                                    
                                # Horizontal line ----
                                tags$hr(),
                                    
                                # Input: Checkbox if file has header ----
                                checkboxInput("header", "Header", TRUE),
                                    
                                # Input: Select separator ----
                                radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    
                                # Input: Select quotes ----
                                radioButtons("quote", "Quote",
                                                 choices = c(None = "",
                                                             "Double Quote" = '"',
                                                             "Single Quote" = "'"),
                                                 selected = '"'),
                                    
                                # Horizontal line ----
                                tags$hr(),
                                    
                                # Input: Select number of rows to display ----
                                radioButtons("disp", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head")
                                    
                                ) # end sidebar panel for file input
                        ), # end sidebar layout - data
                        # Main panel for displaying outputs ----
                        mainPanel(
                                    
                            # Output: Data file ----
                            tableOutput("contents")
                                    
                                )
                                
                            ) # end Data tab
                        )
    
    

# Define server logic to read selected file ----
server <- function(input, output) {
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
        
    })
    
}

# Create Shiny app ----
shinyApp(ui, server)