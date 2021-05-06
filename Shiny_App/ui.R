# Shiny App Layout

# Libraries
library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cerulean"),

    # Application title
    titlePanel("MCR-LTER Benthic Cover Data Analyzer"),
    
    navbarPage("Nav Title",
               # Home page ----------------
               tabPanel(icon("home"),
                        ## picture ----
                        fluidRow(column(3, align="center", 
                                        div(style="display: inline-block; width: 33%;", 
                                            img(src="moorea_wideview.jpg", height="260px")) 
                                        )
                                 ),
                        ## text boxes ----
                        fluidRow(column(
                            br(),
                            
                            p("Text Box", 
                              strong("bold text"), "more text",
                              style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                            
                            br(),
                            
                            p("text box 2", 
                              style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                            
                            width=8) # width for text boxes
                            ),
                        
                        ## markdown file ----
                        fluidRow(column(3, includeMarkdown("Home.md"))
                        )
                                 
                        ), # end tab home
               
               # Data page -------------------------------         
               tabPanel("Data",
                        fluidPage(
                            ## Page title ----
                            titlePanel("Uploading MCR Data"),
                            
                            ## instructions for data ----
                            fluidRow(column(10,
                                p("This will clean and allow you to use the MCR-LTER benthic cover of algae and other community components data set which can be found",
                                  span(a(href='http://mcrlter.msi.ucsb.edu/cgi-bin/showDataset.cgi?docid=knb-lter-mcr.8',"here.")),
                                  "The data is collected every year along fixed transects on the LTER sites 1-6 around the island. 
                                  The file is very large so give it some time to load.",
                                br(),
                                span(br(),
                                     "Data Set Information:", 
                                     br(),
                                     strong("Title"), "MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, ongoing since 2005",
                                     br(),
                                     strong("Citation"), "Carpenter, R of Moorea Coral Reef LTER. 2020. 
                                  MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, 
                                  ongoing since 2005. knb-lter-mcr.8.32 doi:10.6073/pasta/0bf200e9e0f099de69826f57b18ff3da",
                                     style="font-size:12px;"),
                                style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                                ), # end text
                                offset = 1) # end text bubble - column
                                ), # end fluid row
                             
                            ## Sidebar layout with input and output definitions ----
                            sidebarLayout(
                                
                                ### Sidebar panel for inputs ----
                                sidebarPanel(
                                    
                                    #### Input: Select a file ----
                                    fileInput("file1", "Choose CSV File",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    
                                    #### Horizontal line ----
                                    hr(),
                                    
                                    #### Input: Select number of rows to display ----
                                    radioButtons("disp", "Display",
                                                 choices = c(Head = "head",
                                                             All = "all"),
                                                 selected = "head"),
                                    
                                    width=3), # end sidebar Panel
                                
                                ### Main panel for displaying outputs ----
                                mainPanel(
                                    #### download button ----
                                    fluidRow(downloadButton("downloadData", "Download")),
                                    
                                    #### Output: Data file ----
                                    tableOutput("contents")
                                    
                                ) # end main panel  
                            ) # end sidebar layout   
                        ) # end fluid page
                        ), ## end tab Data 
               
               # Plot page -------------------------
               tabPanel("Plots",
                        fluidPage(
                            ## custom CSS ----
                            tags$head(
                                tags$style(HTML("
                                /* Smaller font for preformatted text */
                                pre, table.table {
                                font-size: smaller;
                                }
                                
                                body {
                                min-height: 2000px;
                                }
                                
                                .option-group {
                                border: 1px solid #ccc;
                                border-radius: 6px;
                                padding: 0px 5px;
                                margin: 5px -10px;
                                background-color: #f5f5f5;
                                }
                                
                                .option-header {
                                color: #79d;
                                text-transform: uppercase;
                                margin-bottom: 5px;
                                }
                                                "))
                            ),
                            
                            ## Page title ----
                            titlePanel("Plot Data"),
                            
                            ## Input Options ----
                            fluidRow(column(3, div(class = "option-group",
                                    ### Select scale - box 1 ----               
                                    radioButtons("plot_type", label = h4("Select Scale"), 
                                                choices = c("Island", "Site", "Both"), 
                                                inline=TRUE),
                                    
                                    checkboxGroupInput("plot_habitats", "Habitats", 
                                                       choices = c("Fringing Reef", "Back Reef", "Fore Reef"),
                                                       selected = c("Fringing Reef", "Back Reef", "Fore Reef")),
                                    
                                    conditionalPanel("input.plot_type == 'Site' || input.plot_type == 'Both'",
                                                     checkboxGroupInput("plot_sites", "Include Sites", 
                                                                        choices = c("LTER 1" = "LTER 1", 
                                                                                    "LTER 2" = "LTER 2", 
                                                                                    "LTER 3" = "LTER 3",
                                                                                    "LTER 4" = "LTER 4", 
                                                                                    "LTER 5" = "LTER 5", 
                                                                                    "LTER 6" = "LTER 6"),
                                                                        selected = c("LTER 1","LTER 2","LTER 3",
                                                                                     "LTER 4","LTER 5","LTER 6")),
                                                     selectize = FALSE)
                                ) # end div
                            ), # end column 1
                            
                            ### right side ----
                            column(9,
                                   #### choose points - box 2 ---- 
                                   fluidRow(column(4, div(class = "option-group",
                                           conditionalPanel("input.plot_type === 'Island'",
                                                            selectInput("plot_points", label = h4("Points"), 
                                                                        choices = c("Means", "Sites", "Transects"))),
                                           
                                           conditionalPanel("input.plot_type === 'Site'",
                                                            selectInput("plot_points", label = h4("Points"), 
                                                                        choices = c("Means", "Transects"))),
                                           
                                           conditionalPanel("input.plot_type === 'Both'", h4("Points"),
                                                            div(style="display: inline-block;vertical-align:top; width: 100px;",
                                                                selectInput("plot_points", "Island Plot", 
                                                                                        choices = c("Means", "Sites", "Transects"))
                                                                            ),
                                                            div(style="display: inline-block;vertical-align:top; width: 100px;",
                                                                   selectInput("plot_points", "Sites Plot", 
                                                                               choices = c("Means", "Transects"))
                                                                   )) # end panel                  
                                           )),
                                           
                                           #### title box 3 ----
                                           column(5, textInput("title_plotI", label = h3("Plot Title"), value = "Enter text..."))
                                           
                                           ), # end div , column, fluid row
                                   
                                   #### Plot 1 ----
                                   fluidRow(plotOutput("plot1")),
                                   
                                   #### download button ----
                                   fluidRow(downloadButton("downloadPlot", "Download"))
                                       
                                   )  # end right side
                            
                             ), # end input options - fluid row
                            
                            br(),
                            
                            ### sites plots ----
                            fluidRow(column(12,
                                            p("plots go here", strong("plot2"), "more text",
                                              style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")
                                ))# end sites plots
                            
                        ) # end fluid page - plots
                        ), # end tab Plot
               
               # Analysis Island page ---------------------------
               tabPanel("Analysis: Island Wide", 
                        fluidPage(
                            titlePanel("Island Scale Analysis: linear mixed-effects models"),
                            
                            sidebarLayout(
                                sidebarPanel(
                                    checkboxGroupInput("select_habitats", "Habitats", 
                                                       choices = c("Fringing Reef", "Back Reef", "Fore Reef"),
                                                       selected = c("Fringing Reef", "Back Reef", "Fore Reef")),
                                    
                                    radioButtons("model_type", "Model Type", 
                                                 choices = c("gls", "lmer"),  inline=TRUE),
                                    
                                    radioButtons("transform", "Transformation",
                                                 choices = c("None", "log", "logit"), inline=TRUE),
                                width=3),
                                
                                mainPanel(
                                    tabsetPanel(
                                        tabPanel("plot", p("plots here")),
                                        tabPanel("qqp", p("plots here"))
                                    )
                                )
                            ) # end sidebar layout
                        ) # end fluid page
               ), # end tab Analysis Island
               
               
               # Analysis Sites page ---------------------------
               tabPanel("Analysis by Site")
               
               
               
               ) # end navbar
    ) # end theme
) # end UI
