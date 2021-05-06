# app server

# Libraries ----
library(shiny)
library(tidyverse)
library(forcats)
library(here)

# Data ----
Benthic_Groups <- read_csv(here("Data/Benthic_Algae_Species.csv"))
Benthic_Groups <- Benthic_Groups[c(1:4)] # remove extra columns

Data <- read_csv(here("Data", "clean_MCR_Benthic_Cover.csv"))

# base plot function ----
baseplot <- function(data){
    ggplot(data, aes(y=mean_PercentCover, x=Year, color=Functional_Group)) + 
        geom_point(shape=20, size=2) +
        theme_classic() +
        theme(strip.background = element_rect(fill = "white", color = "white"), 
              panel.spacing = unit(.1, "lines"), 
              strip.text.x = element_text(size=10, face="bold"), 
              strip.text.y = element_text(angle = 0,
                                          margin = margin(2, 0, 2, 5, unit = "pt")),
              legend.position = "right", legend.justification="top",
              legend.text = element_text(size=9), legend.title = element_blank(),
              legend.spacing.y = unit(1, "pt"), legend.key.size = unit(12,"pt"),
              legend.box.spacing = unit(0.5, "pt"), legend.margin = margin(0),
              axis.title = element_text(size=9),
              axis.text.x = element_text(size=7, angle=45, vjust = 0.5, face="plain",
                                         color=c("grey30",0 ,"grey30",0))) +
        scale_x_continuous(breaks = seq(from=2005, to=2020, by=1)) + 
        labs(y="Percent Cover", color="Functional Group") + 
        scale_color_manual(values=c("deepskyblue", "chocolate2", "mediumpurple", "seagreen4"))
    
}

# Server ----

shinyServer(function(input, output) {
    
    # Data Tab ----
    options(shiny.maxRequestSize=30*1024^2) # make sure it will take large file
    
    # Upload data output table --------
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                df <- read_csv(input$file1$datapath)
            },
            error = function(e) {
                # return a safe Error if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        # clean data ----
        df_clean <- left_join(df, Benthic_Groups, by=c("Taxonomy_Substrate_Functional_Group")) %>% # add benthic groups
            rowwise() %>% 
            mutate(Habitat_4 = recode(Habitat, 
                                      "Fringing" = "Fringing Reef",
                                      "Backreef" = "Back Reef", 
                                      "Outer 10" = "Fore Reef 10m", 
                                      "Outer 17" = "Fore Reef 17m"), # habitat column with Fore reef groups seperate
                   Habitat = recode(Habitat, 
                                    "Fringing" = "Fringing Reef",
                                    "Backreef" = "Back Reef", 
                                    "Outer 10" = "Fore Reef", 
                                    "Outer 17" = "Fore Reef")) %>% # habitat column with Fore reef groups together
            rename(Substrate_Taxa = Taxonomy_Substrate_Functional_Group) 
        
        df_clean <- df_clean[-c(187522),] # remove weird last row
        
        Data <- df_clean %>% select(c(1:9, 11)) %>%
            rename(Functional_Group = Course_FG) %>%
            mutate(Transect = as.factor(Transect), 
                   Quadrat = as.factor(Quadrat))
        
        # display output ----
        if(input$disp == "head") {
            return(head(Data))
        }
        else {
            return(Data)
        } 
        
    }) # end render table
    
    
    ## download data button ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("clean_", input$file1, sep = "")
        },
        content = function(file) {
            write.csv(Data, file, row.names = FALSE)
        }
    ) # end button
    
    
    # Plot page ----
    plot_data <- reactive({
        req(input$plot_type)
        req(input$plot_habitats)
        req(input$plot_sites)
        req(input$plot_points)
        
        Data_2 <- Data %>% 
            filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
            group_by(Year, Location, Habitat, Site, Transect, Quadrat, Functional_Group) %>%
            summarize(sum=sum(Percent_Cover)) %>% 
            group_by(Year, Habitat, Functional_Group) %>%
            summarize(mean_PercentCover=mean(sum),
                      se_PercentCover=sd(sum, na.rm=TRUE)/sqrt(length(na.omit(sum)))) %>%
            
    })
    
    ## plot1 ----
    output$plot1 <- renderPlot({
        input$plot_type
        input$plot_habitats
        input$plot_sites
        input$plot_points
        
        if (input$plot_type == "Island" || input$plot_type == "Both") {
            plot1 <- Data %>% filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
                group_by(Year, Location, Habitat, Site, Transect, Quadrat, Functional_Group) %>%
                summarize(sum=sum(Percent_Cover)) %>% 
                group_by(Year, Habitat, Functional_Group) %>%
                summarize(mean_PercentCover=mean(sum),
                          se_PercentCover=sd(sum, na.rm=TRUE)/sqrt(length(na.omit(sum)))) %>%
                baseplot() +
                facet_rep_wrap(.~Habitat) +
                geom_line() + 
                geom_errorbar(aes(ymax=mean_PercentCover+se_PercentCover, 
                                  ymin=mean_PercentCover-se_PercentCover), size=0.3)
            
            plot1
            
        } else if (input$plot_type == "Site") {
            plot1 <- Data %>% filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
                group_by(Year, Location, Habitat, Site, Transect, Quadrat, Functional_Group) %>%
                summarize(sum=sum(Percent_Cover)) %>% 
                group_by(Year, Habitat, Site, Functional_Group) %>%
                summarize(mean_PercentCover=mean(sum),
                          se_PercentCover=sd(sum, na.rm=TRUE)/sqrt(length(na.omit(sum)))) %>%
                geom_line() + 
                geom_point(shape=20) +
                facet_rep_grid(Site~Habitat) + 
                theme_classic() +
                theme(panel.spacing = unit(.5, "lines"), 
                      strip.background = element_rect(color = "white"), 
                      strip.text.x = element_text(size=8, face = "bold"),
                      legend.position = "bottom", legend.justification="left",
                      legend.text = element_text(size=7), legend.title = element_text(size=8),
                      legend.spacing.y = unit(1, "pt"), legend.key.size = unit(10,"pt"),
                      axis.text.x = element_text(angle=45, vjust = 0.5, size=8), 
                      axis.line=element_line()) +
                scale_x_continuous(breaks = seq(from=2005, to=2020, by=2)) + 
                labs(y="Mean Percent Cover", color="Functional Group") + 
                geom_errorbar(aes(ymax=mean_PercentCover+se_PercentCover, 
                                  ymin=mean_PercentCover-se_PercentCover), size=0.3, alpha=0.5)
            
            plot1
            
        }
    })
    
    ## plot1 save button ----
    output$downloadPlot <- downloadHandler(
        filename = function() { paste(input$title_plotI, '.png', sep='') },
        content = function(file) {
            ggsave(file, plot = plot1(), device = "png")
        }
    )
    
    
    
}) # end server

