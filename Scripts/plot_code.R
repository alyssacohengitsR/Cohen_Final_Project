# Code for the plots in app

# Libraries ----
library(tidyverse)
library(here)
library(lemon)

# Data ----
Data <- read_csv(here("Data", "clean_MCR_Benthic_Cover.csv"))

Benthic_Groups <- read_csv(here("Data/Benthic_Algae_Species.csv"))

# fix data ----
Benthic_Groups <- Benthic_Groups[c(1:4)] # remove extra columns

Data$Functional_Group <- as.factor(Data$Functional_Group)


# plot function
baseplot <- function(data){
  ggplot(data, aes(y=mean_PercentCover, x=Year, color=Functional_Group)) + 
    geom_point(shape=20, size=1) +
    theme_classic() +
    theme(strip.background = element_rect(fill = "white", color = "white"), 
          panel.spacing = unit(.1, "lines"), 
          strip.text.x = element_text(size=8, face="bold"), 
          strip.text.y = element_text(angle = 0,
                                      margin = margin(2, 0, 2, 5, unit = "pt")),
          legend.position = "right", legend.justification="top",
          legend.text = element_text(size=7), 
          legend.title = element_blank(),
          legend.spacing.y = unit(1, "pt"), legend.key.size = unit(10,"pt"),
          legend.box.spacing = unit(0.5, "pt"), legend.margin = margin(0),
          plot.title = element_text(size=13), axis.title = element_text(size=8),
          axis.text.x = element_text(size=7, angle=45, vjust = 0.5, face="plain",
                                     color=c("grey30",0 ,"grey30",0))) +
    scale_x_continuous(breaks = seq(from=2005, to=2020, by=1)) + 
    labs(y="Percent Cover", color="Functional Group") + 
    scale_color_manual(values=c("deepskyblue", "chocolate2", "mediumpurple", "seagreen4"))
  
}

# island scale ----

## points = means ----
Data %>% filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
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
  


## points = site ----
Data %>% filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
group_by(Year, Location, Habitat, Site, Transect, Quadrat, Functional_Group) %>%
  summarize(sum=sum(Percent_Cover)) %>% 
  group_by(Year, Habitat, Site, Functional_Group) %>%
  summarize(mean_PercentCover=mean(sum),
            se_PercentCover=sd(sum, na.rm=TRUE)/sqrt(length(na.omit(sum)))) %>%
  ggplot(aes(y=mean_PercentCover, x=Year, color=Functional_Group)) + 
  geom_point(shape=20, size=1) +  
  geom_smooth() +
  facet_rep_wrap(.~Habitat) +
  theme_classic() +
  theme(strip.background = element_rect(fill = "white", color = "white"), 
        panel.spacing = unit(.1, "lines"), 
        strip.text.x = element_text(size=8, face="bold"), 
        strip.text.y = element_text(angle = 0,
                                    margin = margin(2, 0, 2, 5, unit = "pt")),
        legend.position = "right", legend.justification="top",
        legend.text = element_text(size=7), 
        legend.title = element_blank(),
        legend.spacing.y = unit(1, "pt"), legend.key.size = unit(10,"pt"),
        legend.box.spacing = unit(0.5, "pt"), legend.margin = margin(0),
        plot.title = element_text(size=13), axis.title = element_text(size=8),
        axis.text.x = element_text(size=7, angle=45, vjust = 0.5, face="plain",
                                   color=c("grey30",0 ,"grey30",0))) +
  scale_x_continuous(breaks = seq(from=2005, to=2020, by=1)) + 
  labs(y="Percent Cover") + 
  scale_color_manual(values=c("deepskyblue", "violet", "chocolate2", "seagreen4"))


# site scale ----
Data %>% filter(Functional_Group %in% c("Algal Turf", "CCA", "Coral", "Macroalgae")) %>%
  group_by(Year, Location, Habitat, Site, Transect, Quadrat, Functional_Group) %>%
  summarize(sum=sum(Percent_Cover)) %>% 
  group_by(Year, Habitat, Site, Functional_Group) %>%
  summarize(mean_PercentCover=mean(sum),
            se_PercentCover=sd(sum, na.rm=TRUE)/sqrt(length(na.omit(sum)))) %>%
  baseplot() +
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




