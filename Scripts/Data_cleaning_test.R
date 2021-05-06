# data cleaning test script

# libraries
library(tidyverse)
library(forcats)
library(here)

# Data files

# add raw MCR data
df <- read.csv(here("Data/MCR_LTER_Annual_Survey_Benthic_Cover_20201221.csv"))

# benthic groups info
Benthic_Groups <- read.csv(here("Data/Benthic_Algae_Species.csv"))
Benthic_Groups <- Benthic_Groups[c(1:4)]

# clean data
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
  rename(Functional_Group = Course_FG)


write_csv(Data, here("Data","clean_MCR_Benthic_Cover.csv"))
