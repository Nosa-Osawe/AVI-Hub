library(tidyverse)
library(readxl)
library(googlesheets4)

gs4_auth() # You need this to authenticate the google sheet

getwd()

W_systematics <- read_sheet("https://docs.google.com/spreadsheets/d/1mKCD-FHixwMZxUcny2UeyLAk353QCoTzcXdrIkD9W7Y/edit?usp=sharing",
                      sheet = "Tick_systemtatics") %>% 
  rename("Tick_species" = "Species name (as entered in the google sheet)")


WT_merged <- read.csv("Data/merged.westafrica.data.28.07.2025.csv")

Tick_S <- left_join(WT_merged, W_systematics, by = "Tick_species" )

Tick_S %>% 
  select(Tick_species,`Correct Taxonomy`,`Recent taxonomy`,`ITIS (TSN)`) %>% 
  view()


# Tick species that do not have any matching systematic
anti_join(WT_merged, W_systematics, by = "Tick_species") %>%
  as.data.frame() %>% 
  select("Tick_species") %>% 
  distinct("Tick_species")

# Ticks that do not have any match shoul be added to the "Tick_systemtics" 
#section of the Googlesheet, correctly researched for systematics and re-run here 
# so we have a complte and robust work
