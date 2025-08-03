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
  select(Tick_species) %>% 
  filter(!is.na(Tick_species)) # We don't need the NAs

# Ticks that do not have any match should be added to the "Tick_systematics" 
#section of the Googlesheet, correctly researched for systematics and re-run here 
# so we have a complete and robust work

view(Tick_S)


# Create index for paper

Tick_S <- Tick_S %>%
  mutate(
    Code = paste0(
      substr(Country, 1, 1),
      substr(Country, 3, 3),
      substr(Country, nchar(Country), nchar(Country)),
      Index
    ) %>% toupper()
  )


library(factoextra)
library(FactoMineR)



  Tick_S %>%
  group_by(`Recent taxonomy`) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  print(n= 100) # We can see the top most represented tick species
  

  
Tick_contigency <- Tick_S %>% 
  filter(Survey == "Yes" ) %>% 
  filter(`Recent taxonomy` %in% c('Amblyomma variegatum',
                                  'Rhipicephalus microplus',
                                  'Rhipicephalus decoloratus',
                                  'Hyalomma truncatum',
                                  'Rhipicephalus annulatus',
                                  'Rhipicephalus geigyi',
                                  'Hyalomma rufipes',
                                  'Rhipicephalus sanguineus')) %>% 
  select(`Recent taxonomy`, Host_group) %>% 
  group_by(`Recent taxonomy`, Host_group) %>% 
  summarise(countt = n()) %>% 
  pivot_wider(names_from = `Recent taxonomy`,
              values_from = countt) %>% 
  filter(!is.na(Host_group)) %>% 
  as.data.frame()
 

Tick_contigency <- Tick_contigency %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

row.names(Tick_contigency) <- Tick_contigency$Host_group
Tick_contigency <- Tick_contigency[, -1]  ### Careful here!!!


chisq.test(Tick_contigency)

ca.Tick  <- CA(Tick_contigency, graph = TRUE)
summary(ca.Tick)

eig_tick<- get_eigenvalue(ca.Tick)

fviz_screeplot(ca.Tick, addlabels = TRUE, ylim = c(0, 50)) ## 100


biplot_mosq <-fviz_ca_biplot(ca.Tick, alpha.col = 0.5,
                             map ="colgreen", arrow = c(FALSE, TRUE),
                             repel = TRUE,
                             col.col = "black", 
                             col.row = "red",
                             pointsize = 2.5, size.text = 2)+
  theme_classic()


fviz_ca_row(ca.Tick)


# Number of occurence of tick species per study

ca.Tick
# --- at each research article level, the number of tick occurrence

  Tick_S %>% 
  filter(Survey == "Yes" ) %>% 
  filter(`Recent taxonomy` %in% c('Amblyomma variegatum',
                                  'Rhipicephalus microplus',
                                  'Rhipicephalus decoloratus',
                                  'Hyalomma truncatum',
                                  'Rhipicephalus annulatus',
                                  'Rhipicephalus geigyi',
                                  'Hyalomma rufipes',
                                  'Rhipicephalus sanguineus')) %>% 
  select(`Recent taxonomy`, Host_group, Code) %>% 
  group_by(Code, `Recent taxonomy`, Host_group) %>% 
  summarise(countt = n()) %>% 
    pivot_wider(names_from = `Recent taxonomy`,
                values_from = countt) %>% 
    filter(!is.na(Host_group), !is.na(Code)) %>% 
    mutate(across(everything(), ~replace_na(.x, 0))) %>% 
    as.data.frame() %>% 
    view()





