library(tidyverse)
library(readxl)
library(googlesheets4)



gs4_auth() # You need this to authenticate the google sheet

W.Enoch <- read_sheet("https://docs.google.com/spreadsheets/d/1mKCD-FHixwMZxUcny2UeyLAk353QCoTzcXdrIkD9W7Y/edit?usp=sharing",
                      sheet = "Enoch") %>% 
  rename(Month = 'Month(s)') %>% 
  separate(Month, into = paste0("m", 1:12), sep = ",") # Similar separate function would be used
                              # to split the tick_species into Species and Genus columns
# view(W.Enoch)

dd.month.1 <- W.Enoch%>%
  rowwise() %>% # rowwsise ensures works per row and does it independently
  mutate(
        January = sum(c_across(m1:m12) == "Jan", na.rm = TRUE),
         February = sum(c_across(m1:m12) == " Feb", na.rm = TRUE),
         March = sum(c_across(m1:m12) == " Mar", na.rm = TRUE),
         April =  sum(c_across(m1:m12) == " Apr", na.rm = TRUE),
         May =  sum(c_across(m1:m12) == " May", na.rm = TRUE),
         June =  sum(c_across(m1:m12) == " Jun", na.rm = TRUE),
         July = sum(c_across(m1:m12) == " Jul", na.rm = TRUE),
         August = sum(c_across(m1:m12) == " Aug", na.rm = TRUE),
         September =  sum(c_across(m1:m12) == " Sep", na.rm = TRUE),
         October =  sum(c_across(m1:m12) == " October", na.rm = TRUE),
         November =  sum(c_across(m1:m12) == " November", na.rm = TRUE),
         December = sum(c_across(m1:m12) == " December", na.rm = TRUE)
        ) %>%
  ungroup() # is necessary to remove the rowwise() grouping

# renaming some columns

dd.month.2 <- dd.month.1 %>% 
  select(-c(m1:m12)) %>% 
  rename(DOI_Link = "DOI/Link",
         Score = "Review Score",
         Invade ="Flagged Invasion?",
         Location = "Location Name",
         Notes=  "Notes from data collector",
         Host_group = "Animals",
         Host_SCname = "Animal_species",
         survey_period = "Year of survey")%>%
  mutate(survey_period = ifelse(is.na(survey_period), "N.S", as.character(survey_period)))
  


dd.surv <- dd.month.2 %>%
  rowwise() %>%
  mutate(
    years = list(as.integer(str_split(survey_period, ",\\s*")[[1]])),
    Min_surv_year = ifelse(all(is.na(years)), NA_integer_, min(years, na.rm = TRUE)),
    Max_surv_year = ifelse(all(is.na(years)), NA_integer_, max(years, na.rm = TRUE)),
    nYears = ifelse(all(is.na(years)), NA_integer_, n_distinct(years))
  ) %>%
  select(-years) %>%
  ungroup()

# view(dd.surv)
  

dd.geo  <- dd.surv %>%
  mutate(
    longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
    latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
  ) %>%
  mutate(
    longitude_clean = ifelse(str_detect(Longitude, "W"),
                             longitude_clean * -1, 
                             longitude_clean),
    latitude_clean = ifelse(str_detect(Latitude, "S"), 
                            latitude_clean * -1, 
                            latitude_clean)
  ) %>% 
  mutate(Pages = as.character(Pages)) %>% 
  as.data.frame()

View(dd.geo)


dd.geo %>% 
  select(Longitude, longitude_clean, Latitude, latitude_clean) %>% 
  view()


unique(dd.geo$Tick_species) %>% 
  sort() %>% 
  view()  # View tick species to see if they were entered correctly 

unique(dd.geo$Host_SCname) %>% 
  sort() %>% 
  view()   # view host species to see if they are entered correctly


write.csv(x = dd.geo, file = "C:\\Users\\DELL\\Desktop\\enoch.csv")


