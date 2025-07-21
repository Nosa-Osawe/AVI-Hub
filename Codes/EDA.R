
library(tidyverse)
enoch <- read.csv("C:\\Users\\DELL\\Desktop\\enoch.csv")
enoch %>% 
  view()

Cameroon <- enoch %>% 
  filter(Country == "Cameroon")

unique(Cameroon$Index)

Cameroon %>% 
  view()

Cameroon %>% 
  group_by(Index) %>% 
  summarise(number_of_tick_Reports = n(),
            n_of_years = mean(nYears)) %>% 
  as.data.frame() %>% 
  summarise( mean_surv_years = mean(n_of_years, na.rm = TRUE),
             mean_tick_Report = mean(number_of_tick_Reports, na.rm = TRUE),
             SD = sd(number_of_tick_Reports, na.rm = TRUE))

Cameroon %>% 
  group_by(Index,
           Location) %>% 
  summarise(Jan = ifelse((sum(January, na.rm = TRUE) >0), 1, 0),
            Feb = ifelse((sum(February, na.rm = TRUE)>0), 1, 0),
            Mar = ifelse((sum(March, na.rm = TRUE)>0),1, 0),
            Apr = ifelse((sum(April, na.rm = TRUE)>0),1,0),
            May = ifelse((sum(May, na.rm = TRUE)>0), 1, 0),
            Jun = ifelse((sum(June, na.rm = TRUE)>0), 1,0) )%>% 
  view()

  