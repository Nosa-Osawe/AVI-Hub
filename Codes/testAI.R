library(tidyverse)
library(readxl)


ai = read_excel("Data/test_ai_avihub.xlsx") %>% 
  rename(Error = "Error",
         YearoP= "Year of Pub",
         YearSurv = "Year of survey",
         Months = "Month(s)",
         Doi_link =  "DOI/Link",
         Location = "Location Name",
         Invasion = "Flagged Invasion?")

errorRate = ai %>% 
  group_by(Error) %>% 
  summarise(YearoP = 100 * sum(YearoP)/length(YearoP),
            YearSurv = 100 * sum(YearSurv)/length(YearSurv),
            Months = 100 * sum(Months)/length(Months),
            Doi_link = 100 * sum(Doi_link)/length(Doi_link),
            Location = 100 * sum(Location)/length(Location),
            Longitude = 100 * sum(Longitude)/length(Longitude),
            Latitude = 100 * sum(Latitude)/length(Latitude),
            Animals = 100 * sum(Animals)/length(Animals),
            Animal_species= 100 * sum(Animal_species)/length(Animal_species),
            Tick_species= 100 * sum(Tick_species)/length(Tick_species),
            Invasion = 100 * sum(Invasion, na.rm = TRUE)/ length(na.omit(Invasion))
            )


errorRate %>% 
  pivot_longer(col = -c("Error"),
               names_to = "Variables",
               values_to = "Error_rate") %>% 
ggplot( aes(x = Variables, y = Error_rate + 0.03, fill = Error)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  labs(
    title = "False Positive vs False Negative Error Rates",
    x = "Variables",
    y = "% Error Rate"
  ) +
  scale_fill_manual(values = c("False Positive" = "#F8766D", 
                               "False Negative" = "#00BFC4")) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5))



