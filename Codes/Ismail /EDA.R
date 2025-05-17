# Introduction----
# 14-05-2025 
# Ismail Ayomide Ayegboyin (ayegboyinismai@gmail.com)
#This work is new and it is still in progress .....
# This is an exploratory data analysis of 60-year brief history of ticks in west africa

# Libraries----
library(tidyverse)

  
# Load Data----
tick <- readxl::read_xlsx("/Users/aia/Desktop/Tick/merged.westafrica.data.14.05.2025.xlsx")
attach(tick)
view(tick)

  
# Exploratory Data Analysis Starts here ----


# Number of papers that has been accessed and data has been retreived from 
unique(DOI_Link) # this will generate the number of papers that has been touched without replicating the numbers at all

# DOI_Link is 161 papers already 


# Publication trends by country
  pub_trends <- tick %>%
  filter(!is.na(Country),  Country != "NA") %>%  # This line removes rows with NA in Country
  group_by(Country, Year.of.Pub) %>%
  summarise(Studies = n_distinct(Min_surv_year), .groups = "keep")
view(pub_trends)

ggplot(pub_trends, aes(x = Year.of.Pub, y = Studies, color = Country)) +
  geom_point(size = 2) +
  labs(title = "Tick Research Publications Over Time by Country",
       x = "Publication Year",
       y = "Number of Studies") +
  scale_x_continuous(breaks = seq(min(pub_trends$Year.of.Pub), max(pub_trends$Year.of.Pub), by = 1)) + 
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))


# Distribution of Tick Invasion Status 
str(Invade) # Checking for the data structure first ....

tick %>%
  filter(!is.na(Invade) & Invade != "NA") %>%  # Remove NA values
  mutate(Invade = factor(Invade, 
                         levels = c("No", "Unclear", "Yes"),
                         labels = c("No", "Unclear", "Yes"),  # Explicit labels
                         ordered = TRUE)) %>%
  ggplot(aes(x = Invade, fill = Invade)) +
  geom_bar(width = 0.5) +
  scale_fill_manual(values = c("No" = "green", "Unclear" = "orange", "Yes" = "red")) +
  labs(title = "Distribution of Tick Invasion Status",
       x = "Invasion Status",
       y = "Count") +
  theme_light()


  # Animals with flagged invasive for ticks
  # Filter only "Yes" invasions and count by host species
  invasion_yes <- tick %>%
  filter(Invade == "Yes") %>%  # Keep only "Yes" invasions
  count(Host_group, name = "Count") %>%  # Count by host species
  arrange(desc(Count))  # Sort by highest count

# Plot top 10 host species with "Yes" invasions
ggplot(invasion_yes %>% slice_max(Count, n = 10), 
       aes(x = reorder(Host_group, Count), y = Count, fill = Host_group)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Horizontal bars for better readability
  labs(
    title = "Host Species with Tick Species Flagged Invasion",
    x = "Host Species",
    y = "Number of Invasions"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend (redundant here)


# Most Common Tick Species ...
# Correct and standardize tick species names
tick_species_clean <- tick %>%
  filter(!is.na(Tick_species), Tick_species != "NA") %>%
  mutate(
    Tick_species = case_when(
      # Correct misspellings
      Tick_species == "Ambylomma variegatum" ~ "Amblyomma variegatum",
      Tick_species == "Rhicephalus sanguineus" ~ "Rhipicephalus sanguineus",
      Tick_species == "Hyalomma nitidium" ~ "Hyalomma nitidum",
      
      # Update Boophilus to Rhipicephalus (current taxonomy)
      Tick_species == "Boophilus" ~ "Rhipicephalus",
      Tick_species == "Boophilus annulatus" ~ "Rhipicephalus annulatus",
      Tick_species == "Boophilus decoloratus" ~ "Rhipicephalus decoloratus",
      Tick_species == "Boophilus geigyi" ~ "Rhipicephalus geigyi",
      
      # Standardize Hyalomma marginatum rufipes
      Tick_species %in% c("Hyalomma marginatum rufipes", "Hyalomma rufipes") ~ "Hyalomma marginatum rufipes",
      
      # Keep others as-is if already correct
      TRUE ~ Tick_species
    )
  ) %>%
  
  count(Tick_species, sort = TRUE) %>%
  filter(n > 10)
view(tick_species_clean)
# Verify cleaned names
view(unique(tick_species_clean$Tick_species))

# Plot
ggplot(tick_species_clean, aes(x = reorder(Tick_species, -n), y = n)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  labs(title = "Most Frequently Reported Tick Species",
       x = "Tick Species",
       y = "Number of Records") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Countries reporting Rhipicephalus microplus (using cleaned data)
tick_species_clean_2 <- tick %>%
  filter(!is.na(Tick_species), Tick_species != "NA") %>%
  mutate(
    Tick_species = case_when(
      # Correct misspellings
      Tick_species == "Ambylomma variegatum" ~ "Amblyomma variegatum",
      Tick_species == "Rhicephalus sanguineus" ~ "Rhipicephalus sanguineus",
      Tick_species == "Hyalomma nitidium" ~ "Hyalomma nitidum",
      
      # Update Boophilus to Rhipicephalus (current taxonomy)
      Tick_species == "Boophilus" ~ "Rhipicephalus",
      Tick_species == "Boophilus annulatus" ~ "Rhipicephalus annulatus",
      Tick_species == "Boophilus decoloratus" ~ "Rhipicephalus decoloratus",
      Tick_species == "Boophilus geigyi" ~ "Rhipicephalus geigyi",
      
      # Standardize Hyalomma marginatum rufipes
      Tick_species %in% c("Hyalomma marginatum rufipes", "Hyalomma rufipes") ~ "Hyalomma marginatum rufipes",
      
      # Keep others as-is if already correct
      TRUE ~ Tick_species
    )
  )
  microplus_by_country <- tick_species_clean_2 %>% 
  filter(Tick_species == "Rhipicephalus microplus") %>% 
  filter(!is.na(Country)) %>%  # Corrected from ils.na to !is.na
  count(Country, sort = TRUE)

# View the results 
view(microplus_by_country)

ggplot(microplus_by_country, aes(x = reorder(Country, -n), y = n)) + 
  geom_bar(stat = "identity", fill = "blue", width = 0.3) + 
  labs(
    title = "Countries Reporting Rhipicephalus microplus",
    x = "Country",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Countries reporting Amblyomma variegatum (using corrected spelling)
amblyomma_by_country <- tick_species_clean_2 %>% 
  filter(Tick_species == "Amblyomma variegatum") %>% 
  filter(!is.na(Country)) %>%  # Corrected from ils.na to !is.na
  count(Country, sort = TRUE)

# View the results 
view(amblyomma_by_country)  # Also fixed typo in this line
ggplot(amblyomma_by_country, aes(x = reorder(Country, -n), y = n)) + 
  geom_bar(stat = "identity", fill = "#fbc95c", width = 0.5) + 
  labs(
    title = "Countries Reporting Amblyomma variegatum",
    x = "Country",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


  
# Checking the study duration
study_duration <- tick %>%
  mutate(Study_Length = ifelse(nYears > 1, "Multi-Year", "Single-Year")) %>%
  count(Country, Study_Length)

ggplot(study_duration, aes(x = Country, y = n, fill = Study_Length)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Study Duration by Country",
       x = "Country",
       y = "Number of Studies",
       fill = "Study Duration") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 16th May Presentation Ends here ----


   