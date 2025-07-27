library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(readxl)
library(googlesheets4)

gs4_auth() # might be compulsory to run this if you have not 
# authenticated your goggle account with googlesheets4

AVIHub_cleaner <- function(GoggleSheetLink, 
                           sheet_name, 
                           Save_file_path) {
  suppressMessages(suppressWarnings({
    read_sheet(ss = GoggleSheetLink,
               sheet = sheet_name) %>%
      rename(Month = 'Month(s)') %>%
      separate(Month, into = paste0("m", 1:12), sep = ",") %>%
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
      ungroup() %>%
      dplyr::select(-c(m1:m12)) %>%
      rename(
        DOI_Link = "DOI/Link",
        Score = "Review Score",
        Invade = "Flagged Invasion?",
        Location = "Location Name",
        Notes = "Notes from data collector",
        Host_group = "Animals",
        Host_SCname = "Animal_species",
        survey_period = "Year of survey"
      ) %>%
      mutate(survey_period = ifelse(is.na(survey_period), "N.S", as.character(survey_period)))%>%
      rowwise() %>%
      mutate(
        years = list(as.integer(str_split(survey_period, ",\\s*")[[1]])),
        Min_surv_year = ifelse(all(is.na(years)), NA_integer_, min(years, na.rm = TRUE)),
        Max_surv_year = ifelse(all(is.na(years)), NA_integer_, max(years, na.rm = TRUE)),
        nYears = ifelse(all(is.na(years)), NA_integer_, n_distinct(years))
      ) %>%
      dplyr:: select(-years) %>%
      ungroup() %>%
      mutate(
        longitude_clean = str_remove_all(Longitude, "[^0-9.-]") %>% as.numeric(),
        latitude_clean = str_remove_all(Latitude, "[^0-9.-]") %>% as.numeric()
      ) %>%
      mutate(
        longitude_clean = ifelse(str_detect(Longitude, "W"), -1 * longitude_clean, longitude_clean),
        latitude_clean = ifelse(str_detect(Latitude, "S"), -1 * latitude_clean, latitude_clean)
      ) %>% 
      mutate(Pages = as.character(Pages)) %>% 
      as.data.frame() %>%
      write.csv(file = Save_file_path)
  }))
}

# the function is called AVIHub_cleaner, which requires you to pass in three (input) parameters:
# 1: The GoggleSheetLink = link to the GoogleSheet 
# 2 : sheet_name = Name of the specific sheet int he GoogleSheet
# 3: Save_file_path = The file path you would prefer to use to store the data(frame) locally



#---------------------  For E.g: 


# If file is already opened locally on your PC, be sure that R would yell(!) at you
# # you need to have run the gs4_auth() function to authenticate the google sheet

AVIHub_cleaner( # our West Africa data
  GoggleSheetLink= "https://docs.google.com/spreadsheets/d/1mKCD-FHixwMZxUcny2UeyLAk353QCoTzcXdrIkD9W7Y/edit?usp=sharing",
  sheet_name = "Enoch",
  Save_file_path =  "C:\\Users\\DELL\\Desktop\\enoch.csv")

# --------------------   For E.g.:
AVIHub_cleaner( # for our east africa data
  GoggleSheetLink= "https://docs.google.com/spreadsheets/d/1Lc-LipmNMXmTscTE4tWZwkTyDIOJNUROK55D7nUSC3Q/edit?usp=sharing",
  sheet_name = "Ismail",
  Save_file_path =  "C:\\Users\\DELL\\Desktop\\practice2.csv")

# --------------------- you can still create the object to your R as usual
ismail_East.A <- read.csv("C:\\Users\\DELL\\Desktop\\practice2.csv")
# view(ismail_East.A)
