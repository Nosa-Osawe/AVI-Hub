setwd("/Users/user/Desktop/Data Science Library/Data for play/AVI-Hub/Codes/Ismail /Raw Data")  # This is the folder path to the raw data

# List all CSV files in the folder
file_list <- list.files(pattern = "\\.csv$")

# Read and merge all CSVs into one data frame
merged_data <- do.call(rbind, lapply(file_list, read.csv, stringsAsFactors = FALSE))

# View the first few rows
head(merged_data)
view(merged_data)

write.csv(x = merged_data, file = "/Users/user/Desktop/Data Science Library/Data for play/AVI-Hub/Codes/Ismail /Raw Data/merged.westafrica.data.28.05.2025.csv")

