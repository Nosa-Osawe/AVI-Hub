setwd("/Users/aia/Desktop/Tick")  # Replace with your folder path

# List all CSV files in the folder
file_list <- list.files(pattern = "\\.csv$")

# Read and merge all CSVs into one data frame
merged_data <- do.call(rbind, lapply(file_list, read.csv, stringsAsFactors = FALSE))

# View the first few rows
head(merged_data)
view(merged_data)

write.csv(x = merged_data, file = "/Users/aia/Desktop/Tick/merged.westafrica.data.14.05.2025.csv")