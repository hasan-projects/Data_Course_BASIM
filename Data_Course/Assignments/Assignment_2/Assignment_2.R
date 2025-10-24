# List all .csv files in Data/ directory
csv_files <- list.files("Data/", pattern = "\\.csv$", full.names = TRUE)

# Find how many files match
length(csv_files)

# Open wingspan_vs_mass.csv and store as df
df <- read.csv("Data/wingspan_vs_mass.csv")

# Inspect the first 5 lines
head(df, 5)

# Find any files recursively in Data/ that begin with "b" 
b_files <- list.files("Data/", pattern = "^b", recursive = TRUE, full.names = TRUE)

# Display the first line of each "b" file
for (f in b_files) {
  cat("File:", f, "\n")
  cat(readLines(f, n = 1), "\n\n")
}

# Do the same for all files that end in .csv
all_csv <- list.files("Data/", pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
for (f in all_csv) {
  cat("File:", f, "\n")
  cat(readLines(f, n = 1), "\n\n")
}
