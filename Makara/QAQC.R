library(readr) # for read_csv
library(dplyr)
library(stringr)
library(purrr)

#Check Devices----
# 1. Load the list of valid device codes
valid_devices <- read_csv("Makara/Sheets/devices.csv") %>%
  pull(device_code) %>%
  str_trim() %>%
  unique()

# 2. Extract all individual device codes from deployment_device_codes column
all_used_devices <- deployments %>%
  pull(deployment_device_codes) %>%
  str_split(",") %>% # Split comma-separated devices
  unlist() %>%
  str_trim() %>%
  unique()

# Optional: normalize case (if necessary)
# valid_devices <- str_to_lower(valid_devices)
# all_used_devices <- str_to_lower(all_used_devices)

# 3. Compare with valid devices
invalid_devices <- setdiff(all_used_devices, valid_devices)

# 4. Output results
if (length(invalid_devices) == 0) {
  message("✅ All device codes in deployments are valid.")
} else {
  message(
    "❌ The following device codes are NOT in the list of valid device codes:"
  )
  print(invalid_devices)
}

#Check deployment_codes----
csv_files <- list.files(
  path = "Makara/Sheets/",
  pattern = "\\.csv$",
  full.names = TRUE
)

# Initialize list to store valid deployment_code data
deployment_lists <- list()
valid_files <- c()

# Loop through each CSV and keep only those with deployment_code
for (file in csv_files) {
  df <- read_csv(file, show_col_types = FALSE)

  if ("deployment_code" %in% colnames(df)) {
    deployment_lists[[file]] <- df %>%
      select(deployment_code) %>%
      distinct() %>%
      arrange(deployment_code)
    valid_files <- c(valid_files, file)
  } else {
    message("⚠️ Skipping file (no 'deployment_code'): ", basename(file))
  }
}

# Ensure we have at least one valid file
if (length(deployment_lists) < 2) {
  stop("Not enough valid files with 'deployment_code' to compare.")
}

# Use the first file as reference
reference_codes <- deployment_lists[[1]]
names(reference_codes) <- "deployment_code"

# Compare all other files to the reference
for (i in seq_along(deployment_lists)) {
  current_file <- names(deployment_lists)[i]
  current_codes <- deployment_lists[[i]]

  if (!identical(reference_codes, current_codes)) {
    message("❌ Mismatch in: ", basename(current_file))

    # Show differences
    diff <- anti_join(
      reference_codes,
      current_codes,
      by = "deployment_code"
    ) %>%
      bind_rows(anti_join(
        current_codes,
        reference_codes,
        by = "deployment_code"
      ))
    print(diff)
  } else {
    message("✅ Match in: ", basename(current_file))
  }
}


#checking GPS files----
library(readr)
library(dplyr)
library(purrr)
library(fs)

# Define the path to your GPS CSV files
gps_dir <- "C:/Users/kourtney.burger/Documents/GitHub/ADRIFT-PACM/R/GPS"

# Get a list of all .csv files in the directory
gps_files <- dir_ls(path = gps_dir, regexp = "\\.csv$", recurse = FALSE)

# Function to check for NA in DriftName column
check_na_in_driftname <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)

  if ("DriftName" %in% names(df)) {
    if (any(is.na(df$DriftName))) {
      return(basename(file_path)) # Return file name with NA
    }
  } else {
    return(paste("Missing 'DriftName' column:", basename(file_path)))
  }

  return(NA_character_) # return NA if no issues
}

# Run the function on all files
check_results <- map_chr(gps_files, check_na_in_driftname)

# Filter out NAs (i.e., files with no issues)
files_with_issues <- check_results[!is.na(check_results)]

# Output result
if (length(files_with_issues) > 0) {
  cat("⚠️ Files with NA in 'DriftName' column or missing column:\n")
  print(files_with_issues)
} else {
  cat("✅ All files have a complete 'DriftName' column.\n")
}
