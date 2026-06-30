# main.R

# 1. Load Core Dependencies
library(jsonlite)
library(tidyverse)
library(lubridate)
library(config)       # <--- Make sure this is loaded

# 2. Source Project Functions
source("R/soma_helpers.R")

# 3. Read settings from config.yml
# This loads the "default" block as a named list
settings <- config::get()

# 4. Environment/Path Configurations (Dynamically generated from config)
USER_DATA_PATH      <- file.path(settings$data_dir, settings$users_file)
STUDIES_DATA_PATH   <- file.path(settings$data_dir, settings$studies_file)
RESPONSES_DATA_PATH <- file.path(settings$data_dir, settings$responses_file)


# =========================================================================
# EXECUTION (Unchanged)
# =========================================================================
message("Loading raw JSON datasets...")
user_data      <- fromJSON(USER_DATA_PATH)
studies_data   <- fromJSON(STUDIES_DATA_PATH)
responses_data <- fromJSON(RESPONSES_DATA_PATH)

message("Executing data extraction pipeline...")
extracted_ids  <- soma_extract_ids(user_data, studies_data, responses_data)