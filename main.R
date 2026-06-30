# main.R

# 1. Load Core Dependencies
library(jsonlite)
library(tidyverse)
library(lubridate)
library(config)       

# 2. Source Project Functions
source("R/soma_helpers.R")
source("R/soma_visualisations.R")

# 3. Read settings from config.yml
settings <- config::get()

# 4. Environment/Path Configurations (Dynamically generated from config)
USER_DATA_PATH      <- file.path(settings$data_dir, settings$users_file)
STUDIES_DATA_PATH   <- file.path(settings$data_dir, settings$studies_file)
RESPONSES_DATA_PATH <- file.path(settings$data_dir, settings$responses_file)


# =========================================================================
# EXECUTION
# =========================================================================
message("Loading raw JSON datasets...")
user_data      <- fromJSON(USER_DATA_PATH)
studies_data   <- fromJSON(STUDIES_DATA_PATH)
responses_data <- fromJSON(RESPONSES_DATA_PATH)

message("Executing data extraction pipeline...")
extracted_ids  <- soma_extract_ids(user_data, studies_data, responses_data)


# =========================================================================
# VISUALISATIONS (Intuition Building Examples)
# =========================================================================

# Run 1: Truncate to the official 122-day study window
# Saves files inside: figures/consented_user_data_122days/
soma_generate_intuition_plots(
  df_ema = extracted_ids$df_linked_responses,
  subfolder = "consented_user_data",
  x_inc = 25,
  y_inc = 50,
  max_days = 122
)

# Run 2: Plot all tracked data without restrictions
# Saves files inside: figures/consented_user_data_all_days/
soma_generate_intuition_plots(
  df_ema = extracted_ids$df_linked_responses,
  subfolder = "consented_user_data",
  x_inc = 25,
  y_inc = 50,
  max_days = NULL
)

# Run 3: Plot the final dual-track pool with denser labels
# (creates figures/consented_user_qualtrix_data_all_days/)
soma_generate_intuition_plots(
  df_ema = extracted_ids$df_final_dual_track,
  subfolder = "consented_user_qualtrix_data",
  x_inc = 25,
  y_inc = 25,
  max_days = NULL
)


