# install.R
# =========================================================================
# Project Environment Initialization Script
# =========================================================================

# 1. Ensure 'renv' is installed for package version pinning
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# 2. Initialize renv if it hasn't been already
if (!file.exists("renv.lock")) {
  message("Initializing a new project library environment...")
  renv::init(bare = TRUE)
}

# 3. Explicitly install required project dependencies
message("Installing required package dependencies...")
renv::install("tidyverse")
renv::install("jsonlite")
renv::install("lubridate")
renv::install("config")  # Added for path management

# 4. Save the exact versions to the lockfile
renv::snapshot(prompt = FALSE)
message("Environment successfully locked! Use renv::restore() in the future.")
