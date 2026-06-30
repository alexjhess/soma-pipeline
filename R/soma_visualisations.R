# R/soma_visualisations.R
library(tidyverse)
library(lubridate)

# Core Timeline Plotting Engine (Updated to accept a day filter cutoff)
soma_plot_timeline <- function(df, title, x_interval = 25, y_interval = 25, max_days = NULL) {
  
  unique_users <- unique(df$user)
  user_mapping <- tibble(
    user = unique_users,
    user_index = seq_along(unique_users),
    user_label = as.character(user_index)
  )
  
  df_formatted <- df %>% 
    left_join(user_mapping, by = "user") %>% 
    mutate(user_label = factor(user_label, levels = user_mapping$user_label))
  
  # Determine max day based on filter or data ceiling
  max_day <- if (!is.null(max_days)) max_days else max(df_formatted$study_day, na.rm = TRUE)
  
  x_breaks <- seq(0, max_day, by = x_interval)
  if (!(1 %in% x_breaks)) x_breaks <- c(1, x_breaks)
  
  all_labels <- user_mapping$user_label
  y_breaks <- all_labels[seq(1, length(all_labels), by = y_interval)]
  
  ggplot(df_formatted, aes(x = study_day, y = user_label)) +
    geom_hline(aes(yintercept = user_label), color = "grey95", linewidth = 0.3) +
    geom_point(aes(color = has_data, alpha = has_data, size = has_data)) +
    scale_color_manual(
      values = c("TRUE" = "#008080", "FALSE" = "grey85"),
      labels = c("TRUE" = "Data Logged", "FALSE" = "No Entry / Missing")
    ) +
    scale_alpha_manual(values = c("TRUE" = 0.9, "FALSE" = 0.25), guide = "none") +
    scale_size_manual(values = c("TRUE" = 2.2, "FALSE" = 0.6), guide = "none") +
    
    # Force X-axis limits to match the requested max_days window exactly
    scale_x_continuous(breaks = sort(unique(x_breaks)), limits = c(1, max_day)) + 
    scale_y_discrete(breaks = y_breaks) +
    
    theme_minimal() +
    labs(
      title = title, 
      x = "Day of Study", 
      y = "Participant Number Index (Showing Every 25th)", 
      color = "Status"
    ) +
    theme(
      axis.text.y = element_text(size = 8, family = "mono"),
      axis.text.x = element_text(size = 8),
      panel.grid.major.y = element_blank(), 
      panel.grid.minor = element_blank(), 
      legend.position = "bottom"
    )
}

# INTERNAL HELPER: Unpack nested $responses column and bind top-level tracking elements
soma_flatten_pool <- function(df_dual) {
  df_unpacked <- as_tibble(df_dual$responses)
  df_unpacked$user <- df_dual$user
  df_unpacked$createdAt <- df_dual$createdAt
  return(df_unpacked)
}

# Helper to find the actual case-sensitive column name
find_col_insensitive <- function(df, target) {
  actual_names <- names(df)
  matched <- actual_names[tolower(actual_names) == tolower(target)]
  if(length(matched) == 0) return(NULL)
  return(matched[1])
}

# Type 1: Standard/Numeric Columns
soma_prep_numeric_timeline <- function(df_dual, target_col) {
  df_flat <- soma_flatten_pool(df_dual)
  actual_col <- find_col_insensitive(df_flat, target_col)
  
  if (is.null(actual_col)) {
    warning(paste("Column", target_col, "not found. Generating blank timeline."))
    return(df_flat %>% mutate(date = as.Date(ymd_hms(createdAt))) %>% group_by(user) %>% mutate(study_day = as.numeric(date - min(date)) + 1) %>% ungroup() %>% group_by(user, study_day) %>% summarise(has_data = FALSE, .groups = "drop"))
  }

  df_flat %>%
    mutate(date = as.Date(ymd_hms(createdAt))) %>%
    group_by(user) %>%
    mutate(study_day = as.numeric(date - min(date)) + 1) %>%
    ungroup() %>%
    group_by(user, study_day) %>%
    summarise(has_data = any(!is.na(.data[[actual_col]]) & as.character(.data[[actual_col]]) != ""), .groups = "drop")
}

# Type 2: List Columns
soma_prep_list_item_timeline <- function(df_dual, target_col, item_string = NULL) {
  df_flat <- soma_flatten_pool(df_dual)
  actual_col <- find_col_insensitive(df_flat, target_col)
  if (is.null(actual_col)) return(soma_prep_numeric_timeline(df_dual, "NONEXISTENT"))
  
  df_processed <- df_flat %>%
    mutate(date = as.Date(ymd_hms(createdAt))) %>%
    mutate(unpacked = map(.data[[actual_col]], ~ as.character(unlist(.x)))) %>%
    unnest(unpacked, keep_empty = TRUE)
  
  if (!is.null(item_string)) {
    df_processed <- df_processed %>% mutate(match_flag = grepl(item_string, unpacked, ignore.case = TRUE))
  } else {
    df_processed <- df_processed %>% mutate(match_flag = !is.na(unpacked) & unpacked != "")
  }
  
  df_processed %>%
    group_by(user) %>%
    mutate(study_day = as.numeric(date - min(date)) + 1) %>%
    ungroup() %>%
    group_by(user, study_day) %>%
    summarise(has_data = any(match_flag, na.rm = TRUE), .groups = "drop")
}

# Type 3: Nested Dataframes (Updated to handle matrix/nested df rows safely)
soma_prep_df_timeline <- function(df_dual, target_col) {
  df_flat <- soma_flatten_pool(df_dual) 
  actual_col <- find_col_insensitive(df_flat, target_col) 
  if (is.null(actual_col)) return(soma_prep_numeric_timeline(df_dual, "NONEXISTENT"))
  
  # Extract the object safely
  target_object <- df_flat[[actual_col]]
  
  # Determine data presence based on structural type
  has_data_vector <- if (is.data.frame(target_object) || is.matrix(target_object)) {
    # If it's an expanded structure, check if rows have any non-NA / non-empty entries
    rowSums(!is.na(target_object) & target_object != "") > 0
  } else if (is.list(target_object)) {
    # If it's a true list-column of sub-dataframes
    map_lgl(target_object, ~ !is.null(.x) && (nrow(as.data.frame(.x)) > 0))
  } else {
    # Fallback standard check
    !is.na(target_object) & as.character(target_object) != ""
  }
  
  df_flat %>%
    mutate(
      date = as.Date(ymd_hms(createdAt)),
      has_sub_data = has_data_vector
    ) %>%
    group_by(user) %>%
    mutate(study_day = as.numeric(date - min(date)) + 1) %>%
    ungroup() %>%
    group_by(user, study_day) %>%
    summarise(has_data = any(has_sub_data, na.rm = TRUE), .groups = "drop")
}


#' Master Pipeline Plotting Engine with Day Filters
#' @param df_ema The subset data frame to plot (e.g., extracted_ids$df_linked_responses)
#' @param subfolder String prefix name for the export folder inside 'figures/'
#' @param x_inc Integer. Sparsity step size for the study days axis
#' @param y_inc Integer. Sparsity step size for the participant index axis
#' @param max_days Integer. Number of days to truncate the timeline to (e.g., 122). NULL plots all available days.
soma_generate_intuition_plots <- function(df_ema, subfolder = "consented_user_data", x_inc = 25, y_inc = 25, max_days = NULL) {
  
  message("Starting data intuition visualisations...")
  
  # 1. Dynamically update folder name to include time limit constraints
  folder_suffix <- if (!is.null(max_days)) paste0("_", max_days, "days") else "_all_days"
  final_subfolder <- paste0(subfolder, folder_suffix)
  
  target_dir <- file.path("figures", final_subfolder)
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # Helper helper inline loop closure to inject day constraints safely
  apply_day_filter <- function(df_plot) {
    if (!is.null(max_days)) {
      df_plot <- df_plot %>% filter(study_day <= max_days)
    }
    return(df_plot)
  }
  
  # -------------------------------------------------------------------------
  # Batch 1: Standard Numeric, Character, and Logical Variables
  # -------------------------------------------------------------------------
  numeric_vars <- c("mood", "painintensity", "painunpleasantness", "paininterference", "predmood",
                    "predpainintensity", "predpainunpleasantness", "predpaininterference", 
                    "sleepquality", "tired", "stressed", "betterpred", "moodpost",
                    "painintensitypost", "enjoy", "betterpost",
                    "painchange", "predpainchange", "painlocationconfirm")
  
  for (var in numeric_vars) {
    message(paste(" -> Plotting variable:", var))
    df_plot <- soma_prep_numeric_timeline(df_ema, var) %>% apply_day_filter()
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density:", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days)
    ggsave(filename = file.path(target_dir, paste0("timeline_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Batch 2: Nested Data Frame structures
  # -------------------------------------------------------------------------
  df_vars <- c("painlocation", "moodactivity", "painactivity", "painhasmedicationtreatment")
  
  for (var in df_vars) {
    message(paste(" -> Plotting dataframe object:", var))
    df_plot <- soma_prep_df_timeline(df_ema, var) %>% apply_day_filter()
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density:", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days)
    ggsave(filename = file.path(target_dir, paste0("timeline_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Batch 3: List Arrays (General presence check)
  # -------------------------------------------------------------------------
  list_vars <- c("emotion", "activity", "painmedication", "predactivity", "paintreatment")
  
  for (var in list_vars) {
    message(paste(" -> Plotting array list:", var))
    df_plot <- soma_prep_list_item_timeline(df_ema, var) %>% apply_day_filter()
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density (Any Entry):", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days)
    ggsave(filename = file.path(target_dir, paste0("timeline_list_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Specific Sub-item Array Check
  # -------------------------------------------------------------------------
  message(" -> Plotting specific array selection: paintreatment -> 'Other'")
  df_other <- soma_prep_list_item_timeline(df_ema, "paintreatment", "Other") %>% apply_day_filter()
  p_other  <- soma_plot_timeline(df_other, "EMA Timeline Density: Selection of 'Other' Pain Treatments", x_interval = x_inc, y_interval = y_inc, max_days = max_days)
  ggsave(filename = file.path(target_dir, "timeline_paintreatment_item_Other.png"), plot = p_other, width = 10, height = 7, dpi = 300)
  
  message(paste("All visualisations exported to 'figures/", final_subfolder, "/' folder successfully!\n"))
}