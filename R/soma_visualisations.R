# R/soma_visualisations.R
library(tidyverse)
library(lubridate)
library(ggupset)

# Core Timeline Plotting Engine (Updated to accept adjustable dot sizes)
soma_plot_timeline <- function(df, title, x_interval = 25, y_interval = 25, max_days = NULL, dot_size = 1.2) {
  
  unique_users <- unique(df$user)
  user_mapping <- tibble(
    user = unique_users,
    user_index = seq_along(unique_users),
    user_label = as.character(user_index)
  )
  
  df_formatted <- df %>% 
    left_join(user_mapping, by = "user") %>% 
    mutate(user_label = factor(user_label, levels = user_mapping$user_label))
  
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
    
    # FIX: Explicitly bind the active data dot-size to your custom argument
    scale_size_manual(values = c("TRUE" = dot_size, "FALSE" = dot_size * 0.3), guide = "none") +
    
    scale_x_continuous(breaks = sort(unique(x_breaks)), limits = c(1, max_day)) + 
    scale_y_discrete(breaks = y_breaks) +
    theme_minimal() +
    labs(
      title = title, 
      x = "Day of Study", 
      y = "Participant Number Index", 
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
#' @param dot_size Float. Specifies the dot size for plotting.
soma_generate_intuition_plots <- function(df_ema, subfolder = "consented_user_data", x_inc = 25, y_inc = 25, max_days = NULL, dot_size = 1.2) {  
  
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
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density:", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days, dot_size = dot_size)
    ggsave(filename = file.path(target_dir, paste0("timeline_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Batch 2: Nested Data Frame structures
  # -------------------------------------------------------------------------
  df_vars <- c("painlocation", "moodactivity", "painactivity", "painhasmedicationtreatment")
  
  for (var in df_vars) {
    message(paste(" -> Plotting dataframe object:", var))
    df_plot <- soma_prep_df_timeline(df_ema, var) %>% apply_day_filter()
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density:", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days, dot_size = dot_size)
    ggsave(filename = file.path(target_dir, paste0("timeline_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Batch 3: List Arrays (General presence check)
  # -------------------------------------------------------------------------
  list_vars <- c("emotion", "activity", "painmedication", "predactivity", "paintreatment")
  
  for (var in list_vars) {
    message(paste(" -> Plotting array list:", var))
    df_plot <- soma_prep_list_item_timeline(df_ema, var) %>% apply_day_filter()
    p <- soma_plot_timeline(df_plot, paste("EMA Timeline Density (Any Entry):", var), x_interval = x_inc, y_interval = y_inc, max_days = max_days, dot_size = dot_size)
    ggsave(filename = file.path(target_dir, paste0("timeline_list_", var, ".png")), plot = p, width = 10, height = 7, dpi = 300)
  }
  
  # -------------------------------------------------------------------------
  # Specific Sub-item Array Check
  # -------------------------------------------------------------------------
  message(" -> Plotting specific array selection: paintreatment -> 'Other'")
  df_other <- soma_prep_list_item_timeline(df_ema, "paintreatment", "Other") %>% apply_day_filter()
  p_other  <- soma_plot_timeline(df_other, "EMA Timeline Density: Selection of 'Other' Pain Treatments", x_interval = x_inc, y_interval = y_inc, max_days = max_days, dot_size = dot_size)
  ggsave(filename = file.path(target_dir, "timeline_paintreatment_item_Other.png"), plot = p_other, width = 10, height = 7, dpi = 300)
  
  message(paste("All visualisations exported to 'figures/", final_subfolder, "/' folder successfully!\n"))
}


#' Generate Individual Timelines for Every Unique Item in a List Column
#' @param df_ema The subset data frame to plot (e.g., extracted_ids$df_linked_responses)
#' @param target_col String name of the list column (e.g., "painmedication" or "paintreatment")
#' @param subfolder String prefix name for the export folder inside 'figures/'
#' @param x_inc Integer. Sparsity step size for the study days axis
#' @param y_inc Integer. Sparsity step size for the participant index axis
#' @param max_days Integer. Number of days to truncate the timeline to. NULL plots all.
#' @param dot_size Float. Specifies the dot size for plotting.
soma_generate_list_item_plots <- function(df_ema, target_col, subfolder = "consented_user_data", x_inc = 25, y_inc = 50, max_days = NULL, dot_size = 1.2) {

  # 1. Flatten the pool and find the case-sensitive column
  df_flat <- soma_flatten_pool(df_ema)
  actual_col <- find_col_insensitive(df_flat, target_col)
  
  if (is.null(actual_col)) {
    stop(paste("Target list column", target_col, "not found in the dataset."))
  }
  
  # 2. Extract every single unique item/category within this list column
  message(paste("Analyzing unique categories inside list column:", actual_col))
  unique_items <- df_flat %>%
    mutate(unpacked = map(.data[[actual_col]], ~ as.character(unlist(.x)))) %>%
    unnest(unpacked, keep_empty = FALSE) %>%
    filter(!is.na(unpacked) & unpacked != "") %>%
    distinct(unpacked) %>%
    pull(unpacked)
  
  if (length(unique_items) == 0) {
    message(paste("No items found inside list column:", actual_col))
    return(NULL)
  }
  
  message(paste("Found", length(unique_items), "unique categories:", paste(unique_items, collapse = ", ")))
  
  # 3. Establish a separate directory for these categorical sub-items
  folder_suffix <- if (!is.null(max_days)) paste0("_", max_days, "days") else "_all_days"
  target_dir <- file.path("figures", paste0(subfolder, folder_suffix), paste0("categories_", tolower(actual_col)))
  
  if (!dir.exists(target_dir)) {
    dir.create(target_dir, recursive = TRUE)
  }
  
  # Helper to inject day constraints safely
  apply_day_filter <- function(df_plot) {
    if (!is.null(max_days)) df_plot <- df_plot %>% filter(study_day <= max_days)
    return(df_plot)
  }
  
  # 4. Loop through each unique category item and generate its timeline asset
  for (item in unique_items) {
    message(paste(" -> Generating categorical timeline for:", actual_col, "->", item))
    
    # Process item map
    df_plot <- soma_prep_list_item_timeline(df_ema, actual_col, item_string = item) %>% 
      apply_day_filter()
    
    p <- soma_plot_timeline(
      df = df_plot, 
      title = paste0("EMA Timeline Density: ", actual_col, " (Selection: ", item, ")"), 
      x_interval = x_inc, 
      y_interval = y_inc, 
      max_days = max_days,
      dot_size = dot_size
    )
    
    # Sanitize file name (replace spaces/special chars with underscores to prevent OS issues)
    safe_item_name <- gsub("[^[:alnum:]]", "_", item)
    
    ggsave(
      filename = file.path(target_dir, paste0("item_", safe_item_name, ".png")), 
      plot = p, 
      width = 10, 
      height = 7, 
      dpi = 300
    )
  }
  
  message(paste("All unique category timelines for", actual_col, "saved to:", target_dir, "\n"))
}


#' Generate a Clustered Patient-by-Treatment Heatmap
#' @param df_ema The subset data frame to plot (e.g., extracted_ids$df_linked_responses)
#' @param subfolder String prefix name for the export folder inside 'figures/'
#' @param max_days Integer. Optional day truncation window.
soma_plot_treatment_clusters <- function(df_ema, subfolder = "consented_user_data", max_days = NULL) {
  
  message("Extracting treatment profiles for clustering...")
  df_flat <- soma_flatten_pool(df_ema)
  
  # 1. Clean and filter by study day if requested
  if (!is.null(max_days)) {
    df_flat <- df_flat %>%
      mutate(date = as.Date(ymd_hms(createdAt))) %>%
      group_by(user) %>%
      mutate(study_day = as.numeric(date - min(date)) + 1) %>%
      ungroup() %>%
      filter(study_day <= max_days)
  }
  
  # 2. Extract and unnest both medication and treatment arrays safely
  med_col <- find_col_insensitive(df_flat, "painmedication")
  treat_col <- find_col_insensitive(df_flat, "paintreatment")
  
  df_meds <- df_flat %>%
    select(user, all_of(med_col)) %>%
    mutate(item = map(.data[[med_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  df_treats <- df_flat %>%
    select(user, all_of(treat_col)) %>%
    mutate(item = map(.data[[treat_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  # Combined long table of any tracking engagement
  df_combined_long <- bind_rows(df_meds, df_treats) %>%
    filter(!is.na(item) & item != "") %>%
    distinct(user, item) # Binary: did they ever use it?
  
  if(nrow(df_combined_long) == 0) {
    stop("No treatment data found to cluster.")
  }
  
  # 3. Pivot into a Wide Binary Matrix (Users x Treatments)
  matrix_data <- df_combined_long %>%
    mutate(present = 1) %>%
    pivot_wider(names_from = item, values_from = present, values_fill = 0) %>%
    column_to_rownames("user") %>%
    as.matrix()
  
  # 4. Save Plot File
  folder_suffix <- if (!is.null(max_days)) paste0("_", max_days, "days") else "_all_days"
  target_dir <- file.path("figures", paste0(subfolder, folder_suffix))
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  
  png(filename = file.path(target_dir, "treatment_co_occurrence_clusters.png"), 
      width = 3000, height = 2400, res = 300)
  
  # Render using R's built-in hierarchical clustering visualizer
  heatmap(
    matrix_data,
    distfun = function(x) dist(x, method = "binary"), # Optimized for 0/1 data
    hclustfun = function(x) hclust(x, method = "ward.D2"),
    col = c("#E5E5E5", "#008080"), # Light Grey = No, Teal = Yes
    scale = "none",
    margins = c(12, 5),
    main = "Patient Treatment Clusters (Co-Occurrence Over Study Window)"
  )
  
  dev.off()
  message(paste("Cluster heatmap saved successfully to:", target_dir, "\n"))
}


#' Generate a Native UpSet Combination Plot for Medications and Treatments
#' @param df_ema The subset data frame to plot (e.g., extracted_ids$df_linked_responses)
#' @param subfolder String prefix name for the export folder inside 'figures/'
#' @param max_days Integer. Optional day truncation window.
soma_plot_treatment_upset <- function(df_ema, subfolder = "consented_user_data", max_days = NULL) {
  
  message("Extracting treatment combinations for ggupset chart...")
  df_flat <- soma_flatten_pool(df_ema)
  
  # 1. Clean and filter by study day if requested
  if (!is.null(max_days)) {
    df_flat <- df_flat %>%
      mutate(date = as.Date(ymd_hms(createdAt))) %>%
      group_by(user) %>%
      mutate(study_day = as.numeric(date - min(date)) + 1) %>%
      ungroup() %>%
      filter(study_day <= max_days)
  }
  
  # 2. Extract, unnest, and aggregate unique lists *per user*
  med_col <- find_col_insensitive(df_flat, "painmedication")
  treat_col <- find_col_insensitive(df_flat, "paintreatment")
  
  df_meds <- df_flat %>%
    select(user, all_of(med_col)) %>%
    mutate(item = map(.data[[med_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  df_treats <- df_flat %>%
    select(user, all_of(treat_col)) %>%
    mutate(item = map(.data[[treat_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  # Group combinations into a neat list column format for ggupset
  df_combinations <- bind_rows(df_meds, df_treats) %>%
    filter(!is.na(item) & item != "") %>%
    distinct(user, item) %>%
    group_by(user) %>%
    summarise(all_treatments = list(sort(unique(item))), .groups = "drop")
  
  if(nrow(df_combinations) == 0) {
    stop("No treatment combinations found to plot.")
  }
  
  # 3. Build standard ggplot2 layout using the ggupset scale transformer
  p_upset <- ggplot(df_combinations, aes(x = all_treatments)) +
    geom_bar(fill = "#008080", alpha = 0.9, width = 0.6) +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5, size = 3, fontface = "bold") +
    scale_x_upset(
      n_intersections = 20, # Show top 20 most popular combinations
      name = "Therapeutic Treatment Combinations"
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    theme_minimal() +
    labs(
      title = "SOMA Cohort UpSet Plot: Dominant Treatment Combinations",
      subtitle = "Vertical bars show combination popularity; dot matrix shows combination components.",
      y = "Number of Active Users"
    ) +
    theme(
      axis.title.x = element_text(face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # 4. Export high-res graphic asset safely
  folder_suffix <- if (!is.null(max_days)) paste0("_", max_days, "days") else "_all_days"
  target_dir <- file.path("figures", paste0(subfolder, folder_suffix))
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(target_dir, "treatment_upset_combinations.png"),
    plot = p_upset,
    width = 11,
    height = 7,
    dpi = 300
  )
  
  message(paste("UpSet visualization chart saved successfully to:", target_dir, "\n"))
}


#' Generate a State-Transition Timeline Grid for Treatment Combinations
#' @param df_ema The subset data frame to plot (e.g., extracted_ids$df_linked_responses)
#' @param subfolder String prefix name for the export folder inside 'figures/'
#' @param x_inc Integer. Sparsity step size for the study days axis (X)
#' @param y_inc Integer. Sparsity step size for the participant index axis (Y)
#' @param max_days Integer. Optional day truncation window (e.g., 122).
soma_plot_state_timeline <- function(df_ema, subfolder = "consented_user_data", x_inc = 25, y_inc = 50, max_days = NULL) {
  
  message("Structuring daily state combinations for timeline mapping...")
  df_flat <- soma_flatten_pool(df_ema)
  
  # 1. Add standard study days mapping
  df_days <- df_flat %>%
    mutate(date = as.Date(ymd_hms(createdAt))) %>%
    group_by(user) %>%
    mutate(study_day = as.numeric(date - min(date)) + 1) %>%
    ungroup()
  
  if (!is.null(max_days)) {
    df_days <- df_days %>% filter(study_day <= max_days)
  }
  
  # 2. Extract and unnest categories safely
  med_col <- find_col_insensitive(df_days, "painmedication")
  treat_col <- find_col_insensitive(df_days, "paintreatment")
  
  df_meds <- df_days %>%
    select(user, study_day, all_of(med_col)) %>%
    mutate(item = map(.data[[med_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  df_treats <- df_days %>%
    select(user, study_day, all_of(treat_col)) %>%
    mutate(item = map(.data[[treat_col]], ~ as.character(unlist(.x)))) %>%
    unnest(item, keep_empty = FALSE)
  
  # 3. Collapse multiple daily entries into a single sorted string combination per user-day
  df_daily_states <- bind_rows(df_meds, df_treats) %>%
    filter(!is.na(item) & item != "") %>%
    distinct(user, study_day, item) %>%
    group_by(user, study_day) %>%
    summarise(
      state = paste(sort(unique(item)), collapse = " + "),
      .groups = "drop"
    )
  
  # 4. Fill in missing tracking days / "No Entry" days to keep the timeline balanced
  all_users <- unique(df_days$user)
  actual_max_day <- if (!is.null(max_days)) max_days else max(df_days$study_day, na.rm = TRUE)
  
  grid_skeleton <- expanding_grid_skeleton <- expand.grid(
    user = all_users,
    study_day = 1:actual_max_day,
    stringsAsFactors = FALSE
  ) %>% as_tibble()
  
  # Identify the top 10 most common treatment combinations
  top_states <- df_daily_states %>%
    count(state, sort = TRUE) %>%
    slice_max(n, n = 10) %>%
    pull(state)
  
  # Merge states onto grid and categorize rare entries
  df_timeline_ready <- grid_skeleton %>%
    left_join(df_daily_states, by = c("user", "study_day")) %>%
    mutate(
      treatment_regime = case_when(
        is.na(state) ~ "No Active Entry",
        state %in% top_states ~ state,
        TRUE ~ "Other Combination"
      )
    )
  
  # 5. Apply your signature participant indexing (1, 2, 3...) and sparse axis mapping
  user_mapping <- tibble(
    user = all_users,
    user_index = seq_along(all_users),
    user_label = as.character(user_index)
  )
  
  df_plot <- df_timeline_ready %>%
    left_join(user_mapping, by = "user") %>%
    mutate(user_label = factor(user_label, levels = user_mapping$user_label))
  
  x_breaks <- seq(0, actual_max_day, by = x_inc)
  if (!(1 %in% x_breaks)) x_breaks <- c(1, x_breaks)
  
  all_labels <- user_mapping$user_label
  y_breaks <- all_labels[seq(1, length(all_labels), by = y_inc)]
  
  # 6. Generate categorical color palette (Teal-based, with clean neutrals for gaps)
  # Colorblind-friendly qualitative palette base
  regime_colors <- c(
    "No Active Entry" = "grey93",
    "Other Combination" = "grey50"
  )
  
  distinct_colors <- c(
    "#008080", "#2ca02c", "#ff7f0e", "#d62728", "#9467bd", 
    "#8c564b", "#e377c2", "#bcbd22", "#17becf", "#1f77b4"
  )
  
  other_states <- setdiff(unique(df_plot$treatment_regime), c("No Active Entry", "Other Combination"))
  for (i in seq_along(other_states)) {
    regime_colors[other_states[i]] <- distinct_colors[(i - 1) %% length(distinct_colors) + 1]
  }
  
  # Ensure "No Active Entry" and "Other Combination" sit at the bottom of the legend hierarchy
  legend_levels <- c(other_states, "Other Combination", "No Active Entry")
  df_plot <- df_plot %>% mutate(treatment_regime = factor(treatment_regime, levels = legend_levels))
  
  # 7. Build the Plot
  p_state_timeline <- ggplot(df_plot, aes(x = study_day, y = user_label, fill = treatment_regime)) +
    geom_tile(width = 0.9, height = 0.9) +
    scale_fill_manual(values = regime_colors) +
    scale_x_continuous(breaks = sort(unique(x_breaks)), limits = c(0.5, actual_max_day + 0.5)) + 
    scale_y_discrete(breaks = y_breaks) +
    theme_minimal() +
    labs(
      title = "SOMA State-Transition Longitudinal Timeline",
      subtitle = "Tracking changes in personalized therapeutic combinations over time",
      x = "Day of Study",
      y = "Participant Number Index",
      fill = "Active Treatment Combination Status"
    ) +
    theme(
      axis.text.y = element_text(size = 8, family = "mono"),
      axis.text.x = element_text(size = 8),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 8)
    ) +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE))
  
  # 8. Export Graphic Asset
  folder_suffix <- if (!is.null(max_days)) paste0("_", max_days, "days") else "_all_days"
  target_dir <- file.path("figures", paste0(subfolder, folder_suffix))
  if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE)
  
  ggsave(
    filename = file.path(target_dir, "treatment_state_transition_timeline.png"),
    plot = p_state_timeline,
    width = 13,
    height = 9,
    dpi = 300
  )
  
  message(paste("State-Transition timeline plot saved successfully to:", target_dir, "\n"))
}
