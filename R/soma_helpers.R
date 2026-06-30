## Helper functions for SOMA data wrangling, preprocessing, and analysis

soma_extract_ids <- function(user_data, studies_data, responses_data, verbose = TRUE) {
  
  # --------------------------------------------------
  # STEP 1: FILTERING BY ROLE ('user')
  # --------------------------------------------------
  total_subjects_raw <- nrow(user_data)
  df_users_only <- user_data %>% filter(role == "user")
  n_users_only <- nrow(df_users_only)
  
  if (verbose) {
    cat("\n==================================================\n")
    cat("      STEP 1: FILTERING BY ROLE ('user')\n")
    cat("==================================================\n")
    cat("Total raw entries in user_data:         ", total_subjects_raw, "\n")
    cat("Absolute number of 'user' roles:        ", n_users_only, "\n")
    cat("Proportion of 'user' roles:             ", round((n_users_only / total_subjects_raw) * 100, 2), "%\n")
  }
  
  # --------------------------------------------------
  # STEP 2: EXTRACTING CONSENTED USERS
  # --------------------------------------------------
  df_consented <- df_users_only %>%
    mutate(
      agreement_flag = map_lgl(agreements, ~ {
        if (is.null(.x) || nrow(.x) == 0) return(FALSE)
        any(!is.na(.x$agreement) & .x$agreement != "" & !is.na(.x$acceptedAt))
      })
    ) %>%
    filter(agreement_flag == TRUE)
  
  # Extract the clean vector of True Participant IDs
  true_participant_ids <- df_consented %>%
    select(participant_id = `_id`) %>% 
    pull(participant_id)
  
  if (verbose) {
    n_consented <- length(true_participant_ids)
    cat("\n==================================================\n")
    cat("      STEP 2: EXTRACTING CONSENTED USERS\n")
    cat("==================================================\n")
    cat("Absolute number of consented users:     ", n_consented, "\n")
    cat("Proportion out of 'user' subgroup:      ", round((n_consented / n_users_only) * 100, 2), "%\n")
    cat("Successfully isolated", n_consented, "true Participant IDs to carry forward.\n")
  }
  
  # --------------------------------------------------
  # STEP 3: LINKING TO QUALTRICS (STUDIES)
  # --------------------------------------------------
  df_linked_studies <- studies_data %>%
    filter(participant_participant %in% true_participant_ids) %>%
    select(participant_id = participant_participant, qualtrics_uuid = uuid) %>%
    distinct()
  
  if (verbose) {
    n_linked_studies <- n_distinct(df_linked_studies$participant_id)
    cat("\n==================================================\n")
    cat("      STEP 3: LINKING TO QUALTRICS (STUDIES)\n")
    cat("==================================================\n")
    cat("Consented participants found in studies_data: ", n_linked_studies, "out of", length(true_participant_ids), "\n")
    cat("Proportion of matched Qualtrics links:       ", round((n_linked_studies / length(true_participant_ids)) * 100, 2), "%\n")
  }
  
  # --------------------------------------------------
  # STEP 4: LINKING TO EMA DATA (RESPONSES)
  # --------------------------------------------------
  df_linked_responses <- responses_data %>%
    filter(user %in% true_participant_ids)
  
  if (verbose) {
    n_linked_responses <- n_distinct(df_linked_responses$user)
    cat("\n==================================================\n")
    cat("      STEP 4: LINKING TO EMA DATA (RESPONSES)\n")
    cat("==================================================\n")
    cat("Consented participants found in responses_data: ", n_linked_responses, "out of", length(true_participant_ids), "\n")
    cat("Proportion of matched EMA tracks:                ", round((n_linked_responses / length(true_participant_ids)) * 100, 2), "%\n")
  }
  
  # --------------------------------------------------
  # STEP 5: EXTRACTING COMPLETE DUAL-TRACK POOL
  # --------------------------------------------------
  ids_with_qualtrics <- df_linked_studies$participant_id
  
  df_final_dual_track <- responses_data %>%
    filter(user %in% ids_with_qualtrics)
  
  if (verbose) {
    n_final_participants <- n_distinct(df_final_dual_track$user)
    cat("\n==================================================\n")
    cat("      STEP 5: EXTRACTING COMPLETE DUAL-TRACK POOL\n")
    cat("==================================================\n")
    cat("Participants with BOTH Qualtrics & EMA data: ", n_final_participants, "\n")
    cat("Proportion out of all consented users:        ", round((n_final_participants / length(true_participant_ids)) * 100, 2), "%\n")
    cat("==================================================\n")
  }
  
  # Return everything cleanly bundled together inside a named list
  return(
    list(
      true_participant_ids = true_participant_ids,
      df_linked_studies    = df_linked_studies,
      df_linked_responses  = df_linked_responses,
      df_final_dual_track  = df_final_dual_track
    )
  )
}
