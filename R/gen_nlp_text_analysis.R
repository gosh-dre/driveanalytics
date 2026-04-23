#' Natural Language Processing Analysis Using NRC Emotion Lexicon
#'
#' Analyzes text data to detect 8 emotions (anger, fear, sadness, joy, trust, disgust, 
#' surprise, anticipation) with negation handling (e.g., "not happy" → sadness).
#'
#' @param df_rdv Data frame containing text data and metadata
#' @param text_col Column name containing text to analyze
#' @param stratify_by Column name for cohort stratification (defaults to "cohort")
#'
#' @return List with doc_metadata (Emotion_* columns), emotion_data, stratify_by, emotion_names
#'
#' @references Mohammad & Turney (2013). Crowdsourcing a word-emotion association lexicon.
#'   Computational Intelligence, 29(3), 436-465.
#'
#' @family gen_nlp_text_analysis
#' @export
gen_nlp_text_analysis_model <- function(df_rdv, text_col = NULL, stratify_by = NULL) {

  if (is.null(df_rdv) || is.null(text_col) || nrow(df_rdv) == 0) {
    return(list(doc_metadata = NULL, emotion_data = NULL, stratify_by = NULL, emotion_names = NULL))
  }

  if (is.null(stratify_by)) {
    stratify_by <- "cohort"
    if (!"cohort" %in% names(df_rdv)) df_rdv <- df_rdv %>% dplyr::mutate(cohort = "All")
  }

  start.time <- log_start_function_return_time("gen_nlp_text_analysis_model")

  df_rdv[[text_col]] <- as.character(df_rdv[[text_col]])
  df_rdv[[stratify_by]] <- as.factor(df_rdv[[stratify_by]])

  # Load NRC lexicon (8 emotions only, exclude positive/negative sentiment)
  nrc_emotions <- tryCatch({
    tidytext::get_sentiments("nrc") %>% 
    dplyr::filter(!sentiment %in% c("positive", "negative"))
  }, error = function(e) data.frame(word = character(), sentiment = character()))
  
  if (nrow(nrc_emotions) == 0) {
    log_record_time_taken("gen_nlp_text_analysis_model", start.time)
    return(list(doc_metadata = NULL, emotion_data = NULL, stratify_by = NULL, emotion_names = NULL))
  }
  
  text_df <- data.frame(doc_id = seq_len(nrow(df_rdv)), text = df_rdv[[text_col]], stringsAsFactors = FALSE)
  
  # Negation detection: "not happy", "no fear", etc. can be expanded upon
  negation_words <- c("not", "no", "never", "without", "neither", "nobody", "nothing", "nowhere", "none", "hardly", "barely", "scarcely")
  
  # Extract bigrams and identify negated emotion words
  bigrams <- text_df %>%
    tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE)
  
  negated_words <- bigrams %>%
    dplyr::filter(word1 %in% negation_words) %>%
    dplyr::inner_join(nrc_emotions, by = c("word2" = "word")) %>%
    dplyr::select(doc_id, word = bigram, emotion = sentiment, word2) %>%
    dplyr::mutate(negated = TRUE)
  
  # Extract non-negated emotion words (excluding words that appear in negated bigrams)
  emotion_data_base <- text_df %>%
    tidytext::unnest_tokens(word, text) %>%
    dplyr::inner_join(nrc_emotions, by = "word") %>%
    dplyr::rename(emotion = sentiment) %>%
    dplyr::mutate(negated = FALSE) %>%
    dplyr::anti_join(negated_words %>% dplyr::select(doc_id, word2), by = c("doc_id", "word" = "word2"))
  
  # Flip emotions for negated words
  negated_emotions <- negated_words %>%
    dplyr::select(doc_id, word, emotion) %>%
    dplyr::mutate(emotion = dplyr::case_when(
      emotion == "joy" ~ "sadness", emotion == "trust" ~ "fear", emotion == "anticipation" ~ "surprise",
      emotion == "sadness" ~ "joy", emotion == "fear" ~ "trust", emotion == "anger" ~ "trust",
      emotion == "disgust" ~ "joy", emotion == "surprise" ~ "surprise", TRUE ~ emotion
    ), negated = TRUE)
  
  # Combine and calculate emotion prevalence per document
  emotion_data <- dplyr::bind_rows(emotion_data_base, negated_emotions) %>%
    dplyr::count(doc_id, emotion, word, negated) %>%
    dplyr::group_by(doc_id, emotion) %>%
    dplyr::mutate(total_emotion_words = sum(n), proportion = n / total_emotion_words) %>%
    dplyr::ungroup()
  
  emotion_doc <- emotion_data %>%
    dplyr::group_by(doc_id, emotion) %>%
    dplyr::summarise(proportion = sum(proportion), .groups = "drop")
  
  # TEST: emotion_wide must have columns named "Emotion_<emotion>" with values 0-1
  # Pivot to wide format with Emotion_* columns (required for downstream functions)
  emotion_wide <- emotion_doc %>%
    tidyr::pivot_wider(names_from = emotion, values_from = proportion, values_fill = 0, names_prefix = "Emotion_")
  
  df_rdv <- df_rdv %>%
    dplyr::mutate(doc_id = dplyr::row_number()) %>%
    dplyr::left_join(emotion_wide, by = "doc_id") %>%
    dplyr::select(-doc_id)
  
  emotion_cols <- grep("^Emotion_", names(df_rdv), value = TRUE)
  df_rdv[emotion_cols] <- lapply(df_rdv[emotion_cols], function(x) ifelse(is.na(x), 0, x))
  
  log_record_time_taken("gen_nlp_text_analysis_model", start.time)
  
  return(list(
    doc_metadata = df_rdv, 
    stratify_by = stratify_by, 
    emotion_data = emotion_data, 
    emotion_names = unique(emotion_data$emotion)
  ))
}


#' Generate Emotion Prevalence Plot
#'
#' Creates bar chart showing average emotion proportions across cohorts.
#'
#' @param model_result Output from gen_nlp_text_analysis_model()
#' @return Plotly interactive bar chart
#' @export
gen_nlp_topic_prevalence <- function(model_result) {
  
  doc_metadata <- model_result$doc_metadata
  stratify_by <- model_result$stratify_by
  emotion_cols <- grep("^Emotion_", names(doc_metadata), value = TRUE)
  
  if (length(emotion_cols) == 0) return(NULL)
  
  prevalence_data <- doc_metadata %>%
    dplyr::select(dplyr::all_of(c(stratify_by, emotion_cols))) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(emotion_cols), names_to = "Emotion", values_to = "Prevalence") %>%
    dplyr::mutate(Emotion = gsub("^Emotion_", "", Emotion)) %>%
    dplyr::rename(Cohort = dplyr::all_of(stratify_by)) %>%
    dplyr::group_by(Emotion, Cohort) %>%
    dplyr::summarise(Prevalence = mean(Prevalence, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot2::ggplot(prevalence_data, ggplot2::aes(x = Emotion, y = Prevalence, fill = Cohort,
    text = paste0("Emotion: ", Emotion, "\nCohort: ", Cohort, "\nPrevalence: ", round(Prevalence, 3)))) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::labs(title = "Emotion Prevalence by Cohort", 
                  subtitle = "Average proportion of each NRC emotion across cohorts",
                  x = "Emotion", y = "Prevalence", fill = "Cohort") +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9))
  
  plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(hovermode = "closest")
}


#' Generate Top Emotion Words Table
#'
#' Shows most frequent words (including negated bigrams) for each emotion.
#'
#' @param model_result Output from gen_nlp_text_analysis_model()
#' @param n_words Number of top words per emotion
#' @return Data frame with Emotion and Words columns
#' @export
gen_nlp_top_words <- function(model_result, n_words = 30) {
  if (is.null(model_result$emotion_data)) return(NULL)
  
  model_result$emotion_data %>%
    dplyr::group_by(emotion, word) %>%
    dplyr::summarise(total_count = sum(n), .groups = "drop") %>%
    dplyr::group_by(emotion) %>%
    dplyr::arrange(desc(total_count)) %>%
    dplyr::slice_head(n = n_words) %>%
    dplyr::summarise(Words = paste(word, collapse = ", "), .groups = "drop") %>%
    dplyr::rename(Emotion = emotion)
}


#' Create Evolution Plot (Internal Helper)
#' @keywords internal
.create_evolution_plot <- function(emotion_by_time, x_var, x_label, subtitle, cohort, time_mode) {
  if (nrow(emotion_by_time) == 0) return(NULL)
  
  # Build hover tooltips based on time mode
  emotion_by_time$tooltip <- if (time_mode == "absolute") {
    paste0("Emotion: ", emotion_by_time$Emotion, "\nDate: ", format(emotion_by_time[[x_var]], "%Y-%m-%d"), 
           "\nPrevalence: ", round(emotion_by_time$Prevalence, 3))
  } else if (time_mode == "relative") {
    paste0("Emotion: ", emotion_by_time$Emotion, "\nDays: ", round(emotion_by_time[[x_var]], 1), 
           "\nPrevalence: ", round(emotion_by_time$Prevalence, 3))
  } else {
    paste0("Emotion: ", emotion_by_time$Emotion, "\nTime: ", round(emotion_by_time[[x_var]], 1), 
           "%\nPrevalence: ", round(emotion_by_time$Prevalence, 3))
  }
  
  p <- ggplot2::ggplot(emotion_by_time, ggplot2::aes(x = .data[[x_var]], y = .data[["Prevalence"]], 
                                                       color = .data[["Emotion"]], group = .data[["Emotion"]], 
                                                       text = .data[["tooltip"]])) +
    ggplot2::geom_line(linewidth = 1) + 
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = paste0("Emotion Evolution Over Time - ", cohort), 
                  subtitle = subtitle, x = x_label, y = "Emotion Prevalence", color = "Emotion") +
    ggplot2::scale_y_continuous(limits = c(0, NA), expand = c(0, 0.05)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  # Format x-axis based on time mode
  if (time_mode == "absolute") {
    p <- p + ggplot2::scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month")
  } else if (time_mode == "normalized") {
    p <- p + ggplot2::scale_x_continuous(breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%"), limits = c(0, 100))
  }
  
  plotly::ggplotly(p, tooltip = "text") %>% plotly::layout(hovermode = "closest")
}


#' Generate Emotion Evolution Over Time Plots
#'
#' Creates line plots showing how emotion prevalence changes over time for each cohort.
#' Supports three time modes: normalized (0-100%), relative (days), absolute (calendar dates).
#'
#' @param model_result Output from gen_nlp_text_analysis_model()
#' @param resolution Number of time bins for aggregation
#' @param time_mode "normalized", "relative", or "absolute"
#' @return Named list of plotly objects (one per cohort)
#' @export
gen_nlp_topic_evolution <- function(model_result, resolution = 100, time_mode = "normalized") {
  
  doc_metadata <- model_result$doc_metadata
  stratify_by <- model_result$stratify_by
  
  if (!all(c("start_datetime", "project_id") %in% names(doc_metadata))) return(NULL)
  if (is.null(time_mode) || length(time_mode) == 0) time_mode <- "normalized"
  
  emotion_cols <- grep("^Emotion_", names(doc_metadata), value = TRUE)
  if (length(emotion_cols) == 0) return(NULL)
  
  evolution_plots <- list()
  
  for (cohort in levels(doc_metadata[[stratify_by]])) {
    cohort_data <- doc_metadata[doc_metadata[[stratify_by]] == cohort, ]
    if (nrow(cohort_data) == 0) next
    
    # Prepare time variable based on mode
    if (time_mode == "normalized") {
      # Scale each patient's timeline to 0-100%
      time_data <- cohort_data %>%
        dplyr::group_by(project_id) %>%
        dplyr::mutate(normalized_time = {
          duration <- as.numeric(difftime(max(start_datetime, na.rm = TRUE), min(start_datetime, na.rm = TRUE), units = "days"))
          if (duration > 0) as.numeric(difftime(start_datetime, min(start_datetime, na.rm = TRUE), units = "days")) / duration else 0
        }) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(time_bin = cut(normalized_time, breaks = min(resolution, length(unique(normalized_time))), include.lowest = TRUE)) %>%
        dplyr::group_by(time_bin) %>%
        dplyr::mutate(normalized_time_percent = mean(normalized_time, na.rm = TRUE) * 100) %>%
        dplyr::ungroup()
      x_var <- "normalized_time_percent"
      x_label <- "Normalized Time (%)"
      subtitle <- "Normalized time (0-100% of treatment)"
      
    } else if (time_mode == "relative") {
      # Days since first response
      global_first <- min(cohort_data$start_datetime, na.rm = TRUE)
      time_data <- cohort_data %>%
        dplyr::mutate(mean_days = as.numeric(difftime(start_datetime, global_first, units = "days")),
                      time_bin = cut(mean_days, breaks = resolution, include.lowest = TRUE)) %>%
        dplyr::group_by(time_bin) %>%
        dplyr::mutate(mean_days = mean(mean_days, na.rm = TRUE)) %>%
        dplyr::ungroup()
      x_var <- "mean_days"
      x_label <- "Days Since First Response"
      subtitle <- "Days since first response in dataset"
      
    } else {
      # Absolute calendar dates
      time_data <- cohort_data %>%
        dplyr::mutate(time_bin = cut(start_datetime, breaks = resolution, include.lowest = TRUE)) %>%
        dplyr::group_by(time_bin) %>%
        dplyr::mutate(mean_date = mean(start_datetime, na.rm = TRUE)) %>%
        dplyr::ungroup()
      x_var <- "mean_date"
      x_label <- "Date"
      subtitle <- "Calendar dates"
    }
    
    # Aggregate emotion prevalence by time bin
    emotion_by_time <- time_data %>%
      dplyr::select(dplyr::all_of(x_var), dplyr::starts_with("Emotion_")) %>%
      tidyr::pivot_longer(cols = dplyr::starts_with("Emotion_"), names_to = "Emotion", values_to = "Prevalence") %>%
      dplyr::group_by(!!as.name(x_var), Emotion) %>%
      dplyr::summarise(Prevalence = mean(Prevalence, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(Emotion = gsub("^Emotion_", "", Emotion))
    
    plot <- .create_evolution_plot(emotion_by_time, x_var, x_label, subtitle, cohort, time_mode)
    if (!is.null(plot)) evolution_plots[[cohort]] <- plot
  }
  
  evolution_plots
}
