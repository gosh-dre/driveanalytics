#' Sentiment vs Severity Analysis Module
#'
#' Description
#' This function performs sentiment analysis on clinical notes and compares it against
#' extracted or provided severity levels derived from AFINN sentiment scores.
#'
#' @param df_nlp tibble; contains the clinical notes; required.
#' @param text_col character; name of column containing the clinical text.
#' @param stratify_by character; name of column to stratify the analysis by (e.g., Anatomy).
#' @param group_cols named character vector; names of grouping columns for summary.
#'
#' @return Named list containing summary tables and ggplot objects.
#'
#' @family gen_sentiment_analysis
#' @export
gen_sentiment_severity_analysis <- function(df_nlp = NULL,
                                            text_col = "raw_clinical_note",
                                            stratify_by = NULL,
                                            group_cols = NULL) {

  df_summary <- NULL
  sentiment_boxplot <- NULL
  severity_bar_plot <- NULL
  sentiment_density <- NULL
  tbl_n <- NULL

  # 1. Input Validation
  if (is.null(df_nlp) || nrow(df_nlp) == 0) {
    return(list(df = NULL, plots = NULL))
  }
  
  if (!text_col %in% names(df_nlp)) {
    stop(paste("Column", text_col, "not found in dataframe."))
  }

  logger::log_info("Started gen_sentiment_severity_analysis")
  start.time <- Sys.time()

  # 2. Data Processing & Feature Engineering
  sentiments <- tidytext::get_sentiments("afinn")

  # Define negation words
  negation_words <- c("not", "no", "never", "without", "can't", "cant", 
                      "don't", "dont", "won't", "wont", "didn't", "didnt",
                      "isn't", "isnt", "aren't", "arent", "wasn't", "wasnt",
                      "weren't", "werent", "hasn't", "hasnt", "haven't", "havent",
                      "hadn't", "hadnt", "shouldn't", "shouldnt", "wouldn't", 
                      "wouldnt", "couldn't", "couldnt", "neither", "none", "nobody",
                      "nothing", "nowhere", "hardly", "scarcely", "barely")

  stop_words <- tidytext::stop_words %>%
    dplyr::filter(!word %in% negation_words)  # Keep all negation words

  # Create bigrams for negation handling (gets all the no pain, not happy etc)
  bigrams <- df_nlp %>%
    dplyr::mutate(doc_id = dplyr::row_number()) %>%
    dplyr::select(dplyr::all_of(c("doc_id", text_col, stratify_by))) %>%
    tidyr::drop_na(dplyr::all_of(text_col)) %>%
    tidytext::unnest_tokens(bigram, !!as.name(text_col), token = "ngrams", n = 2) %>%
    tidyr::separate(bigram, c("word1", "word2"), sep = " ", remove = FALSE)

  # Process negated sentiment (bigrams starting with negation)
  negated_sentiment <- bigrams %>%
    dplyr::filter(word1 %in% negation_words) %>%
    dplyr::filter(!word2 %in% stop_words$word) %>%
    dplyr::inner_join(sentiments, by = c("word2" = "word")) %>%
    dplyr::mutate(value = -value) %>%  # Flip the sentiment
    dplyr::select(doc_id, word = bigram, value, stratify_by)

  # Process regular unigrams (not part of negation bigrams)
  negation_bigrams <- bigrams %>%
    dplyr::filter(word1 %in% negation_words) %>%
    dplyr::select(doc_id, word2)

  unigrams <- df_nlp %>%
    dplyr::mutate(doc_id = dplyr::row_number()) %>%
    dplyr::select(dplyr::all_of(c("doc_id", text_col, stratify_by))) %>%
    tidyr::drop_na(dplyr::all_of(text_col)) %>%
    tidytext::unnest_tokens(word, !!as.name(text_col))

  regular_sentiment <- unigrams %>%
    dplyr::anti_join(negation_bigrams, by = c("doc_id", "word" = "word2")) %>%
    dplyr::anti_join(stop_words, by = "word") %>%
    dplyr::filter(!word %in% negation_words) %>%  # Remove standalone negations
    dplyr::inner_join(sentiments, by = "word") %>%
    dplyr::select(doc_id, word, value, stratify_by)

  # Combine negated and regular sentiment
  all_sentiment_words <- dplyr::bind_rows(negated_sentiment, regular_sentiment) %>%
    dplyr::filter(value != 0)

  # Aggregate sentiment per document
  df_sentiment_summary <- all_sentiment_words %>%
    dplyr::group_by(doc_id) %>%
    dplyr::summarise(
      sentiment_total = sum(value, na.rm = TRUE),
      avg_sentiment = mean(value, na.rm = TRUE),
      word_count = dplyr::n(),
      .groups = "drop"
    )
  
  # Join sentiment scores back to original df_nlp
  df_nlp <- df_nlp %>%
    dplyr::mutate(doc_id = dplyr::row_number()) %>%
    dplyr::left_join(df_sentiment_summary, by = "doc_id") %>%
    dplyr::select(-doc_id)  # Remove temporary doc_id

  # Calculate weighted sentiment score
  # Formula: total_sentiment * sqrt(word_count) / (sqrt(word_count) + 1)
  # This gives more weight to documents with more sentiment words while still normalizing
  df_nlp <- df_nlp %>%
    dplyr::mutate(
      sentiment_score = ifelse(
        is.na(sentiment_total) | is.na(word_count),
        0,
        sentiment_total * sqrt(word_count) / (sqrt(word_count) + 1)
      )
    )

  # Derive severity from weighted sentiment scores
  df_nlp <- df_nlp %>%
    dplyr::mutate(
      severity_derived = dplyr::case_when(
        is.na(avg_sentiment) ~ "Unknown",  # No sentiment words found
        sentiment_score <= -3 ~ "High",       # Very negative
        sentiment_score > -3 & sentiment_score < -1 ~ "Moderate",  # Somewhat negative
        sentiment_score >= -1 & sentiment_score < 1 ~ "Low",       # Neutral/mild
        sentiment_score >= 1 ~ "Very Low",    # Positive
        TRUE ~ "Unknown"
      ),
      severity_derived = factor(severity_derived, levels = c("Very Low", "Low", "Moderate", "High", "Unknown"))
    )

  # 3. Stratification
  if (!is.null(stratify_by) && stratify_by %in% names(df_nlp)) {
    df_nlp <- df_nlp %>%
      dplyr::mutate(group = as.factor(.data[[stratify_by]]))
  } else {
    df_nlp$group <- "All Patients"
  }

  # 4. Generate Plots
  
  # Plot 1: Sentiment Score Distribution by Severity (Boxplot)
  p1 <- ggplot2::ggplot(df_nlp, 
                        ggplot2::aes(x = severity_derived, 
                                     y = sentiment_score, 
                                     fill = severity_derived,
                                     text = paste0(
                                       "Severity: ", severity_derived, "\n",
                                       "Sentiment Score: ", round(sentiment_score, 2), "\n",
                                       "Patient ID: ", project_id
                                     ))) +
    ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.3) +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::labs(title = "Sentiment Score vs. Severity Level",
                  x = "Severity Level",
                  y = "Sentiment Score (AFINN)",
                  fill = "Severity") +
    ggplot2::theme_minimal()
  
  sentiment_boxplot <- plotly::ggplotly(p1, tooltip = "text")

  # Plot 2: Severity Counts (Bar Plot) stratified by Group (e.g. Anatomy)
  # Pre-compute counts for tooltip
  df_counts <- df_nlp %>%
    dplyr::group_by(severity_derived, group) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
  p2 <- ggplot2::ggplot(df_counts, 
                        ggplot2::aes(x = severity_derived, 
                                     y = count,
                                     fill = group,
                                     text = paste0(
                                       "Severity: ", severity_derived, "\n",
                                       "Group: ", group, "\n",
                                       "Count: ", count
                                     ))) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::labs(title = "Count of Reports by Severity",
                  x = "Severity Level",
                  y = "Count",
                  fill = stratify_by) +
    ggplot2::theme_minimal()
  
  severity_bar_plot <- plotly::ggplotly(p2, tooltip = "text")

  # Plot 3: Sentiment Distribution (Density by Group)
  p3 <- ggplot2::ggplot(df_nlp, 
                        ggplot2::aes(x = sentiment_score,
                                     fill = group,
                                     colour = group,
                                     group = group)) +
    ggplot2::geom_density(alpha = 0.3, linewidth = 1) +
    ggplot2::scale_fill_viridis_d(option = "C") +
    ggplot2::scale_color_viridis_d(option = "C") +
    ggplot2::labs(title = "Distribution of Sentiment Scores by Group",
                  x = "Sentiment Score",
                  y = "Density",
                  fill = stratify_by,
                  color = stratify_by) +
    ggplot2::theme_minimal()

  sentiment_density <- plotly::ggplotly(p3)

  # 5. Generate Summary Table
  df_summary <- df_nlp %>%
    dplyr::group_by(group, severity_derived) %>%
    dplyr::summarise(
      count = dplyr::n(),
      avg_sentiment = mean(sentiment_score, na.rm = TRUE),
      min_sentiment = min(sentiment_score, na.rm = TRUE),
      max_sentiment = max(sentiment_score, na.rm = TRUE),
      .groups = "drop"
    )

  log_record_time_taken("gen_sentiment_severity_analysis", start.time)

  return(
    list(
      df = df_summary,
      sentiment_boxplot = sentiment_boxplot,
      severity_bar_plot = severity_bar_plot,
      sentiment_density = sentiment_density,
      raw_data = df_nlp # Returning processed data for table view
    )
  )
}