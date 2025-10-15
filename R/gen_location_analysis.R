#' Simple location data analysis with any generic RDV
#'
#' Description
#'
#' Digital boundaries Source: Office for National Statistics licensed under the
#' Open Government Licence v.3.0
#' Contains OS data © Crown copyright and database right 2022
#'
#' @param df_rdv tibble; contains the RDV; required.
#' @param shp_path string; path to shapefile required for plotting.
#' @param shp_colname character; name of column to use in shapefile.
#' @param df_colname character; name of location column to use in RDV.
#' @param rank_colname character; name of index rank column in RDV.
#' @param decile_colname character; name of index decile column in RDV.
#' @param destination_long_lat numeric vector; longitude and latitude of
#' destination point.
#' @param stratify_by character; name of column to stratify the analysis by.
#' @param distance character; options for plotting distributions, one of geo_km,
#' travel_km or travel_mins.
#' @param group_cols named character vector; names of grouping columns.
#' @param home_location character; which home location(s) to include in analysis.
#'
#' @return Named list containing a summary table and maps.
#'
#' @family gen_location_analysis
#' @export
gen_location_analysis <- function(df_rdv = NULL,
                                  shp_path = NULL,
                                  shp_colname = NULL,
                                  df_colname = NULL,
                                  rank_colname = NULL,
                                  decile_colname = NULL,
                                  destination_long_lat = NULL,
                                  stratify_by = NULL,
                                  distance = NULL,
                                  group_cols = NULL,
                                  home_location = "all_in_cohort") {

  df_summary <- NULL
  list_map <- NULL
  imd_bar_plot <- NULL
  imd_violin_plot <- NULL
  distance_density <- NULL
  distance_boxplot <- NULL
  df_distance_counts <- NULL

  if (is.null(df_rdv) || nrow(df_rdv) == 0) {
    return(list(df = NULL,
                list_maps = NULL,
                imd_bar_plot = NULL,
                imd_violin_plot = NULL,
                distance_density = NULL,
                distance_boxplot = NULL,
                tbl_n = NULL
    ))
  }

  # TODO: Review as most output can be generated without shapefile
  if (is.null(shp_path) || shp_path == "") {
    message("Shapefile is required")
    return(list(df = NULL,
                list_maps = NULL,
                imd_bar_plot = NULL,
                imd_violin_plot = NULL,
                distance_density = NULL,
                distance_boxplot = NULL,
                tbl_n = NULL
    ))
  } else if (!file.exists(shp_path) || dir.exists(shp_path)) {
    message("Shapefile not found")
    return(list(df = NULL,
                list_maps = NULL,
                imd_bar_plot = NULL,
                imd_violin_plot = NULL,
                distance_density = NULL,
                distance_boxplot = NULL,
                tbl_n = NULL
    ))
  }

  logger::log_info("Started gen_location_analysis")
  start.time <- Sys.time()

  df_rdv <- df_rdv %>%
    dplyr::mutate(
      dplyr::across(tidyselect::all_of(c(stratify_by)), factor, .names = "group"))

  # TODO: Add check for passed column names
  if (decile_colname %in% names(df_rdv) && rank_colname %in% names(df_rdv)) {
    df_rdv$decile <- as.factor(df_rdv[[decile_colname]])
    df_rdv$rank <- df_rdv[[rank_colname]]
  }

  if (df_colname %in% names(df_rdv)) {
    df_rdv$subdivision <- df_rdv[[df_colname]]
  }

  if ("subdivision" %in% names(df_rdv)) {
    # TODO: handle all possible cases
    if (home_location == "max_in_cohort") {
      df_rdv <- df_rdv %>%
        # currently project IDs can be in multiple groups
        # revisit later
        dplyr::group_by(project_id, group) %>%
        # TODO: define date column name parameter
        dplyr::slice_max(order_by = end_datetime, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    } else if (home_location == "min_in_cohort") {
      df_rdv <- df_rdv %>%
        dplyr::group_by(project_id, group) %>%
        dplyr::slice_min(order_by = start_datetime, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()
    }

    # read shapefile
    shp <- sf::st_read(
      shp_path,
      quiet = TRUE
    )

    shp_transformed_simple <- sf::st_simplify(shp, dTolerance = 1000) %>%
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(c(shp_colname)), factor,
                      .names = "subdivision")
      )

    df_subdivisions <- shp_transformed_simple %>%
      dplyr::distinct(subdivision)

    df_counts <- df_rdv %>%
      dplyr::filter(!is.na(subdivision)) %>%
      dplyr::group_by(subdivision, dplyr::across(tidyselect::all_of(group_cols)), group) %>%
      dplyr::summarise(id_count = dplyr::n_distinct(project_id))

    list_df_counts <- split(df_counts, df_counts$group)

    list_subdivision <- lapply(list_df_counts, function(x) {

      shp_transformed_simple %>%
        dplyr::left_join(x, by = "subdivision") %>%
        dplyr::filter(!is.na(group)) %>%
        # distance between home location and destination in metres
        dplyr::mutate(
          geo_distance = geosphere::distVincentyEllipsoid(
            destination_long_lat,
            cbind(LONG, LAT)
          )/1000
        )

    })

    names(list_subdivision) <- names(list_df_counts)

    list_map <- lapply(list_subdivision, function(x) {

      pal_reverse <- leaflet::colorBin(
        palette = "viridis",
        domain = x$id_count,
        reverse = TRUE
      )

      pal <- leaflet::colorBin(
        palette = "viridis",
        domain = x$id_count,
        reverse = FALSE
      )

      # calculate total count per grouping variable
      # and show in maps
      # TODO: rewrite

      cols <- c("group", group_cols)

      for (col in cols) {
        x <- x %>%
          dplyr::group_by(dplyr::across(tidyselect::all_of(col))) %>%
          dplyr::mutate(#"{col}_id_count" := sum(id_count, na.rm = TRUE),
            #"{col}_perc" := 100*id_count/sum(id_count, na.rm = TRUE),
            "{col}_string" := sprintf(
              "<br/>%.2f%% of %s (%s)",
              100*id_count/sum(id_count, na.rm = TRUE),
              .data[[col]], col)) %>%
          dplyr::ungroup()
      }

      x <- x %>%
        tidyr::unite("cols_string",
                     tidyselect::all_of(paste0(cols, "_string")), sep = "")

      labels <- sprintf(
        "<strong>%s</strong><br/>%d cases %s",
        x$subdivision,
        x$id_count,
        x$cols_string
      ) %>% lapply(htmltools::HTML)

      map <- leaflet::leaflet(x) %>%
        #leaflet::addTiles() %>%
        leaflet::addTiles(attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors. Digital boundaries Source: Office for National Statistics licensed under the Open Government Licence version 3.0') %>%
        leaflet::addPolygons(fillColor = ~pal_reverse(id_count),
                             weight = 0.2,
                             opacity = 0.5,
                             color = "purple",
                             dashArray = "1",
                             fillOpacity = 1,
                             label = labels,
                             labelOptions = leaflet::labelOptions(
                               style = list("font-weight" = "normal",
                                            padding = "3px 8px"),
                               textsize = "15px",
                               direction = "auto")) %>%
        leaflet::addLegend(pal = pal,
                           values = ~id_count,
                           opacity = 0.7,
                           title = NULL,
                           position = "bottomright",
                           labFormat = leaflet::labelFormat(
                             digits = 0,
                             transform = function(val) sort(val, decreasing = TRUE)
                           )
        )

      list(group = paste0("Cohort ", unique(x$group)),
           map = map,
           line = shiny::br())

    })

    df_summary <- df_counts %>%
      dplyr::left_join(df_subdivisions, by = "subdivision") %>%
      dplyr::rename(area = subdivision) %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(total_id_count = sum(id_count)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(perc = paste0(round(id_count / total_id_count * 100),"%")) %>%
      tidyr::pivot_wider(id_cols = area,
                         names_from = group,
                         names_prefix = "cohort_",
                         values_from = c(id_count, perc))

    df_distance <- do.call(rbind.data.frame, list_subdivision)

    df_distance_counts <- df_distance %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(number_of_patients = sum(id_count, na.rm = TRUE)) %>%
      dplyr::ungroup()

    # geometry column persists after grouping
    df_distance_counts <- data.frame(
      group = df_distance_counts$group,
      number_of_patients = df_distance_counts$number_of_patients
    )
    names(df_distance_counts) <- stringr::str_to_title(
      gsub('[^a-zA-Z0-9]', ' ', names(df_distance_counts))
    )

    df_distance <- data.frame(
      group = rep(df_distance$group, df_distance$id_count),
      geo_distance = rep(df_distance$geo_distance, df_distance$id_count)
    )

    distance_density <- NULL
    distance_boxplot <- NULL

    if (distance == "geo_km") {

      distance_density <- plot_density(df_plot = df_distance,
                                       xvar = "geo_distance",
                                       fill = "group",
                                       colour = "group",
                                       xlab = "Distance between home location and hospital (km)")

      distance_boxplot <- plot_boxplot(df_plot = df_distance,
                                       xvar = "geo_distance",
                                       yvar = "group",
                                       colour = "group",
                                       xlab = "Distance between home location and hospital (km)")
    } else if (distance == "travel_km") {
      distance_density <- plot_density(df_plot = df_rdv,
                                       xvar = "traveldist_km",
                                       fill = "group",
                                       colour = "group",
                                       xlab = "Travel distance between home location and hospital (km)")

      distance_boxplot <- plot_boxplot(df_plot = df_rdv,
                                       xvar = "traveldist_km",
                                       yvar = "group",
                                       colour = "group",
                                       xlab = "Travel distance between home location and hospital (km)")
    } else if (distance == "travel_mins") {
      distance_density <- plot_density(df_plot = df_rdv,
                                       xvar = "traveltime_mins",
                                       fill = "group",
                                       colour = "group",
                                       xlab = "Travel time between home location and hospital (mins)")

      distance_boxplot <- plot_boxplot(df_plot = df_rdv,
                                       xvar = "traveltime_mins",
                                       yvar = "group",
                                       colour = "group",
                                       xlab = "Travel time between home location and hospital (mins)")
    }

  }

  if (!is.null(df_rdv$decile) && !is.null(df_rdv$rank)) {
    # Plot count per IMD decile
    df_imd_grouped <- df_rdv %>%
      dplyr::group_by(group, decile) %>%
      dplyr::summarise(counts = dplyr::n_distinct(project_id)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(group) %>%
      dplyr::mutate(group_total = sum(counts), perc = counts / sum(counts) * 100)

    imd_bar_plot <- ggplot2::ggplot(df_imd_grouped,
                                    ggplot2::aes(x = decile,
                                                 y = perc, fill = group)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_text(ggplot2::aes(group = group, label = paste0("n=", counts)),
                         position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::ylab("% of cohort") +
      ggplot2::xlab("Decile") +
      ggplot2::labs(fill = 'Cohort') +
      ggplot2::coord_flip()

    # Plot distribution of IMD rank
    imd_violin_plot <- ggplot2::ggplot(df_rdv,
                                       ggplot2::aes(x = group,
                                                    y = rank,
                                                    fill = group)) +
      ggplot2::geom_violin() +
      ggplot2::geom_boxplot(width=0.1) +
      #ggplot2::scale_y_log10() +
      ggplot2::ylab("Rank") +
      ggplot2::xlab("Cohort") +
      ggplot2::labs(fill = 'Cohort')
  }

  log_record_time_taken("gen_location_analysis", start.time)

  return(
    list(
      df = df_summary,
      list_maps = list_map,
      imd_bar_plot = imd_bar_plot,
      imd_violin_plot = imd_violin_plot,
      distance_density = distance_density,
      distance_boxplot = distance_boxplot,
      tbl_n = df_distance_counts
    )
  )

}
