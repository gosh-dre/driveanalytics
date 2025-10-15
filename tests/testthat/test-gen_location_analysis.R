# create some location data
# TODO: replace with dummy locations RDV

create_dummy_loc <- function() {
  df_loc_sample <- data.frame(
    postcode = c("LU1","CO13","N5","N7","CM3","HA8","HR2","RM16","PE30","E1"),
    `LSOA Code` = c("E01017561", "E01022009",	"E01002749", "E01033486", "E01021540", "E01000188", "E01013997", "E01015977", "E01026709", "E01004284"),
    `MSOA Code` = c("E02003651", "E02004580", "E02000559", "E02000564", "E02004485", "E02000037", "E02002920", "E02003299", "E02005555", "E02000882"),
    District = c("Central Bedfordshire", "Tendring", "Islington", "Islington", "Chelmsford", "Barnet", "Herefordshire, County of", "Thurrock", "King's Lynn and West Norfolk", "Tower Hamlets"),
    County = c("Bedfordshire", "Essex", "Greater London", "Greater London", "Essex", "Greater London", "Herefordshire", "Essex", "Norfolk", "Greater London"),
    Region = c("East of England", "East of England", "London", "London", "East of England", "London", "West Midlands", "East of England", "East of England", "London"),
    Country = "England",
    `LSOA.name..2011.` = c("Central Bedfordshire 033A", "Tendring 008E", "Islington 006D", "Islington 011E", "Chelmsford 001C", "Barnet 014C", "Herefordshire 016B", "Thurrock 004A", "King's Lynn and West Norfolk 005D", "Tower Hamlets 019A"),
    `Local.Authority.District.code..2019.` = c("E06000056", "E07000076", "E09000019", "E09000019", "E07000070", "E09000003", "E06000019", "E06000034", "E07000146", "E09000030"),
    `Local.Authority.District.name..2019.` = c("Central Bedfordshire", "Tendring", "Islington", "Islington", "Chelmsford", "Barnet", "Herefordshire, County of", "Thurrock", "King's Lynn and West Norfolk", "Tower Hamlets"),
    `Index.of.Multiple.Deprivation..IMD..Rank` = c(21833, 14233, 12160, 5005, 20306, 27036, 25748, 22346, 30752, 7366),
    `Index.of.Multiple.Deprivation..IMD..Decile` = c(7, 5, 4, 2, 7, 9, 8, 7, 10, 3),
    pcd_district_centre_long = c(-0.395007791, 0.144418194, -0.114086985, -0.114242862, 0.583769368, -0.279519434, -2.637214118, 0.328621441, 0.428261039, -0.060561205),
    pcd_district_centre_lat = c(51.87120772, 52.69058762, 51.55783055, 51.56316842, 51.74219429, 51.62240206, 52.02603079, 51.48906984, 52.7699486, 51.51513757),
    traveltime_mins = c(62.96333333, 129.7066667, 37.55166667, 37.99333333, 45.22, 41.94, 202.7233333, 19.32, 133.915, 22.09666667),
    traveldist_km = c(81.0171, 162.3663, 28.9444, 29.0182, 49.9176, 39.1723, 230.5349, 19.0508, 168.2171, 14.684),
    imd_rank = c(21833, 14233, 12160, 5005, 20306, 27036, 25748, 22346, 30752, 7366),
    imd_decile = c(7, 5, 4, 2, 7, 9, 8, 7, 10, 3)
  )

  df_loc <- df_pde %>%
    dplyr::select(project_id, birth_date, death_date, cohort, cohort_id, cohort_entry_date, cohort_exit_date) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      start_date = birth_date,
      end_datetime = dplyr::if_else(is.na(death_date), Sys.Date(), death_date),
      address_type = "Permanent"
    )
  df_loc <- cbind(df_loc, dplyr::slice_sample(df_loc_sample, n = nrow(df_loc), replace = TRUE))

  shp_path <- system.file("external_data", "districts.shp", package = "driveanalytics")

  return(
    list(
      df_loc = df_loc,
      shp_path = shp_path
    )
  )
}

dummy_loc <- create_dummy_loc()

test_that("run data", {

  gla <- driveanalytics::gen_location_analysis(
    df_rdv = dummy_loc$df_loc,
    shp_path = dummy_loc$shp_path,
    shp_colname = "LAD20NM",
    df_colname = "District",
    rank_colname = "imd_rank",
    decile_colname = "Index.of.Multiple.Deprivation..IMD..Decile",
    destination_long_lat = c(-0.120299, 51.522388),
    stratify_by = "cohort",
    distance = "travel_km",
    group_cols = c("County", "Region", "Country"),
    home_location = "all_in_cohort"
    )

  # Types
  testthat::expect_type(gla, "list")
  testthat::expect_s3_class(gla$df, "data.frame")
  testthat::expect_type(gla$list_maps, "list")
  testthat::expect_s3_class(gla$imd_bar_plot, "ggplot")
  testthat::expect_s3_class(gla$distance_density, "ggplot")
  testthat::expect_s3_class(gla$distance_boxplot, "ggplot")
  testthat::expect_s3_class(gla$tbl_n, "data.frame")

  # number of cohorts
  testthat::expect_equal(length(gla$list_maps), length(cohort_spec))

  testthat::expect_true(dim(gla$df)[1] == length(unique(dummy_loc$df_loc$District)) & dim(gla$df)[2] == 5)

  #testthat::expect_true(all(gla$df == df_test) & identical(gla$df, df_test))

})

test_that("no data", {

  gla <- driveanalytics::gen_location_analysis(NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL)

  # Types
  testthat::expect_type(gla, "list")
  testthat::expect_null(gla$df)
  testthat::expect_null(gla$list_maps)
  testthat::expect_null(gla$imd_bar_plot)
  testthat::expect_null(gla$distance_density)
  testthat::expect_null(gla$distance_boxplot)
  testthat::expect_null(gla$tbl_n)

  # number of cohorts
  testthat::expect_equal(length(gla$list_maps), 0)

})

test_that("shapefile null", {

  gla <- driveanalytics::gen_location_analysis(
    df_rdv = dummy_loc$df_loc,
    shp_path = NULL,
    shp_colname = "LAD20NM",
    df_colname = "District",
    rank_colname = "imd_rank",
    decile_colname = "Index.of.Multiple.Deprivation..IMD..Decile",
    destination_long_lat = c(-0.120299, 51.522388),
    stratify_by = "cohort",
    distance = "travel_km",
    group_cols = c("County", "Region", "Country"),
    home_location = "all_in_cohort"
  )

  # Types
  testthat::expect_type(gla, "list")
  testthat::expect_null(gla$df)
  testthat::expect_null(gla$list_maps)
  testthat::expect_null(gla$imd_bar_plot)
  testthat::expect_null(gla$distance_density)
  testthat::expect_null(gla$distance_boxplot)
  testthat::expect_null(gla$tbl_n)

  # number of cohorts
  testthat::expect_equal(length(gla$list_maps), 0)

})

test_that("shapefile doesn't exist", {

  gla <- driveanalytics::gen_location_analysis(
    df_rdv = dummy_loc$df_loc,
    shp_path = "Z:\\distr.shp",
    shp_colname = "LAD20NM",
    df_colname = "District",
    rank_colname = "imd_rank",
    decile_colname = "Index.of.Multiple.Deprivation..IMD..Decile",
    destination_long_lat = c(-0.120299, 51.522388),
    stratify_by = "cohort",
    distance = "travel_km",
    group_cols = c("County", "Region", "Country"),
    home_location = "all_in_cohort"
  )

  # Types
  testthat::expect_type(gla, "list")
  testthat::expect_null(gla$df)
  testthat::expect_null(gla$list_maps)
  testthat::expect_null(gla$imd_bar_plot)
  testthat::expect_null(gla$distance_density)
  testthat::expect_null(gla$distance_boxplot)
  testthat::expect_null(gla$tbl_n)

  # number of cohorts
  testthat::expect_equal(length(gla$list_maps), 0)

})

test_that("shiny app", {

  app_ui <- shiny::fluidPage(driveanalytics::gen_location_analysis_ui("test_loc"))
  app_server <- function(input, output, session) {
    df_loc_in <- shiny::reactive({dummy_loc$df_loc})
    driveanalytics::gen_location_analysis_server(
      id = "test_loc",
      df_loc_server = df_loc_in,
      shp_path = dummy_loc$shp_path,
      shp_colname = "LAD20NM",
      df_colname = "District",
      rank_colname = "imd_rank",
      decile_colname ="Index.of.Multiple.Deprivation..IMD..Decile",
      destination_long_lat = c(-0.120299, 51.522388),  # GOSH
      group_cols = c("County",	"Region",	"Country"),
      stratify_by = "cohort"
    )
  }

  app <- shiny::shinyApp(ui = app_ui, server = app_server)
  testthat::expect_equal(class(app), "shiny.appobj")
  # shiny::runApp(app)

})

test_that("report generation as HTML", {
  testthat::skip(message = "Not working on Gitlab CI")
  rmd_file <- system.file("rmd", "gen_location_analysis-report.Rmd", package = "driveanalytics")
  output_dir <- withr::local_tempdir(clean = FALSE)

  render_params <- list(
    df_rdv = dummy_loc$df_loc,
    shp_path = dummy_loc$shp_path,
    shp_colname = "LAD20NM",
    df_colname = "District",
    rank_colname = "imd_rank",
    decile_colname ="Index.of.Multiple.Deprivation..IMD..Decile",
    destination_long_lat = c(-0.120299, 51.522388),  # GOSH
    group_cols = c("County",	"Region",	"Country"),
    stratify_by = "cohort"
  )

  # HTML report
  result_html <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params
  )

  # PDF report
  result_pdf <- rmarkdown::render(
    input = rmd_file,
    output_file = output_dir,
    params = render_params,
    output_format = rmarkdown::pdf_document(includes = list(in_header = tex_preamble))
  )
  testthat::expect_true(file.exists(result_html))
  testthat::expect_true(file.exists(result_pdf))
})

test_that("shiny server", {

  # To test shiny, we can test the server functionality, but not the UI yet
  testthat::skip(message = "Error in `eval(code, test_env)`: object 'df_loc' not found")
  distance <- "travel_km"
  home_location <- "all_in_cohort"
  subdivision <- "District"

  # Load the 'true' values from the raw function
  gla_extern <- driveanalytics::gen_location_analysis(
    df_rdv = df_loc,
    shp_path = shp_path,
    shp_colname = "LAD20NM",
    df_colname = "District",
    rank_colname = "imd_rank",
    decile_colname ="Index.of.Multiple.Deprivation..IMD..Decile",
    destination_long_lat = c(-0.120299, 51.522388),  # GOSH
    stratify_by = "cohort",
    distance = distance,
    group_cols = c("County",	"Region",	"Country"),
    home_location = home_location
  )
  # Test the shiny server module
  shiny::testServer(
    driveanalytics::gen_location_analysis_server,
    args = list(
      id = "test",
      df_loc_server = shiny::reactive({df_loc}),
      shp_path = shp_path
    ),
    {
      session$setInputs(homelocation = home_location, distance = distance, subdivision = "District")
      testthat::expect_equal(gla(), gla_extern)
    }
  )

})
