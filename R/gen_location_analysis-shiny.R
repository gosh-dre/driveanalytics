#' Shiny UI module for the Simple location data analysis in any generic RDV
#'
#' Shiny UI function encapsulated by namespace 'id'
#'
#' @param id Character label of the namespace for the module instance.
#'
#' @return Shiny taglist for the UI
#'
#' @family gen_location_analysis
#' @export
gen_location_analysis_ui <- function(id, title = NULL) {
  ns <- shiny::NS(id)
  shiny::tagList(
    bs4Dash::tabBox(
      title = title,
      width = 12,
      side = "right",
      shiny::tabPanel(
        title = "Maps",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::selectInput(
              ns("homelocation"),
              "Analysis based on",
              choices = c(
                "home location at cohort exit" = "max_in_cohort",
                "home location at cohort entry" = "min_in_cohort",
                "all home locations in cohort period" = "all_in_cohort"
              ),
              selected = "max_in_cohort"
            )
          ),
          shiny::column(
            4,
            shiny::selectInput(
              ns("subdivision"),
              "Subdivision in maps",
              #choices = c("Country", "District"),
              choices = "District",
              selected = "District"
            )
          ),
          shiny::column(
            4,
            shiny::selectInput(
              ns("distance"),
              "Plot distribution of",
              choices = c(
                "geographical distance between home postcode district and hospital (km)" = "geo_km",
                "travel distance between home postcode district and hospital (km)" = "travel_km",
                "travel time between home postcode district and hospital (mins)" = "travel_mins"
              ),
              selected = "travel_km"
            )
          )
        ),
        shiny::uiOutput(ns("maps")),
        shiny::br(),
        shiny::fluidRow(
          shiny::column(6, shiny::plotOutput(ns(
            "distance_boxplot"
          ))),
          shiny::column(6, shiny::plotOutput(ns(
            "distance_density"
          )))
        ),
        shiny::br(),
        shiny::tableOutput(ns("plot_n"))
      ),
      shiny::tabPanel(title = "Index of Multiple Deprivation",
                      shiny::fluidRow(
                        shiny::column(6, shiny::plotOutput(ns("imd_bar_plot"))),
                        shiny::column(6, shiny::plotOutput(ns(
                          "imd_violin_plot"
                        )))
                      )),
      shiny::tabPanel(title = "Table", DT::dataTableOutput(ns("table"))),
      picture.platform::ui_info_panel(
        system.file("about", "gen_location_analysis-text.html", package = "driveanalytics")
      )
    )
  )

}


#' Shiny server module for the Simple location data analysis in any generic RDV
#'
#' Shiny server function encapsulated by namespace 'id'
#'
#' @param id character label of the namespace for the module instance.
#' @param shp_path string; path to shapefile required for plotting.
#' @param shp_colname character; name of column to use in shapefile.
#' @param df_colname character; name of location column to use in RDV.
#' @param rank_colname character; name of index rank column in RDV.
#' @param decile_colname character; name of index decile column RDV.
#' @param destination_long_lat numeric vector; longitude and latitude of
#' destination point.
#' @param group_cols named character vector; names of grouping columns.
#' @param stratify_by character; name of column to stratify the analysis by.
#' @param df_loc_server reactive server function providing a dataframe.
#'
#' @return Shiny moduleServer function for the server functionality
#'
#' @family gen_location_analysis
#' @export
gen_location_analysis_server <- function(
  id,
  df_loc_server,
  shp_path = system.file("external_data", "districts.shp", package = "driveanalytics"),
  shp_colname = "LAD20NM",
  df_colname = "District",
  rank_colname = "imd_rank",
  decile_colname ="Index.of.Multiple.Deprivation..IMD..Decile",
  destination_long_lat = c(-0.120299, 51.522388),  # GOSH
  group_cols = c("County",	"Region",	"Country"),
  stratify_by = "cohort"
) {

  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- shiny::NS(id)

      gla <- shiny::reactive({

        if (input$subdivision == "District") {
          gen_location_analysis(df_loc_server(), shp_path, shp_colname, df_colname,
                                rank_colname, decile_colname,
                                destination_long_lat, stratify_by,
                                input$distance, group_cols,
                                input$homelocation)

        } #else if (input$subdivision == "Country") {
        #   gen_location_analysis(df_loc_server(), external_data_dir, countries_shp, "CTRY21NM", "Country", stratify_by)
        # }

      })

      output$maps <- shiny::renderUI({
        # attempt to put maps side by side
        shiny::fluidRow(lapply(gla()$list_maps, function(x) shiny::column(6, x)))
      })

      output$table <- DT::renderDataTable({
        gla()$df
      })

      output$imd_bar_plot <- shiny::renderPlot({
        gla()$imd_bar_plot
      })

      output$imd_violin_plot <- shiny::renderPlot({
        gla()$imd_violin_plot
      })

      output$distance_boxplot <- shiny::renderPlot({
        gla()$distance_boxplot
      })

      output$distance_density <- shiny::renderPlot({
        gla()$distance_density
      })

      output$plot_n <- shiny::renderTable({
        gla()$tbl_n
      })

    }
  )
}
