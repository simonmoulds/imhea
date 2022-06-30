#' imhea: handle hydrometeorological data from the iMHEA network.
#'
#' @aliases NULL imhea-package
#' @importFrom dplyr dplyr_row_slice dplyr_col_modify dplyr_reconstruct
#'
#' @examples
#' \dontrun{
#' iMHEA_Catchment_AREA = read_csv(
#'   system.file("extdata", "iMHEA_Data_Areas.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' q1_raw <- read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_HI_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' p1_raw = read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_PO_01_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' p2_raw = read_csv(
#'   system.file("extdata", "LLO/iMHEA_LLO_01_PO_02_raw.csv", package = "imhea"),
#'   show_col_types = FALSE
#' )
#'
#' # Convert precipitation data to rain_gauge objects:
#' p1 <- p1_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_01", event_units = "mm")
#' p2 <- p2_raw %>% tipping_bucket_rain_gauge(id = "LLO_01_P0_02", event_units = "mm")
#'
#' # Convert streamflow data to stream_gauge object:
#' q1 <-
#'   q1_raw %>%
#'   stream_gauge(
#'     id = "LLO_01_HI_01",
#'     discharge_units = "l/s",
#'     level_column = "Level cm",
#'     level_units = "cm"
#'   )
#'
#' # Define the catchment ID, and retrieve catchment area:
#' catchment_id <- "LLO_01"
#' catchment_area <-
#'   iMHEA_Catchment_AREA %>%
#'   filter(Catchment %in% catchment_id) %>%
#'   `[`(, 2, drop=T)
#'
#' # Create a catchment object (this takes a few minutes):
#' x <- catchment(q1, p1, p2, id = catchment_id, area = set_units(catchment_area, km^2))
#'
#' # Have a look at some of the attributes
#' daily(x)
#' monthly(x)
#' area(x)
#' }
"_PACKAGE"
