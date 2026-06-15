#' prep_miv_data
#'
#' wrapper around prep_data to provide appropriate location data
#'
#' @param miv_all tibble with miv data
#'
#' @returns list of tibbles/lists with aggregated data
#'
#' @export
#' @examples \dontrun{
#' prep_miv_data(read_miv_data())
#' }
prep_miv_data <- function(miv_all) {
  clean_miv_directions(miv_all) |> 
  prep_data(read_miv_standorte())
}
