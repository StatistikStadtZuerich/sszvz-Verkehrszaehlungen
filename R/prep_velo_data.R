#' prep_velo_data
#'
#' wraps around prep_data, providing the correct velo locations, and runs the additional
#' cleaning step that is only necessary for bikes but not miv
#'
#' @param velo_all tibble with velo data
#'
#' @returns list of tibbles/lists with aggregated data
#'
#' @export
#' @examples \dontrun{
#' prep_velo_data(read_velo_data())
#' }
prep_velo_data <- function(velo_all) {
  velo_all |>
    # correct locations with direction switches
    clean_velo_directions() |>
    prep_data(read_velo_standorte())
}
