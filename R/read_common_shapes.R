#' read_common_shapes
#'
#' function to read the commonly used shapes
#'
#' @return named list of shapes for stadtkreise ("kreise"), stadtquartiere ("quartiere"), and the lake
#'
#' @export
#'
#' @examples read_common_shapes()
read_common_shapes <- function() {
  links_geojsons <- c(
    "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Stadtkreise?service=WFS&version=1.1.0&request=GetFeature&outputFormat=application/json&typename=adm_stadtkreise_a",
    "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Statistische_Quartiere?service=WFS&version=1.1.0&request=GetFeature&outputFormat=application/json&typename=adm_statistische_quartiere_v",
    "https://raw.githubusercontent.com/StatistikStadtZuerich/sszvis/master/geodata/lakezurich.geojson"
  )
  names_shp <- c("kreise", "quartiere", "see")

  purrr::map(links_geojsons, \(x) read_sf(x)) |>
    set_names(names_shp)
}
