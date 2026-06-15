#' read locations of MIV counting
#'
#' @returns geo-df with MIV locations
#' @export
#'
#' @examples
#' read_miv_standorte()
read_miv_standorte <- function() {
  link_geojson <- "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Standorte_der_Verkehrszaehlungen_MIV?service=WFS&version=1.1.0&request=GetFeature&outputFormat=application/json&typename=tbl_standort_zaehlung_miv_p"

  st_read(link_geojson) |>
    rename(
      loc_id = zsid,
      loc_name = zsname
    )
}

#' read currently active MIV locations
#'
#' @returns dataframe with active MIV locations
#' @export
#'
#' @examples
#' read_miv_loc_current()
read_miv_loc_current <- function() {
  read_miv_standorte() |>
    st_drop_geometry() |>
    # keep only locations that are currently still active
    filter(status == "aktiv") |>
    select(-status, -geometrie_gdo, -adresse, -ekoord, -nkoord, -suchen)
}
