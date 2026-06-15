#' read_velo_standorte
#'
#' read velo/fuss zählungsstandorte from OGD
#'
#' @return sf tibble
#' @export
#' @examples \dontrun{
#' read_velo_standorte()
#' }
read_velo_standorte <- function() {
  link_geojson <- "https://www.ogd.stadt-zuerich.ch/wfs/geoportal/Standorte_der_automatischen_Fuss__und_Velozaehlungen?service=WFS&version=1.1.0&request=GetFeature&outputFormat=application/json&typename=view_eco_standorte"

  st_read(link_geojson) |>
    rename(
      loc_id = abkuerzung,
      loc_name = bezeichnung
    )
}

#' read_velo_loc_current
#'
#' read locations of velo counts that are currently active
#'
#' @returns tibble
#'
#' @export
#' @examples
#' read_velo_loc_current()
read_velo_loc_current <- function() {
  # also read the locations as they have correction factors
  df_loc <- read_velo_standorte() |>
    st_drop_geometry()
  active_velo_locs <- df_loc |>
    filter(str_starts(loc_id, "VZS")) |>
    group_by(loc_id) |>
    summarise(active = max(bis)) |>
    filter(is.na(active)) |>
    pull(loc_id)
  df_loc <- df_loc |>
    # keep only locations that are currently still active and only Velo
    filter(loc_id %in% active_velo_locs) |>
    filter(loc_id != "VZS_LANN") |> # for now, data error on midnight of 2017-09-08, two entries from two logging devices
    select(-von, -bis, -geometrie_gdo)
}
