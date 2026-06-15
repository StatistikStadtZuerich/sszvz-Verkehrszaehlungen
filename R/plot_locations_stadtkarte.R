#' plot_locations_stadtkarte
#'
#' plot locations with map tiles in the background
#'
#' @param df_location locations to be plotted as circles, needs loc_id and tooltip columns
#' @param stadtkreise sf/tibble with stadtkreise, used for limiting the map and plotting the border
#' @param duplicates_possible default FALSE, if TRUE takes the last of several entries for the same location (based on loc_id and column "von")
#' @param raster_color default "color", alternative can be "grey" to determine which background map tiles are used
#'
#' @returns maplibregl object
#' @export
#' @examples \dontrun{
#' plot_locations_stadtkarte(df_loc, read_common_shapes()$kreise, duplicates_possible = TRUE)
#' }
#'
plot_locations_stadtkarte <- function(df_location, stadtkreise, duplicates_possible = FALSE, raster_color = c("color", "grey")) {
  # check input
  stopifnot(all(c("loc_id", "tooltip") %in% names(df_location)))
  stopifnot(any(class(df_location) == "sf"))
  raster_color <- match.arg(raster_color)

  # with velo locations, it is possible that with the replacement of the actual
  # counters, the exact location has changed slightly --> only keep and plot the
  # most recent one; requires the column named "von"
  if (duplicates_possible) {
    stopifnot("von" %in% names(df_location))
    df_location <- df_location |>
      arrange(von) |>
      group_by(loc_id) |>
      slice_tail(n = 1)
  }

  # prepare parameters: colors
  zueriblau <- get_zuericolors("qual6", 1)
  hover_color <- get_zuericolors("qual6a", 2)
  outline_color <- get_zuericolors("seq6gry", 5)
  background_color <- "#f4f4f4" # DESI color for cards
  cluster_colors <- get_zuericolors("qual6b", 6:4)

  # prepare mask to hide everything outside the city (the tiles cover the entire canton of Zurich)
  sf_use_s2(FALSE)
  bbox <- st_as_sfc(st_bbox(c(xmin = 5, xmax = 15, ymin = 44, ymax = 50), crs = 4326))
  stadtgrenze <- stadtkreise |>
    st_make_valid() |>
    st_transform(2056) |>
    st_union() |>
    st_transform(4326)
  neg_stadtgrenze <- st_difference(bbox, stadtgrenze)
  bbox_stadt <- st_bbox(stadtgrenze)
  buffer <- 0.05
  sf_use_s2(TRUE)

  if (raster_color == "color") {
    # prepare tiles for map (in color)
    wms_base <- "https://www.ogd.stadt-zuerich.ch/mapproxy/service?"

    wms_tile_template <- paste0(
      wms_base,
      "SERVICE=WMS",
      "&REQUEST=GetMap",
      "&VERSION=1.1.1",
      "&LAYERS=Basiskarte_Zuerich_Raster",
      "&STYLES=",
      "&FORMAT=image/png",
      "&TRANSPARENT=FALSE",
      "&SRS=EPSG:3857",
      "&BBOX={bbox-epsg-3857}",
      "&WIDTH=256",
      "&HEIGHT=256"
    )
  } else if (raster_color == "grey") {
    # prepare for grey tiles
    wms_base <- "https://www.ogc.stadt-zuerich.ch/mapproxy/service?"

    wms_tile_template <- paste0(
      wms_base,
      "SERVICE=WMS",
      "&REQUEST=GetMap",
      "&VERSION=1.1.1",
      "&LAYERS=basiskarte_zuerich_grau",
      "&STYLES=",
      "&FORMAT=image/png",
      "&TRANSPARENT=FALSE",
      "&SRS=EPSG:3857",
      "&BBOX={bbox-epsg-3857}",
      "&WIDTH=256",
      "&HEIGHT=256"
    )
  }

  # prepare empty style to start with empty map (sources must be {} not [])
  empty_style <- jsonlite::fromJSON(
    '{
    "version": 8,
    "sources": {},
    "layers": [
      { "id": "background", "type": "background",
        "paint": { "background-color": "#ffffff" } }
    ]
  }',
    simplifyVector = FALSE
  )

  # create map
  maplibre(
    style = empty_style,
    center = c(8.5417, 47.3769),
    zoom = 11,
    minZoom = 10,
    maxZoom = 22,
    maxBounds = list(
      c(bbox_stadt[["xmin"]] - buffer, bbox_stadt[["ymin"]] - buffer),
      c(bbox_stadt[["xmax"]] + buffer, bbox_stadt[["ymax"]] + buffer)
    )
  ) |>
    add_raster_source(
      id = "zh_basemap",
      tiles = wms_tile_template,
      tileSize = 256,
      maxZoom = 22
    ) |>
    add_raster_layer(
      id = "zh_basemap_layer",
      source = "zh_basemap",
      raster_opacity = 0.8
    ) |>
    add_source(
      id = "mask",
      data = st_sf(neg_stadtgrenze)
    ) |>
    add_fill_layer(
      id = "mask-fill",
      source = "mask",
      fill_color = background_color,
      fill_opacity = 1,
      fill_outline_color = outline_color,
    ) |>
    add_circle_layer(
      id = "locations",
      source = df_location,
      circle_color = zueriblau,
      circle_radius = 10,
      circle_opacity = 0.9,
      tooltip = "tooltip",
      hover_options = list(
        circle_radius = 12,
        circle_color = hover_color
      ),
      cluster_options = cluster_options(
        max_zoom = 15,
        color_stops = cluster_colors,
        text_color = "black"
      )
    ) |>
    add_scale_control() |>
    add_navigation_control(show_compass = FALSE)
}
