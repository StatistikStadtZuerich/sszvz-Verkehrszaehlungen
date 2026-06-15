#' plot_locations_stadtkreise
#'
#' Function to create an (interactive) ggplot with locations of counting stations, with Stadtkreise as background
#'
#' @param df tibble with geo-information with points to plot (needs column named loc_id)
#' @param common_shapes list with "see" and "kreise"
#' @param duplicates_possible default FALSE, if TRUE takes the last of several entries for the same location (based on loc_id)
#'
#' @returns ggplot object
#'
#' @export
#'
#' @examples \dontrun{
#' plot_locations_stadtkreise(df_loc, common_shapes)
#' }
plot_locations_stadtkreise <- function(df, common_shapes, duplicates_possible = FALSE) {
  # check input
  stopifnot("loc_id" %in% names(df))
  stopifnot(any(class(df) == "sf"))
  stopifnot(all(c("see", "kreise") %in% names(common_shapes)))

  # colors
  colors_see <- get_zuericolors(palette = "seq6gry", nth = c(1, 2))
  zueriblau <- get_zuericolors("qual6", 1)

  # with velo locations, it is possible that with the replacement of the actual
  # counters, the exact location has changed slightly --> only keep and plot the
  # most recent one
  if (duplicates_possible) {
    df <- df |>
      arrange(von) |>
      group_by(loc_id) |>
      slice_tail(n = 1)
  }

  base_plot <- ggplot() +
    geom_sf(
      data = common_shapes$kreise,
      color = "white",
      fill = colors_see[2],
      linewidth = 0.75
    ) +
    geom_sf_pattern(
      data = common_shapes$see,
      color = NA,
      fill = colors_see[1],
      pattern_fill = colors_see[2],
      pattern_color = colors_see[2],
      pattern_angle = 45,
      pattern_density = 0.07,
      pattern_spacing = 0.01
    )

  # if tooltip and data_id columns are present, add tooltip with ggiraph
  if ("tooltip" %in% names(df)) {
    base_plot <- base_plot +
      geom_sf_interactive(
        data = df,
        aes(tooltip = tooltip, data_id = loc_id),
        color = zueriblau
      )
  } else {
    # otherwise just plot points without tooltip
    base_plot <- base_plot +
      geom_sf(
        data = df,
        color = zueriblau
      )
  }

  # continue with generic plot stuff
  base_plot +
    coord_sf() +
    ssz_theme_void(
      base_family = "Helv",
      base_size = 12
    ) +
    theme(legend.title = element_text(
      color = "#020304",
      size = rel(1),
      face = "bold"
    ))
}
