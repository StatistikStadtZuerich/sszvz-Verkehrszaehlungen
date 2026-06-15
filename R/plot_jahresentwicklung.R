#' plot_jahresentwicklung
#'
#' plot yearly development
#'
#' @param df df with number of vehicles per year (year, n_vehicles) and day_category as well as dir_name and loc_name for labels. If data_id and tooltip are present, a tooltip is added with ggiraph.
#' @param font_name optional name of the font to be used in theme; the name used for registering the font for ggplot (with sysfonts::font_add) and/or ggiraph (with systemfonts::register_font)
#' @param vehicle_type optional, specifies bike/car to be used in axis label, must be one of "Velos" / "Autos" / "Fahrzeuge", default "Velos"
#'
#' @return ggplot object
#'
#' @export
#'
#' @examples \dontrun{
#' plot_jahresentwicklung(df_jahre)
#' }
plot_jahresentwicklung <- function(df, font_name = "", vehicle_type = c("Velos", "Autos", "Fahrzeuge")) {
  stopifnot(all(
    c("jahr", "n_vehicles", "day_category", "loc_name", "dir_name") %in% names(df)
  ))
  vehicle_type <- match.arg(vehicle_type)

  base_plot <- df |>
    ggplot(aes(x = jahr, y = n_vehicles, color = day_category)) +
    geom_line(linewidth = 0.5)

  # make it interactive if the necessary columns are present
  if (all(c("data_id", "tooltip") %in% names(df))) {
    base_plot <- base_plot +
      geom_point_interactive(aes(data_id = data_id, tooltip = tooltip),
        shape = 20, size = 0.1, hover_nearest = TRUE
      )
  } else {
    base_plot <- base_plot +
      geom_point(shape = 20, size = 0.1)
  }

  base_plot +
    scale_x_continuous(breaks = function(x) {
      breaks <- scales::breaks_extended()(x) # pretty breaks as a start 
      breaks[breaks == floor(breaks)] # keep only integers
    }) +
    scale_y_continuous(labels = scales::label_number(), limits = c(0, NA)) +
    scale_color_manual(values = get_zuericolors("qual6")) +
    labs(
      title = "Entwicklung über die Jahre",
      subtitle = glue("Zählstelle {unique(df$loc_name)} Richtung {unique(df$dir_name)}"),
      x = "",
      y = glue("Mittlere Anzahl \n{vehicle_type} pro Tag"),
      color = "Wochentag"
    ) +
    ssz_theme(base_family = font_name)
}
