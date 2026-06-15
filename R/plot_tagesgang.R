#' plot_tagesgang
#'
#' Function to create a line plot for the "Tagesgang" (number of vehicles over the course of a day) based on the day category (Werktag/Samstag/Sonntag)
#'
#' @param df data frame with data to be plotted, needs timestamp_hours_only,
#' n_vehicles, day_category, loc_name and dir_name columns. if data_id and  tooltip columns are present, a tooltip is added with ggiraph
#' @param font_name optional name of the font to be used in theme; the name used for registering the font for ggplot (with sysfonts::font_add) and/or ggiraph (with systemfonts::register_font)
#' @param vehicle_type optional, specifies bike/car to be used in axis label, must be one of "Velos" / "Autos" / "Fahrzeuge", default "Velos"
#'
#' @returns ggplot object
#'
#' @export
#'
#' @examples \dontrun{
#' plot_tagesgang(df_tagesgang)
#' }
plot_tagesgang <- function(df, font_name = "", vehicle_type = c("Velos", "Autos", "Fahrzeuge")) {
  # check input
  stopifnot(all(
    c("timestamp_hours_only", "n_vehicles", "day_category", "loc_name", "dir_name") %in% names(df)
  ))
  vehicle_type <- match.arg(vehicle_type)

  base_plot <- df |>
    ggplot(aes(
      x = timestamp_hours_only, y = n_vehicles, color = day_category, group = day_category
    )) +
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
    scale_color_manual(values = get_zuericolors("qual6")) +
    scale_x_time(
      breaks = hms(hours = c(0, 6, 12, 18, 24)),
      labels = scales::label_time(format = "%H:00"),
      limits = hms(hours = c(0, 24), minutes = c(0, 5))
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(
      title = "Tagesgang",
      subtitle = glue("Zählstelle {unique(df$loc_name)} Richtung {unique(df$dir_name)}"),
      caption = "Daten der letzten 365 Tage",
      x = "Tageszeit",
      y = glue("Anzahl {vehicle_type} \npro Stunde"),
      color = "Wochentag"
    ) +
    ssz_theme(base_family = font_name)
}
