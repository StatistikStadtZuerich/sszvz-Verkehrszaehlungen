#' categorise_weekdays
#'
#' categorise days into workdays, Saturdays, Sundays/bank holidays
#'
#' @param df tibble to be categorised
#' @param feiertage ogd dataset
#' @param name_date_column name of the date/datetime column, default "timestamp"
#'
#' @return df with new column day_category
#' @keywords internal
#'
#' @examples \dontrun{
#' categorise_weekdays(df_data, df_feiertage)
#' }
categorise_weekdays <- function(df, feiertage, name_date_column = "timestamp") {
  # check necessary columns are available
  stopifnot(name_date_column %in% names(df))

  df |>
    mutate(
      weekday = wday(.data[[name_date_column]], week_start = 1),
      date_only = as_date(.data[[name_date_column]]),
      feiertage_sep = if_else(date_only %in% feiertage$start_date, TRUE, FALSE),
      day_category = case_when(
        date_only %in% feiertage$start_date | weekday == 7 ~ "Sonn-/Feiertag",
        weekday == 6 ~ "Samstag",
        .default = "Werktag"
      ),
      day_category = fct_relevel(day_category, c("Werktag", "Samstag", "Sonn-/Feiertag"))
    ) |>
    select(-weekday, -date_only)
}

#' add_tooltip_location
#'
#' Add tooltip to location dataframe
#'
#' @param df_location data.frame with locations, needs to have variables loc_name and loc_id
#'
#' @return dataframe with additional tooltip column
#' @keywords internal
#'
#' @examples \dontrun{
#' add_tooltip_location(df_location)
#' }
add_tooltip_location <- function(df_location) {
  # check necessary columns are available
  stopifnot("loc_id" %in% names(df_location))
  stopifnot("loc_name" %in% names(df_location))

  df_location |>
    mutate(
      tooltip = glue(
        "<div class='tooltip-container'>
            <div class='tooltip-title'>
              Zählstelle (ID)
            </div>
            <hr>
            <div class='tooltip-content'>
              <div class='tooltip-row'>
                  <span class='tooltip-value'>{loc_name} ({loc_id})</span>
              </div>
            </div>
        </div>"
      )
    )
}
