#' clean_velo_directions
#'
#' Function to correct for cases, when bike counters have been replaced and the directions of
#' velo_in and velo_out have been switched
#'
#' @param velos dataset for velos, tibble
#'
#' @returns tibble with cleaned directions for locations where the directions have been switched (in memory)
#' @keywords internal
#'
#' @examples \dontrun{
#' velo_cleaned <- clean_velo_directions(velos)
#' }
clean_velo_directions <- function(velos) {
  # find locations where directions have been switched
  dir_check <- velos |>
    count(loc_id, dir_id, dir_name) |>
    count(loc_id) |>
    filter(n > 2)

  # clean those locations
  cleaned_locations <- map(
    dir_check$loc_id,
    \(x) clean_velo_directions_one_location(x, velos)
  ) |> list_rbind()

  # combine with rest of the data
  list_rbind(list(
    cleaned_locations,
    velos |>
      filter(!(loc_id %in% dir_check$loc_id))
  ))
}

#' clean_velo_directions_one_location
#'
#' helper function to actually switch directions for one location
#'
#' @param loc_id_selected string, selected loc_id
#' @param velos velo dataset
#'
#' @returns in-memory cleaned dataset for the selected loc_id only
#' @examples \dontrun{
#' clean_velo_directions_one_location("VZS_BINZ", velos)
#' }
clean_velo_directions_one_location <- function(loc_id_selected, velos) {
  current_loc <- velos |>
    filter(loc_id == loc_id_selected) |>
    arrange(timestamp)

  # check that the directions have only been switched and not changed otherwise
  stopifnot(length(unique(current_loc$dir_name)) == 2)

  # and make sure we only have two dir_ids (should always be velo_in and velo_out)
  stopifnot(length(unique(current_loc$dir_id)) == 2)

  # look at direction combinations: we want to keep the first two and change the last two
  dir_initial <- current_loc |>
    filter(timestamp == min(current_loc$timestamp)) |>
    select(dir_id, dir_name)

  stopifnot(nrow(dir_initial) == 2)

  dir_final <- current_loc |>
    filter(timestamp == max(current_loc$timestamp)) |>
    select(dir_id, dir_name)

  stopifnot(nrow(dir_final) == 2)

  stopifnot(!all(dir_initial == dir_final))

  current_loc_adjusted <- current_loc |>
    mutate(dir_id = case_when(
      dir_name == dir_initial$dir_name[[1]] ~ dir_initial$dir_id[[1]],
      dir_name == dir_initial$dir_name[[2]] ~ dir_initial$dir_id[[2]],
      .default = NA
    ))

  # there should not be any NAs
  stopifnot(sum(is.na(current_loc_adjusted$dir_id)) == 0)

  # there should only be 2 combinations now
  dir_combinations_adjusted <- current_loc_adjusted |>
    arrange(timestamp) |>
    select(dir_id, dir_name) |>
    distinct()
  stopifnot(nrow(dir_combinations_adjusted) == 2)

  # number of rows mustn't change
  stopifnot(nrow(current_loc) == nrow(current_loc_adjusted))

  return(current_loc_adjusted)
}
