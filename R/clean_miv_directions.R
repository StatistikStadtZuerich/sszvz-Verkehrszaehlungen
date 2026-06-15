#' clean_miv_directions
#'
#' Function to correct for cases, when miv dir_id's have switched but kept the same direction name (while)
#' also changing kname and knr
#'
#' @param mivs dataset for mivs, tibble
#'
#' @returns tibble with cleaned directions for locations where the old directions have been removed (in memory)
#' @keywords internal
#'
#' @examples \dontrun{
#' miv_cleaned <- clean_miv_directions(mivs)
#' }
clean_miv_directions <- function(mivs) {
  # the directions that we want to get rid of are those that have more than two
  # entries when we only keep distinct values of loc_id, loc_name, dir_id, and dir_name.
  # we want to only keep two combinations per loc_id, and we want to keep those 
  # that occur later in time --> keep the last ones after arranging by time
  loc_dir <- mivs  |> 
    arrange(timestamp) |> 
    select(loc_id, loc_name, dir_id, dir_name)  |> 
    distinct()  |> 
    arrange(loc_id)
  # keep only the last entries per group
  loc_last <- loc_dir  |> 
    summarise(dir_id = last(dir_id), .by = c("loc_id", "dir_name")) 

  # keep the data of these dir_ids
  mivs |> 
    filter(dir_id %in% loc_last$dir_id)
}
