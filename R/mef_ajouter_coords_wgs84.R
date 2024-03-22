#' Ajouter à un dataframe les coordonnées géographiques WGS84
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_name
#' @importFrom sf st_as_sf st_transform st_coordinates
#' @importFrom stats na.omit
#'
#' @param df dataframe
#' @param var_id variable identifiant le site de prélèvement (sta_id ou pop_id)
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_coords_wgs84(var_id = pop_id)
#' }
mef_ajouter_coords_wgs84 <- function(df, var_id) {
  var_id <- enquo(var_id)
  if (quo_name(var_id) == "sta_id") {
    coord <- select(df, !!var_id) %>% na.omit() %>% distinct() %>%
      left_join(select(station, !!var_id, X = sta_coordonnees_x, Y = sta_coordonnees_y, typ_id = sta_typ_id))
  }
  if (quo_name(var_id) == "pop_id") {
    coord <- select(df, !!var_id) %>% distinct() %>%
      left_join(select(point_prelevement, !!var_id, X = pop_coordonnees_x, Y = pop_coordonnees_y, typ_id = pop_typ_id))
  }
  coord <- left_join(coord, select(ref_type_projection, typ_id, typ_code_epsg))
  coord_wgs84 <- do.call("rbind", lapply(unique(coord$typ_code_epsg), function(i)
    coord %>% filter(typ_code_epsg == i) %>%
      st_as_sf(coords = c("X","Y"), crs = i) %>%
      st_transform(crs = 4326) %>%
      mutate(X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2]) %>%
      data.frame() %>%
      select(-geometry, -typ_id, -typ_code_epsg)
  ))
  df <- left_join(df, coord_wgs84)
  return(df)
}
