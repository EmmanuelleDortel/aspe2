#' Ajouter à un dataframe les coordonnées géographiques et le code EPSG
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_name
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
#' mef_ajouter_coords(var_id = pop_id)
#' }
mef_ajouter_coords <- function(df, var_id) {
  var_id <- enquo(var_id)
  if (quo_name(var_id) == "sta_id") {
    df <- df %>%
      left_join(select(station, !!var_id, sta_coordonnees_x, sta_coordonnees_y, typ_id = sta_typ_id)) %>%
      left_join(select(ref_type_projection, typ_id, typ_code_epsg)) %>%
      select(-typ_id)
  }
  if (quo_name(var_id) == "pop_id") {
    df <- df %>%
      left_join(select(point_prelevement, !!var_id, pop_coordonnees_x, pop_coordonnees_y, typ_id = pop_typ_id)) %>%
      left_join(select(ref_type_projection, typ_id, typ_code_epsg)) %>%
      select(-typ_id)
  }
  return(df)
}
