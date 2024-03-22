#' Ajouter les poids des lots à la passerelle
#'
#' @import magrittr
#' @importFrom dplyr left_join select
#'
#' @param df data frame
#'
#' @return df
#' @export
#' 
#' @examples
#' \dontrun{
#' passerelle <- mef_creer_passerelle() %>%
#' mef_ajouter_poids()
#' }
mef_ajouter_poids <- function(df) {
  df <- df %>% left_join(select(lot_poissons, lop_id, lop_poids, lop_poids_estime))
  return(df)
}
