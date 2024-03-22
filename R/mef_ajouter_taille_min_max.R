#' Ajouter les tailles minimales et maximales des lots
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#'
#' @param df data frame (doit contenir les champs lop_id, tyl_id)
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_taille_min_max()
#' }
mef_ajouter_taille_min_max <- function(df) {
  #-----------------------------------------------------------------------------
  # Add individual measurements
  df_mei <- select(df, lop_id, tlo_id, tyl_id) %>%
    left_join(rename(mesure_individuelle, lop_id = mei_lop_id))
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual sizes
  df_tai <- filter(df_mei, !is.na(mei_taille)) %>%
    summarise(taille_mini = min(mei_taille), taille_maxi = max(mei_taille), .by = lop_id)
  #-----------------------------------------------------------------------------
  # Add minimum and maximum batch sizes
  df <- left_join(df, df_tai) %>%
    left_join(select(lot_poissons, lop_id, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi)) %>%
    mutate(lop_longueur_specimens_taille_mini = ifelse(tyl_id %in% c(1,4), taille_mini, lop_longueur_specimens_taille_mini),
           lop_longueur_specimens_taille_maxi = ifelse(tyl_id %in% c(1,4), taille_maxi, lop_longueur_specimens_taille_maxi),
           lop_longueur_specimens_taille_mini = ifelse(tyl_id %in% c(2,3), ifelse(is.na(lop_longueur_specimens_taille_mini), taille_mini, lop_longueur_specimens_taille_mini),lop_longueur_specimens_taille_mini),
           lop_longueur_specimens_taille_maxi = ifelse(tyl_id %in% c(2,3), ifelse(is.na(lop_longueur_specimens_taille_maxi), taille_maxi, lop_longueur_specimens_taille_maxi),lop_longueur_specimens_taille_maxi)) %>%
    select(-taille_mini, -taille_maxi)
  #-----------------------------------------------------------------------------
  return(df)
}
