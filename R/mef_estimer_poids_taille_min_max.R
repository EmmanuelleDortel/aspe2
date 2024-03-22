#' Estimer les poids minimales et maximales des lots à partir de la relation taille-poids
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#'
#' @param df data frame (doit contenir les champs tlo_id, lop_longueur_specimens_taille_mini et lop_longueur_specimens_taille_maxi)
#' @param var_taxon variable identifiant les espèces
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_taille_min_max() %>%
#' mef_estimer_poids_taille_min_max()
#' }
mef_estimer_poids_taille_min_max <- function(df, var_taxon=esp_code_alternatif) {
  var_taxon <- enquo(var_taxon)
  data_taxa
  #-----------------------------------------------------------------------------
  df <- df %>%
    left_join(select(data_taxa, !!var_taxon, tlo_id, a, b)) %>%
    mutate(lop_poids_taille_mini = a * (lop_longueur_specimens_taille_mini^b)) %>%
    mutate(lop_poids_taille_maxi = a * (lop_longueur_specimens_taille_maxi^b)) %>%
    select(-a, -b)
  #-----------------------------------------------------------------------------
  return(df)
}
