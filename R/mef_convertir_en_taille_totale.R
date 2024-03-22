#' Convertir les longueurs à la fourche en longueurs totale
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#'
#' @param df data frame (doit contenir tlo_id)
#' @param var_tai variable(s) identifiant les tailles à convertir
#' @param var_taxon variable identifiant les espèces
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_mei() %>%
#' mef_convertir_en_taille_totale()
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_taille_min_max() %>%
#' mef_convertir_en_taille_totale(var_tai = c(lop_longueur_specimens_taille_mini,
#' lop_longueur_specimens_taille_maxi))
#' }
mef_convertir_en_taille_totale <- function(df, var_tai=mei_taille, var_taxon=esp_code_alternatif) {
  var_tai <- enquo(var_tai)
  var_taxon <- enquo(var_taxon)
  data_taxa
  #-----------------------------------------------------------------------------
  df_1 <- filter(df, tlo_id %in% 1) %>%
    left_join(select(data_taxa, !!var_taxon, tlo_id, LT_LF)) %>%
    mutate(across(!!var_tai, ~round(LT_LF*.x))) %>%
    mutate(tlo_id = 2) %>%
    select(-LT_LF)
  df <- filter(df, !lop_id %in% unique(df_1$lop_id)) %>%
    rbind(df_1)
  #-----------------------------------------------------------------------------
  return(df)
}
