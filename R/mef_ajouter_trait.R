#' Ajouter les traits des espèces à la passerelle
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#'
#' @param df data frame
#' @param var_taxon variable identifiant les espèces
#' @param trait vecteur de caractères listant les traits à ajouter (optionnel)
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_trait(var_taxon = esp_code_alternatif)
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_trait(var_taxon = esp_code_alternatif,
#' trait =c("esp_hab_tol","esp_qua_tol","esp_tmp_tol","esp_oxy_tol","esp_tox_tol","esp_aci_tol"))
#' }
mef_ajouter_trait  <- function(df, var_taxon, trait=c("Native_fr","Naturalise_fr","esp_tot_tol")) {
  var_taxon <- enquo(var_taxon)
  #-----------------------------------------------------------------------------
  df <- df %>% left_join(select(data_taxa, !!var_taxon, !!trait) %>% distinct())
  return(df)
}
