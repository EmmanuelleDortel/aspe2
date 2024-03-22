#' Filtrer les points de prélèvements de chaque espèce en fonction de leur présence
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_name
#'
#' @param df data frame
#' @param var_id variable(s) identifiant les sites de prélèvement
#' @param var_taxon variable identifiant les espèces
#' @param var_obs variable contenant les presences/absence (numérique)
#'
#' @return df_filtre data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_passage() %>%
#' summarise(effectif = sum(lop_effectif),
#' .by = c(pop_id,ope_id,pas_numero,esp_code_alternatif)) %>%
#' mef_ajouter_absence(var_id = pop_id,
#' var_taxon = esp_code_alternatif,
#' var_abs = effectif,
#' var_obs= c(ope_id,pas_numero))
#' df_filtre <- df %>%
#' filter(pas_numero %in% 1) %>%
#' mef_supprimer_site_absence(var_id = pop_id,
#' var_taxon = esp_code_alternatif,
#' var_obs = effectif)
#' }
mef_filtrer_presence <- function(df, var_id, var_taxon, var_obs) {
  var_id <- enquo(var_id)
  var_taxon <- enquo(var_taxon)
  var_obs <- enquo(var_obs)
  #-----------------------------------------------------------------------------
  nbobs <- summarise(df, across(quo_name(var_obs), ~sum(.x, na.rm=T), .names = "n"), .by = c(!!var_id, !!var_taxon)) %>%
    filter(!n == 0) %>%
    select(-n)
  vec_taxon <- pull(nbobs, !!var_taxon) %>% unique()
  #-----------------------------------------------------------------------------
  df_filtre <- do.call("rbind", lapply(vec_taxon, function(i) {
    df %>% right_join(filter_at(nbobs, quo_name(var_taxon), all_vars(. %in% i)))
  }))
  #-----------------------------------------------------------------------------
  return(df_filtre)
}
