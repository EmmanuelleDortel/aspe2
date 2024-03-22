#' Ajouter à un dataframe les observations manquantes
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_name
#' @importFrom tidyr crossing
#'
#' @param df data frame
#' @param var_id variable(s) identifiant les points de prélèvement
#' @param var_taxon variable identifiant les espèces
#' @param var_obs variable identifiant les observations
#' @param vec_obs vecteur avec les observations complète
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_ope_date() %>%
#' filter(annee %in% 2010:2020, !annee == 2015)
#' df_na <- mef_ajouter_na(df,
#' var_id = pop_id,
#' var_taxon =esp_code_alternatif,
#' var_obs = annee)
#' sort(unique(df_na$annee))
#' df_na <- mef_ajouter_na(df,
#' var_id = pop_id,
#' var_taxon =esp_code_alternatif,
#' var_obs = annee,
#' vec_obs = 2010:2020)
#' sort(unique(df_na$annee))
#' }
mef_ajouter_na <- function(df, var_id, var_taxon, var_obs, vec_obs=NULL) {
  var_id <- enquo(var_id)
  var_taxon <- enquo(var_taxon)
  var_obs <- enquo(var_obs)
  if (is.null(vec_obs)) { vec_obs <- pull(df, !!var_obs) %>% unique() }
  names_na <- colnames(select(df, -!!var_id, -!!var_taxon, -!!var_obs))
  #-----------------------------------------------------------------------------
  df_na <- select(df, !!var_id, !!var_taxon) %>% distinct() %>%
    full_join(crossing(unique(select(df, !!var_id)), !!quo_name(var_obs) := vec_obs), relationship = "many-to-many") %>%
    anti_join(select(df, !!var_id, !!var_taxon, !!var_obs) %>% distinct()) %>%
    bind_cols(matrix(NA, ncol=length(names_na), dimnames=list(1,names_na))) %>%
    select(colnames(df))
  #-----------------------------------------------------------------------------
  return(rbind(df,df_na))
}
