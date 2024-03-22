#' Ajouter à un dataframe de présences les lignes des absences
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#' @importFrom tidyselect everything
#'
#' @param df data frame
#' @param var_id variable(s) identifiant les sites de prélèvement
#' @param var_taxon variable identifiant les espèces
#' @param var_abs variable(s) dont les absences sont à ajouter
#' @param var_obs variable(s) identifiant les observations
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_absence(var_id = pop_id,
#' var_taxon = esp_code_alternatif,
#' var_abs = lop_effectif)
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_poids() %>%
#' mef_ajouter_ope_date() %>%
#' summarise(effectif = sum(lop_effectif),
#' poids = sum(lop_poids),
#' .by=c(pop_id, annee, esp_code_alternatif)) %>%
#' mef_ajouter_absence(var_id = pop_id,
#' var_taxon = esp_code_alternatif,
#' var_abs = c(effectif,poids),
#' var_obs = annee)
#' }
mef_ajouter_absence <- function(df, var_id, var_taxon, var_abs, var_obs=ope_id) {
  var_id <- enquo(var_id)
  var_taxon <- enquo(var_taxon)
  var_obs <- enquo(var_obs)
  var_abs <- enquo(var_abs)
  names_abs <- select(df, !!var_abs) %>% colnames()
  #-----------------------------------------------------------------------------
  df_abs <- select(df, !!var_id, !!var_taxon) %>% distinct() %>%
    full_join(select(df, !!var_id, !!var_obs) %>% distinct(), relationship = "many-to-many") %>%
    anti_join(select(df, !!var_obs, !!var_taxon) %>% distinct()) %>%
    bind_cols(matrix(0, ncol=length(names_abs), dimnames=list(1,names_abs))) %>%
    left_join(select(df, -!!var_id, -!!var_taxon, -!!var_abs) %>%
                summarise(across(.cols = everything(), ~ifelse(max(n_distinct(.x)) > 1, NA, .x)), .by = !!var_obs) %>%
                distinct()) %>%
    select(colnames(df))
  #-----------------------------------------------------------------------------
  return(rbind(df,df_abs))
}
