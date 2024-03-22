#' Filtrer les séries temporelles (prélèvement, temps)
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_is_null quo_name
#'
#' @param df data frame
#' @param df_obs data frame contenant les critères de filtre
#' @param var_id variable(s) identifiant les prélèvements
#' @param var_id_sup variable(s) additionnelle identifiant les prélèvements
#' @param var_pro variable identifiant le protocole de pêche
#' @param min_obs numerique définissant le nombre minimal d'observation par var_id
#' @param max_na_cons numerique définissant le nombre maximal de valeurs manquantes consécutives par var_id
#' @param max_pro numerique définissant le nombre maximal de protocoles de pêche par var_id
#' @param max_chg numerique définissant le nombre maximal de changement de protocole de pêche par var_id
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_ope_date() %>%
#' mef_ajouter_ope_saison() %>%
#' mef_ajouter_type_protocole() %>%
#' filter(pop_id %in% c(10,2271,3969,7405,12316,35119,71350,79638,86743)) %>%
#' filter(annee %in% 2010:2020)
#' df_obs <- def_compter_obs(df,var_id=c(pop_id,saison),var_tmp=annee,var_pro=pro_libelle)
#' df_filtre <- mef_filtrer_obs(df,df_obs,
#' var_id=pop_id,
#' var_id_sup=saison,
#' var_pro=pro_libelle,
#' min_obs = 7,
#' max_na_cons = 3,
#' max_pro = 2,
#' max_chg = 1)
#' df_filtre <- mef_filtrer_obs(df,df_obs,
#' var_id=c(pop_id,saison),
#' var_pro=pro_libelle,
#' min_obs = 7,
#' max_na_cons = 3,
#' max_pro = 2,
#' max_chg = 1)
#' df_filtre <- mef_filtrer_obs(df,df_obs,
#' var_id=pop_id,
#' min_obs = 7,
#' max_na_cons = 3)
#' }
mef_filtrer_obs <- function(df,df_obs,var_id,var_id_sup=NULL,var_pro=NULL,min_obs,max_na_cons=NULL,max_pro=NULL,max_chg=NULL) {
  var_id <- enquo(var_id)
  var_id_sup <- enquo(var_id_sup)
  var_pro <- enquo(var_pro)
  #-----------------------------------------------------------------------------
  # Filter df_obs according to observation criteria
  df_obs <- filter(df_obs, nb_obs >= min_obs)
  if (!is.null(max_na_cons)) {
    df_obs <- filter(df_obs, nb_na_cons <= max_na_cons)
  }
  if (!is.null(max_pro)) {
    df_obs <- filter(df_obs, nb_pro <= max_pro)
  }
  if (!is.null(max_chg)) {
    df_obs <- filter(df_obs, nb_chg <= max_chg)
  }
  #-----------------------------------------------------------------------------
  # Select a single temporal series by var_id
  df_obs <- df_obs %>% inner_join(summarise(df_obs, nb_obs = max(nb_obs), .by=!!var_id))
  df_obs <- df_obs %>% inner_join(summarise(df_obs, nb_na_cons = min(nb_na_cons), .by=!!var_id)) %>%
    mutate(rowid = row_number())
  df_obs <- df_obs %>% inner_join(summarise(df_obs, rowid = min(rowid), .by = !!var_id))
  #-----------------------------------------------------------------------------
  # Filter df according to criteria of df_obs
  if (quo_is_null(var_pro)) {
    df <- df %>% inner_join(select(df_obs, !!var_id, !!var_id_sup))
  } else {
    df <- do.call("rbind", lapply(1:nrow(df_obs), function(i) {
      pro <- unlist(strsplit(pull(df_obs[i,], !!var_pro),","))
      df_i <- df %>% inner_join(select(df_obs[i,], !!var_id, !!var_id_sup)) %>%
        filter_at(quo_name(var_pro), all_vars(. %in% pro))
      return(df_i)
    }))
  }
  #-----------------------------------------------------------------------------
  return(df)
}
