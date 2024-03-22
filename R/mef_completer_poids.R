#' Compléter le poids des lots à partir des mesures individuelles
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#'
#' @param df data frame
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_poids() %>%
#' mef_completer_poids()
#' }
mef_completer_poids <- function(df) {
  #-----------------------------------------------------------------------------
  df_mei <- select(df, lop_id, tyl_id, lop_effectif) %>%
    left_join(select(mesure_individuelle, lop_id = mei_lop_id, mei_taille, mei_poids, mei_poids_estime, mei_id)) %>%
    mutate(mei_poids = replace(mei_poids, mei_poids %in% c(0,1), NA),
           mei_poids_estime = replace(mei_poids_estime, mei_poids_estime %in% c(0,1), NA))
  #-----------------------------------------------------------------------------
  dech <- df_mei[(df_mei$tyl_id %in% 2:3 & !is.na(df_mei$mei_taille)),]
  if (nrow(dech) > 0) {
    ech <- aggregate(dech$mei_id, list("lop_id"=dech$lop_id,"eff"=dech$lop_effectif), function(i) length(unique(i)))
    ech$diff <- ech$eff - ech$x
  } else { ech <- setNames(data.frame(array(dim=c(0,4))),c("lop_id","eff","x","diff")) }
  #-----------------------------------------------------------------------------
  df_pds <- df_mei %>%
    filter(tyl_id %in% c(1,4)) %>%
    rbind(dech[dech$lop_id %in% ech$lop_id[ech$diff == 0],]) %>%
    summarise(across(c("mei_poids","mei_poids_estime"), ~sum(.x)), .by =lop_id)
  #-----------------------------------------------------------------------------
  df <- df %>% left_join(df_pds) %>%
    mutate(lop_poids = ifelse(is.na(mei_poids), lop_poids, mei_poids),
           lop_poids_estime = ifelse(is.na(mei_poids_estime), lop_poids_estime, mei_poids_estime)) %>%
    select(-mei_poids, -mei_poids_estime)
  #-----------------------------------------------------------------------------
  return(df)
}
