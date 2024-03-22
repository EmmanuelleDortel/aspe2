#' Estimer les poids des lots à partir de la relation taille-poids
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#'
#' @param df data frame (doit contenir les champs lop_id, tlo_id, tyl_id, lop_effectif)
#' @param var_taxon variable identifiant les espèces
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_estimer_poids_taille()
#' }
mef_estimer_poids_taille <- function(df, var_taxon=esp_code_alternatif) {
  var_taxon <- enquo(var_taxon)
  data_taxa
  #-----------------------------------------------------------------------------
  # Function to reconstitution of headcounts by batch and size
  buid_size <- function(df_n,i) {
    d <- as.data.frame(proportions(xtabs(~mei_taille, df_n[df_n$lop_id == i,])))
    d$mei_effectif <- round_preserve_sum(unique(df_n[df_n$lop_id == i,"lop_effectif"]) * d$Freq)
    d <- d %>% mutate(mei_taille = as.numeric(as.vector(d$mei_taille))) %>%
      mutate(lop_id = i) %>%
      mutate(!!var_taxon := unique(pull(df_n[df_n$lop_id == i,], !!var_taxon))) %>%
      mutate(tlo_id = unique(df_n$tlo_id[df_n$lop_id == i])) %>%
      select(lop_id, !!var_taxon, mei_taille, tlo_id, mei_effectif)
    return(d)
  }
  #-----------------------------------------------------------------------------
  # Add individual measurements
  df_mei <- select(df, !!var_taxon, lop_id, tlo_id, tyl_id, lop_effectif) %>%
    left_join(rename(mesure_individuelle, lop_id = mei_lop_id))
  #-----------------------------------------------------------------------------
  # Difference between numbers of measured individuals and catched individuals (S/L and G)
  dech <- df_mei[(df_mei$tyl_id %in% 2:3 & !is.na(df_mei$mei_taille)),]
  if (nrow(dech) > 0) {
    ech <- aggregate(dech$mei_id, list("lop_id"=dech$lop_id,"eff"=dech$lop_effectif), function(i) length(unique(i)))
    ech$diff <- ech$eff - ech$x
  } else { ech <- setNames(data.frame(array(dim=c(0,4))),c("lop_id","eff","x","diff")) }
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual sizes
  df_tai <- df_mei %>%
    filter(tyl_id %in% c(1,4)) %>%
    rbind(dech[dech$lop_id %in% ech$lop_id[ech$diff == 0],]) %>%
    select(lop_id, !!var_taxon, mei_taille, tlo_id) %>%
    mutate(mei_effectif = 1)
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual sizes for representative samples
  df_2 <-  dech[(dech$lop_id %in% ech$lop_id[ech$diff > 0]),]
  if (nrow(df_2) > 0) {
    df_tai <- df_tai %>%
      rbind(do.call("rbind",lapply(unique(df_2$lop_id), function(i) buid_size(df_2,i))))
  }
  #-----------------------------------------------------------------------------
  # Estimate weight of batches from length-weight relationship
  df_tai <- df_tai %>%
    left_join(select(data_taxa, !!var_taxon, tlo_id, a, b)) %>%
    mutate(mei_poids_taille = mei_effectif * (a * (mei_taille^b))) %>%
    summarise(lop_poids_taille = sum(mei_poids_taille), .by = lop_id)
  #-----------------------------------------------------------------------------
  return(left_join(df, df_tai))
}
