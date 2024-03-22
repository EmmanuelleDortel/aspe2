#' Reconstituer les effectifs par lots et taille
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#' @importFrom stats xtabs setNames aggregate
#' @importFrom GFE round_preserve_sum
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
#' mef_reconstituer_effectif_par_taille()
#' }
mef_reconstituer_effectif_par_taille <- function(df, var_taxon=esp_code_alternatif) {
  var_taxon <- enquo(var_taxon)
  #-----------------------------------------------------------------------------
  # Function to reconstitution of headcounts by batch and size
  buid_size <- function(df_n,i) {
    d <- as.data.frame(proportions(xtabs(~mei_taille, df_n[df_n$lop_id == i,])))
    d$tai_effectif <- round_preserve_sum(unique(df_n[df_n$lop_id == i,"lop_effectif"]) * d$Freq)
    d <- d %>% mutate(mei_taille = as.numeric(as.vector(d$mei_taille))) %>%
      mutate(lop_id = i) %>%
      mutate(!!var_taxon := unique(pull(df_n[df_n$lop_id == i,], !!var_taxon))) %>%
      mutate(tlo_id = unique(df_n$tlo_id[df_n$lop_id == i])) %>%
      mutate(lop_effectif = unique(df_n$lop_effectif[df_n$lop_id == i])) %>%
      select(lop_id, !!var_taxon, mei_taille, tlo_id, lop_effectif, tai_effectif)
    return(d)
  }
  #-----------------------------------------------------------------------------
  # Add individual measurements and minimum and maximum lot sizes
  df_mei <- select(df, !!var_taxon, lop_id, tlo_id, tyl_id, lop_effectif) %>%
    distinct() %>%
    left_join(rename(mesure_individuelle, lop_id = mei_lop_id)) %>%
    left_join(select(lot_poissons, lop_id, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi))
  #-----------------------------------------------------------------------------
  # Difference between numbers of measured individuals and catched individuals (S/L and G)
  dech <- df_mei[(df_mei$tyl_id %in% 2:3 & !is.na(df_mei$mei_taille)),]
  if (nrow(dech) > 0) {
    ech <- aggregate(dech$mei_id, list("lop_id"=dech$lop_id,"eff"=dech$lop_effectif), function(i) length(unique(i)))
    ech$diff <- ech$eff - ech$x
  } else { ech <- setNames(data.frame(array(dim=c(0,4))),c("lop_id","eff","x","diff")) }
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual size and weight (N)
  df_tai <- df_mei %>%
    filter(tyl_id %in% 4) %>%
    mutate(across(c("lop_longueur_specimens_taille_mini","lop_longueur_specimens_taille_maxi"), ~mei_taille)) %>%
    mutate(tai_effectif = 1) %>%
    select(lop_id, !!var_taxon, mei_taille, tlo_id, lop_effectif, tai_effectif, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi)
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual size, total weight (I, S/L, G)
  df_1 <- df_mei %>%
    filter(tyl_id %in% 1) %>%
    rbind(dech[dech$lop_id %in% ech$lop_id[ech$diff == 0],])
  if (nrow(df_1) > 0) {
    df_tai <- df_tai %>%
      rbind(mutate(df_1, mei_effectif = 1) %>%
              summarise(tai_effectif = sum(mei_effectif), .by = c(lop_id,!!var_taxon,mei_taille,tlo_id,lop_effectif)) %>%
              left_join(summarise(df_1, "lop_longueur_specimens_taille_mini"=min(mei_taille),
                                  "lop_longueur_specimens_taille_maxi"=max(mei_taille),
                                  .by=lop_id)))
  }
  #-----------------------------------------------------------------------------
  # Batches with measurements of individual size for representative sample, individuals or total weights (S/L, G)
  df_2 <- dech[(dech$lop_id %in% ech$lop_id[ech$diff > 0]),]
  if (nrow(df_2) > 0) {
    df_tai <- df_tai %>%
      rbind(do.call("rbind",lapply(unique(df_2$lop_id), function(i) buid_size(df_2,i))) %>%
              left_join(summarise(df_2, "lop_longueur_specimens_taille_mini"=min(mei_taille),
                                  "lop_longueur_specimens_taille_maxi"=max(mei_taille),
                                  .by=lop_id)))
  }
  #-----------------------------------------------------------------------------
  # Batches with measurements absent or unusable
  df_3 <- df_mei[!df_mei$lop_id %in% unique(df_tai$lop_id),]
  if (nrow(df_3) > 0) {
    df_tai <- df_tai %>%
      rbind(mutate(df_3, tai_effectif = NA, mei_taille = NA) %>%
              select(lop_id, !!var_taxon, mei_taille, tlo_id, lop_effectif, tai_effectif, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi) %>%
              distinct())
  }
  #-----------------------------------------------------------------------------
  return(left_join(df_tai, df))
}
