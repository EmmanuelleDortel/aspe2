#' Reconstituer les effectifs par taxon et stade de vie
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo
#'
#' @param df data frame (doit contenir les champs tlo_id, tai_effectif, lop_effectif, mei_taille, lop_longueur_specimens_taille_mini, lop_longueur_specimens_taille_maxi)
#' @param var_id variable(s) identifiant les observations
#' @param var_taxon variable identifiant les espèces
#' @param var_tai variable identifiant les tailles utilisées pour définir les stades
#' @param var_tlo variable identifiant le type de longueur
#' @param tb data frame optionnel fournissant les tailles utilisées pour définir les stades
#'
#' @return df_std data frame
#' @export
#'
#' @examples
#'  \dontrun{
#'  df <- mef_creer_passerelle() %>%
#'  mef_ajouter_lots() %>%
#'  mef_reconstituer_capture_par_taille() %>%
#'  mef_reconstituer_effectif_par_stade()
#'  }
mef_reconstituer_effectif_par_stade <- function(df, var_id=ope_id, var_taxon=esp_code_alternatif, var_tai=esp_taille_mat, var_tlo=tlo_id, tb=NULL) {
  var_taxon <- enquo(var_taxon)
  var_tai <- enquo(var_tai)
  var_tlo <- enquo(var_tlo)
  var_id <- enquo(var_id)
  #-----------------------------------------------------------------------------
  if (is.null(tb)) { tb <- data_taxa }
  #-----------------------------------------------------------------------------
  # Add sizes for life-stage ranking
  df <- df %>% left_join(select(tb, !!var_taxon, tai_stade = !!var_tai, tlo_id = !!var_tlo) %>%
                           na.omit() %>% distinct())
  df_tai <- df %>% filter(!is.na(tai_stade))
  #-----------------------------------------------------------------------------
  # Measurement of individual sizes
  df_std <- df_tai[!is.na(df_tai$mei_taille),]
  df_std$stade <- ifelse(df_std$mei_taille < df_std$tai_stade, "S1", "S2")
  #-----------------------------------------------------------------------------
  # Measurement of individual sizes absent or unusable
  df_1 <- df_tai[is.na(df_tai$mei_taille),]
  if (nrow(df_1) > 0) {
    df_1$stade <- ifelse(df_1$lop_longueur_specimens_taille_mini >= df_1$tai_stade, "S2",
                         ifelse(df_1$lop_longueur_specimens_taille_maxi <= df_1$tai_stade, "S1", NA))
    df_std <- df_std %>%
      rbind(filter(df_1, !is.na(df_1$stade)) %>% mutate(tai_effectif = lop_effectif) %>% distinct()) %>%
      rbind(filter(df_1, is.na(df_1$stade)) %>% mutate(stade = "S1", tai_effectif = NA) %>% distinct()) %>%
      rbind(filter(df_1, is.na(df_1$stade)) %>% mutate(stade = "S2", tai_effectif = NA) %>% distinct())
  }
  #-----------------------------------------------------------------------------
  # Group headcounts by taxa, observation and life-stage
  df_std <- df_std %>%
    summarise(std_effectif = sum(tai_effectif), .by = c(!!var_id, !!var_taxon, stade))
  #-----------------------------------------------------------------------------
  # Add absence of life-stages
  df_std <- df_std %>%
    rbind(select(df_std, !!var_id, !!var_taxon) %>% distinct() %>%
            full_join(crossing(unique(select(df_std, !!var_taxon)), "stade"=c("S1","S2")), relationship = "many-to-many") %>%
            anti_join(select(df_std, !!var_id, !!var_taxon, stade) %>% distinct()) %>%
            mutate(std_effectif = 0))
  #-----------------------------------------------------------------------------
  # Add observations for species with an unavailable life-stage ranking
  df_std <- df_std %>%
    rbind(filter(df, is.na(tai_stade)) %>%
            select(!!var_id, !!var_taxon) %>%
            distinct() %>%
            mutate(stade = NA, std_effectif = NA))
  #-----------------------------------------------------------------------------
  return(df_std)
}
