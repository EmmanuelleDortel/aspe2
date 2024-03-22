#' Filtrer les points de prélèvements en fonction de la distribution native des espèce
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_name quo_is_null
#' @importFrom sf st_as_sf st_transform st_filter
#'
#' @param df data frame
#' @param var_taxon variable identifiant les espèces
#' @param var_obs variable identifiant les observation (utilise sur recode_taxon = TRUE)
#' @param recode_taxon logique, si TRUE les espèces non-identifiées sont ajoutées)
#'
#' @return data frame
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_coords_wgs84(var_id = pop_id) %>%
#' mef_filtrer_distribution_native(var_taxon = esp_code_alternatif)
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_coords_wgs84(var_id = pop_id) %>%
#' mef_filtrer_distribution_native(var_taxon = esp_code_alternatif,
#' recode_taxon = TRUE)
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_coords_wgs84(var_id = pop_id) %>%
#' mef_filtrer_distribution_native(var_taxon = esp_code_alternatif,
#' var_obs = ope_id,
#' recode_taxon = TRUE)
#' }
mef_filtrer_distribution_native <- function(df, var_taxon, var_obs=NULL, recode_taxon = FALSE) {
  var_taxon <- enquo(var_taxon)
  var_obs <- enquo(var_obs)
  #-----------------------------------------------------------------------------
  vec_taxon <- pull(df, !!var_taxon)[pull(df, !!var_taxon) %in% pull(distribution_native, !!var_taxon)] %>% unique()
  df <- df %>% mutate(XX = X, YY = Y) %>%
    st_as_sf(coords = c("XX", "YY"), crs = 4326) %>%
    st_transform(crs = 2154)
  #-----------------------------------------------------------------------------
  df_filtre <- do.call("rbind",lapply(vec_taxon, function(i) {
    distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
    df_i <- filter_at(df, quo_name(var_taxon), all_vars(.%in% i)) %>%
      st_filter(distrib)
    return(df_i)
  })) %>% rbind(filter_at(df, quo_name(var_taxon), all_vars(!.%in% vec_taxon)))
  #-----------------------------------------------------------------------------
  if (isTRUE(recode_taxon)) {
    ad_taxon <- filter(distribution_native, ind %in% 1) %>% pull(!!var_taxon) %>% unique()
    df_ad <- do.call("rbind",lapply(ad_taxon, function(i) {
      distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
      ref <- unlist(strsplit(unique(distrib$esp_code_ref),","))
      df_ref <- filter_at(df, quo_name(var_taxon), all_vars(.%in% ref))
      if (nrow(df_ref) > 0) {
        if (quo_is_null(var_obs)) {
          df_i <- filter_at(df, quo_name(var_taxon), all_vars(.%in% c(i,ref))) %>%
            st_filter(distrib) %>%
            mutate(!!var_taxon := i)
        } else {
          df_tax <- filter_at(df, quo_name(var_taxon), all_vars(.%in% i))
          df_i <- filter_at(df_ref, quo_name(var_obs), all_vars(!.%in% pull(df_tax, !!var_obs))) %>%
            rbind(df_tax) %>%
            st_filter(distrib) %>%
            mutate(!!var_taxon := i)
        }
      } else {df_i <- df[NULL,]}
      return(df_i)
    }))
    df_filtre <- filter_at(df_filtre, quo_name(var_taxon), all_vars(!.%in% ad_taxon)) %>%
      rbind(df_ad)
  }
  #-----------------------------------------------------------------------------
  return(data.frame(df_filtre) %>% select(-geometry))
}
