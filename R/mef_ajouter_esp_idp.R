#' Ajouter à un dataframe le code idp des espèces
#'
#' @import magrittr
#' @importFrom dplyr left_join select distinct
#' @importFrom tidyselect any_of
#'
#' @param df data frame (doit contenir au moins l'un des codes suivant : esp_id, esp_code_sandre, esp_code_alternatif)
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_esp_idp()
#' }
mef_ajouter_esp_idp <- function(df) {
  esp_names <- names(select(df, any_of(c("esp_id","esp_code_sandre","esp_code_alternatif"))))
  df <- df %>% left_join(select(data_taxa, esp_code_idp, !!esp_names) %>% distinct())
  return(df)
}
