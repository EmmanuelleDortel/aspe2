#' Rajouter à la passerelle la saison de chaque opération
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom lubridate ymd_hms month
#'
#' @param df data frame
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_ope_date() %>%
#' mef_ajouter_ope_saison()
#' }
mef_ajouter_ope_saison <- function(df) {
  if ("mois" %in% colnames(df)) {
    df <- mutate(df, saison = ifelse(mois %in% c(4,5,6,7), "printemps", ifelse(mois %in% c(8,9,10,11), "automne", NA)))
  } else {
    df <- mutate(df, date = ymd_hms(ope_date), mois = month(date)) %>%
      mutate(saison = ifelse(mois %in% c(4,5,6,7), "printemps", ifelse(mois %in% c(8,9,10,11), "automne", NA))) %>%
      select(-date)
  }
}

