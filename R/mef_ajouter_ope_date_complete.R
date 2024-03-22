#' Rajouter à la passerelle la date, le jour, le mois et l'année de chaque opération
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom lubridate ymd_hms year month day
#'
#' @param df data frame
#'
#' @return df
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_ope_date_complete()
#' }
mef_ajouter_ope_date_complete <- function(df) {
  df <- df %>% left_join(select(operation, ope_id, ope_date)) %>%
    mutate(date = ymd_hms(ope_date), annee = year(date), mois = month(date), jour = day(date)) %>%
    select(-date)
}
