#' Cartes de répartition des observations de chaque espèce
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_is_null quo_name
#' @importFrom sf st_as_sf st_transform plot_sf
#' @importFrom grDevices dev.off png postscript x11
#' @importFrom graphics par
#'
#' @param df data frame devant contenir les coordonnées géographique X et Y en WGS84
#' @param var_taxon variable identifiant les espèces
#' @param distrib_native logique indiquant si la distribution native est ajoutée
#' @param titre logique indiquant si un titre est ajouté
#' @param var_titre variable utilisé pour le titre
#' @param cex.titre numerique, taille du titre
#' @param font.titre numerique, police du titre
#' @param cex.point numerique, taille des points
#' @param col.point caractère, couleur des points
#' @param col.distribution caractère, couleur de la distribution native
#' @param col.secteur caractère, couleur des secteurs
#' @param col.contour caractère, couleur du contour de la carte
#' @param new.plot logique indiquant si une nouvelle fênetre graphique est générée pour chaque espèce
#' @param sauv.png caractère, chemin et nom des fichiers enregistrés en .png
#' @param sauv.eps caractère, chemin et nom des fichiers enregistrés en .eps
#'
#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' df <- mef_creer_passerelle() %>%
#' mef_ajouter_ope_date() %>%
#' filter(annee %in% 2010:2020) %>%
#' mef_ajouter_lots() %>%
#' mef_ajouter_coords_wgs84(var_id = pop_id) %>%
#' filter(esp_code_alternatif %in% c("BRO","CHA"))
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif)
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif,
#' distrib_native = FALSE)
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif,
#' var_titre = esp_nom_latin,
#' font.titre=4,
#' sauv.png = "Carte")
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif,
#' var_titre = esp_nom_latin,
#' font.titre=4,
#' col.distribution="azure",
#' col.secteur="transparent",
#' sauv.png = "Carte")
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif,
#' titre = FALSE,
#' col.distribution="azure",
#' col.secteur="transparent",
#' sauv.png = "Carte")
#' par(mfrow=c(1,2))
#' plot_repartition_observation(df, var_taxon = esp_code_alternatif,
#' new.plot=FALSE)
#' }
plot_repartition_observation <- function(df,var_taxon,distrib_native=TRUE,titre=TRUE,var_titre=NULL,cex.titre=2,font.titre=2,cex.point=0.75,col.point="black",col.distribution="grey90",col.secteur="grey",col.contour="black",new.plot=TRUE,sauv.png=NULL,sauv.eps=NULL) {
  var_taxon <- enquo(var_taxon)
  var_titre <- enquo(var_titre)
  if (quo_is_null(var_titre)) { var_titre <- var_taxon }
  #-----------------------------------------------------------------------------
  vec_taxon <- pull(df, !!var_taxon) %>% unique()
  df <- st_as_sf(df, coords = c("X", "Y"), crs = 4326) %>% st_transform(crs = 2154)
  #-----------------------------------------------------------------------------
  for (i in vec_taxon) {
    if (isTRUE(titre)) {
      i.titre <- filter_at(data_taxa, quo_name(var_taxon), all_vars(. %in% i)) %>% pull(!!var_titre) %>% unique()
    }
    #---------------------------------------------------------------------------
    if (!is.null(sauv.png)) {
      png(filename=paste(sauv.png,"_",i,".png",sep=""), width = 604, height = 604, units="px")
      par(mar=c(1,1,2,1))
      if (isFALSE(titre)) {
        plot(select(distribution_native, geometry), border = col.secteur, key.pos = NULL, reset = FALSE)
      } else {
        plot(select(distribution_native, geometry), main = i.titre, cex.main = cex.titre, font.main = font.titre, border = col.secteur, key.pos = NULL, reset = FALSE)
      }
      if (isTRUE(distrib_native)) {
        i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
        if (nrow(i.distrib) > 0) {
          plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
        } else {
          plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
        }
      }
      df_i <- filter_at(df, quo_name(var_taxon), all_vars(.%in% i))
      plot(select(df_i, geometry), pch = 16, cex = cex.point, col = col.point, add = T)
      plot(fond_france, border = col.contour, col = "transparent", add=T)
      dev.off()
    }
    #---------------------------------------------------------------------------
    if (!is.null(sauv.eps)) {
      postscript(file=paste(sauv.eps,"_",i,".eps",sep=""), paper = "letter", horizontal = FALSE)
      par(mar=c(1,1,2,1))
      if (isFALSE(titre)) {
        plot(select(distribution_native, geometry), border = col.secteur, key.pos = NULL, reset = FALSE)
      } else {
        plot(select(distribution_native, geometry), main = i.titre, cex.main = cex.titre, font.main = font.titre, border = col.secteur, key.pos = NULL, reset = FALSE)
      }
      if (isTRUE(distrib_native)) {
        i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
        if (nrow(i.distrib) > 0) {
          plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
        } else {
          plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
        }
      }
      df_i <- filter_at(df, quo_name(var_taxon), all_vars(.%in% i))
      plot(select(df_i, geometry), pch = 16, cex = cex.point, col = col.point, add = T)
      plot(fond_france, border = col.contour, col = "transparent", add=T)
      dev.off()
    }
    #---------------------------------------------------------------------------
    if (is.null(sauv.eps) & is.null(sauv.png)) {
      if (isTRUE(new.plot)) { x11(title = i) }
      par(mar=c(1,1,2,1))
      if (isFALSE(titre)) {
        plot(select(distribution_native, geometry), border = col.secteur, key.pos = NULL, reset = FALSE)
      } else {
        plot(select(distribution_native, geometry), main = i.titre, cex.main = cex.titre, font.main = font.titre, border = col.secteur, key.pos = NULL, reset = FALSE)
      }
      if (isTRUE(distrib_native)) {
        i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
        if (nrow(i.distrib) > 0) {
          plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
        } else {
          plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
        }
      }
      df_i <- filter_at(df, quo_name(var_taxon), all_vars(.%in% i))
      plot(select(df_i, geometry), pch = 16, cex = cex.point, col = col.point, add = T)
      plot(fond_france, border = col.contour, col = "transparent", add=T)
    }
  }
}
