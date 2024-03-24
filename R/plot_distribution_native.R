#' Cartes de la distribution native des espèces
#'
#' @import magrittr
#' @rawNamespace import(dplyr, except = lag)
#' @importFrom rlang enquo quo_is_null quo_name
#' @importFrom sf plot_sf
#' @importFrom grDevices dev.off png postscript x11
#' @importFrom graphics par
#'
#' @param var_taxon variable identifiant les espèces
#' @param vec_taxon vecteur de caractères listant les espèces souhaitées
#' @param titre logique indiquant si un titre est ajouté
#' @param var_titre variable utilisé pour le titre
#' @param cex.titre numerique, taille du titre
#' @param font.titre numerique, police du titre
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
#' plot_distribution_native()
#' plot_distribution_native(vec_taxon = c("BRE","BRO"))
#' }
plot_distribution_native <- function(var_taxon=esp_code_alternatif,vec_taxon=NULL,titre=TRUE,var_titre=NULL,cex.titre=2,font.titre=2,col.distribution="grey90",col.secteur="grey",col.contour="black",new.plot=TRUE,sauv.png=NULL,sauv.eps=NULL) {
  var_taxon <- enquo(var_taxon)
  var_titre <- enquo(var_titre)
  if (quo_is_null(var_titre)) { var_titre <- var_taxon }
  #-----------------------------------------------------------------------------
  if (is.null(vec_taxon)) {
    vec_taxon <- pull(distribution_native, !!var_taxon) %>% unique()
    vec_taxon <- vec_taxon[!vec_taxon %in% c("LOQ+LOL","VAC+VAD","GOU+GOO")]
  }
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
      i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
      if (nrow(i.distrib) > 0) {
        plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
      } else {
        plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
      }
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
      i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
      if (nrow(i.distrib) > 0) {
        plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
      } else {
        plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
      }
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
      i.distrib <- filter_at(distribution_native, quo_name(var_taxon), all_vars(.%in% i))
      if (nrow(i.distrib) > 0) {
        plot(select(i.distrib, geometry), border = col.secteur, col = col.distribution, add = T)
      } else {
        plot(select(distribution_native, geometry), border = col.secteur, col = col.distribution, add = T)
      }
      plot(fond_france, border = col.contour, col = "transparent", add=T)
    }
  }
}
