
# aspe2

<!-- badges: start -->
<!-- badges: end -->
*aspe2* a été développé pour traiter les données sur les poissons de rivière issues de la base de données ASPE, gérée par l'Agence française pour la biodiversité (OFB). Il fournit des fonctions supplémentaires au package aspe pour le traitement de la base de données ASPE. Il comprend notamment les fonctions utiles pour la préparation des jeux de données nécessaires aux estimations des indicateurs de dynamique de population.

*aspe2* was developed to process river fish data from the ASPE database managed by the French Biodiversity Agency (OFB). It provides additionnal functions to aspe package for the ASPE database processing. In particular, it include useful functions for preparing datasets to estimate population dynamics indicators.

## Installation (fr)

*aspe2* nécessite l'installation des packages *dplyr*, *magrittr*, *GFE*, *lubridate*, *rlang*, *sf*, *tidyr* et *tidyselect* :

``` r 
install.packages(c("dplyr","magrittr","GFE","lubridate","rlang","sf","tidyr","tidyselect"))
```

La dernière version du package *apse2* peut être installée depuis Github au moyen de la commande suivante (nécessite l'installation préalable du package *devtools* et du *Rtools*) :

``` r
library(devtools)
devtools::install_github("manue6/aspe2")
```

Le package peut également être installé à partir de l'archive .tar [*aspe2_1.0.tar.gz*](https://github.com/manue6/Indicateurs_IDP) :

``` r
install.packages("aspe2_1.0.tar.gz", repos = NULL)
```

## Installation (en)

*aspe2* requires the installation of packages *dplyr*, *magrittr*, *GFE*, *lubridate*, *rlang*, *sf*, *tidyr* and *tidyselect*:

``` r 
install.packages(c("dplyr","magrittr","GFE","lubridate","rlang","sf","tidyr","tidyselect"))
```

The latest version of the *apse2* can be installed from Github using (requires prior installation of *Rtools* and package *devtools*):

``` r
library(devtools)
devtools::install_github("manue6/aspe2")
```
The package can also be installed from the downloadable tar archive [*aspe2_1.0.tar.gz*](https://github.com/manue6/Indicateur_IDP):

``` r
install.packages("aspe2_1.0.tar.gz", repos = NULL)
```
