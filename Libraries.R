## Libraries required for the course

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)

# Install INLA
install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

libs <- c("R.rsp", "devtools", "sf", "survival", "ggplot2", "gridExtra", "fmesher")
ix <- which(!sapply(libs, require, char = TRUE))
if (length(ix) > 0) {install.packages(libs[ix], repos = "https://cloud.r-project.org/")
  sapply(libs[ix], require, char = TRUE)}

# Install INLAjoint
devtools::install_github('DenisRustand/INLAjoint', build_vignettes = TRUE)
