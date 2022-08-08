library(tidyverse)
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("brms", "ggrepel", "readxl", "rnaturalearth", 
                            "rnaturalearthdata", "rmapshaper"))
# full workflow
list(
  # files
  tar_target(fileDat, "data/WVS and ESS Merged Data.xlsx", format = "file"),
  tar_target(fileISO, "data/iso.csv", format = "file"),
  tar_target(fileGeo, "data/networks/1F Population Distance.xlsx", format = "file"),
  tar_target(fileLin, "data/networks/2F Country Distance 1pml adj.xlsx", format = "file"),
  # load data
  tar_target(d, loadData(fileDat, fileISO)),
  # load covariance matrices
  tar_target(geoCov, loadCovMat(fileGeo, d, log = TRUE)),
  tar_target(linCov, loadCovMat(fileLin, d, log = FALSE)),
  # fit treehugging models
  # 1. intercept-only models
  tar_target(m1.1, fitModel1.1(d)),
  tar_target(m1.2, fitModel1.2(d, geoCov, linCov)),
  # 2. HDI
  tar_target(m2.1, fitModel2.1(d)),
  tar_target(m2.2, fitModel2.2(d, geoCov, linCov)),
  # 3. EPI
  tar_target(m3.1, fitModel3.1(d)),
  tar_target(m3.2, fitModel3.2(d, geoCov, linCov)),
  # plots
  tar_target(plot1, plotWorldMap(d)),
  tar_target(plot2, plotCondEffects(m2.1, d, effects = "HDI.std", file = "plots/resultsHDI.pdf")),
  tar_target(plot3, plotCondEffects(m3.1, d, effects = "EPI.std", file = "plots/resultsEPI.pdf")),
  # intra-class correlations
  tar_target(iccHyp1.1a, "sd_year__Intercept^2 / (sd_year__Intercept^2 + sd_iso__Intercept^2 + sigma^2) = 0"),
  tar_target(iccHyp1.1b, "sd_iso__Intercept^2 / (sd_year__Intercept^2 + sd_iso__Intercept^2 + sigma^2) = 0"),
  tar_target(iccHyp1.2a, "sd_isoGeo__Intercept^2 / (sd_isoGeo__Intercept^2 + sd_isoLin__Intercept^2 + sd_iso__Intercept^2) = 0"),
  tar_target(iccHyp1.2b, "sd_isoLin__Intercept^2 / (sd_isoGeo__Intercept^2 + sd_isoLin__Intercept^2 + sd_iso__Intercept^2) = 0"),
  tar_target(iccHyp1.2c, "sd_iso__Intercept^2 / (sd_isoGeo__Intercept^2 + sd_isoLin__Intercept^2 + sd_iso__Intercept^2) = 0"),
  tar_target(icc1.1a, hypothesis(m1.1, iccHyp1.1a, class = NULL)),
  tar_target(icc1.1b, hypothesis(m1.1, iccHyp1.1b, class = NULL)),
  tar_target(icc1.2a, hypothesis(m1.2, iccHyp1.2a, class = NULL)),
  tar_target(icc1.2b, hypothesis(m1.2, iccHyp1.2b, class = NULL)),
  tar_target(icc1.2c, hypothesis(m1.2, iccHyp1.2c, class = NULL)),
  # results markdown
  tar_render(resultsRmd, "results.Rmd")
)
