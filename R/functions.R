# custom functions

# load WVS and ESS combined data
loadData <- function(fileDat, fileISO) {
  # load from file
  read_xlsx(fileDat) %>%
    # modify variables
    mutate(
      Gender        = factor(ifelse(Gender == 1, "Male", "Female")),
      Age.std       = as.numeric(scale(Age)),
      RelAttend.std = as.numeric(scale(RelAttend)),
      LeftRight.std = as.numeric(scale(LeftRight)),
      Income.std    = as.numeric(scale(Income)),
      HDI.std       = as.numeric(scale(HDI)),
      EPI.std       = as.numeric(scale(EPI))
    ) %>%
    # add ISO codes
    left_join(read.csv(fileISO), by = "country") %>%
    # copy columns
    mutate(isoGeo = iso, isoLin = iso)
}

# load covariance matrices
loadCovMat <- function(file, d, log) {
  # load distance matrix
  out <- 
    read_excel(file, na = "") %>%
    dplyr::select(-ISO) %>%
    as.matrix()
  rownames(out) <- colnames(out)
  # log distances?
  if (log) out <- log(out)
  # distances between 0 and 1
  out <- out / max(out)
  diag(out) <- 0
  # 1 - distance = proximity (covariance)
  out <- 1 - out
  # subset matrix
  out <- out[rownames(out)[rownames(out) %in% d$iso],
             colnames(out)[colnames(out) %in% d$iso]]
  # fit model
  return(out)
}

# plot world map
plotWorldMap <- function(d) {
  # get treehugging data
  dAvg <-
    d %>%
    group_by(iso) %>%
    summarise(Treehugging = mean(Treehugging, na.rm = TRUE))
  # get world map
  world <- 
    ne_countries(scale = "medium", returnclass = "sf") %>%
    # add data
    left_join(dAvg, by = c("iso_a2" = "iso"))
  # plot with treehugger averages
  out <- 
    world %>%
    filter(region_wb != "Antarctica") %>%
    ms_filter_islands(min_area = 12391399903) %>%
    ggplot() +
    geom_sf(aes(fill = Treehugging), size = 0.05) +
    scale_fill_gradient2(high = "darkgreen", mid = "white", low = "blue",
                         limits = c(-1.77, 1.77)) +
    theme_void()
  # save
  ggsave(out, filename = "plots/plotWorldMap.pdf", height = 3.5, width = 8)
  return(out)
}

# fit model 1.1
fitModel1.1 <- function(d) {
  brm(
    # model formula
    Treehugging ~ 1 + (1 | year) + (1 | iso),
    # data
    data = d,
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113
    )
}

# fit model 1.2
fitModel1.2 <- function(d, geoCov, linCov) {
  brm(
    # model formula
    Treehugging ~ 1 + (1 | gr(isoGeo, cov = geoCov)) + (1 | gr(isoLin, cov = linCov)) + (1 | iso),
    # data
    data = d, data2 = list(geoCov = geoCov, linCov = linCov),
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113,
    # control arguments
    control = list(adapt_delta = 0.99, max_treedepth = 12)
  )
}

# fit model 2.1
fitModel2.1 <- function(d) {
  brm(
    # model formula
    Treehugging ~ 1 + HDI.std + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std +
      (1 + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std | iso),
    # data
    data = d,
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113
  )
}

# fit model 2.2
fitModel2.2 <- function(d, geoCov, linCov) {
  brm(
    # model formula
    Treehugging ~ 1 + HDI.std + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std +
      (1 + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std | iso) +
      (1 | gr(isoGeo, cov = geoCov)) + (1 | gr(isoLin, cov = linCov)),
    # data
    data = d, data2 = list(geoCov = geoCov, linCov = linCov),
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113
  )
}

# fit model 3.1
fitModel3.1 <- function(d) {
  brm(
    # model formula
    Treehugging ~ 1 + EPI.std + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std +
      (1 + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std | iso),
    # data
    data = d,
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113
  )
}

# fit model 3.2
fitModel3.2 <- function(d, geoCov, linCov) {
  brm(
    # model formula
    Treehugging ~ 1 + EPI.std + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std +
      (1 + Gender + Age.std + RelAttend.std + LeftRight.std + Income.std | iso) +
      (1 | gr(isoGeo, cov = geoCov)) + (1 | gr(isoLin, cov = linCov)),
    # data
    data = d, data2 = list(geoCov = geoCov, linCov = linCov),
    # priors (from prior predictive simulation)
    prior = c(prior(normal(0, 0.5), class = Intercept),
              prior(normal(0, 0.5), class = b),
              prior(exponential(4), class = sd),
              prior(exponential(4), class = sigma)),
    # number of iterations
    iter = 4000,
    # number of cores
    cores = 4,
    # seed
    seed = 2113
  )
}

# plot conditional effects
plotCondEffects <- function(model, d, effects = "", file = "") {
  # data grouped by country
  dISO <-
    d %>%
    group_by(iso) %>%
    summarise(
      Treehugging = mean(Treehugging, na.rm = TRUE),
      HDI.std     = mean(HDI.std),
      EPI.std     = mean(EPI.std)
      ) %>%
    drop_na() %>%
    # for plotting
    mutate(lower__ = 1, upper__ = 1)
  # plot
  out <- 
    plot(conditional_effects(model, effects = effects), plot = FALSE)[[1]] +
    geom_point(data = dISO, aes(y = Treehugging, x = !!sym(effects)), alpha = 0.2) +
    geom_text_repel(data = dISO, aes(y = Treehugging, x = !!sym(effects), label = iso), 
                    alpha = 0.2, max.overlaps = 35, seed = 2113) +
    ylim(c(-5, 5)) +
    theme_classic()
  # save
  ggsave(out, filename = file, width = 5, height = 5)
  return(out)
}
