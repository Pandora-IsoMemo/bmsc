.First <- function() {
  options(repos = c(
    getOption('repos'),
    INWTLab = "https://inwtlab.github.io/drat/",
    PANDORA = "https://Pandora-IsoMemo.github.io/drat/"
  ))
}

.First()

if (interactive()) {
  rstantools::rstan_config()
}
