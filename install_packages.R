# Other packages
install.packages(c("posterior", "bayesplot", "loo"))


install.packages("devtools")

library(devtools)

# Package we need for footBayes
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan(overwrite = TRUE)

# FootBayes package installation
install_github("LeoEgidi/footBayes")

# Check
cmdstanr::check_cmdstan_toolchain()
instantiate::stan_cmdstan_exists()

install.packages(c("httr", "jsonlite"))
