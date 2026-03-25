# Install dependencies
install.packages(c("roxygen2", "remotes", "rcmdcheck"))

# Generate documentation, install package and validate it
roxygen2::roxygenise()
remotes::install_local(".")
rcmdcheck::rcmdcheck(args = c("--no-manual"), build_args = c("--no-manual"))
