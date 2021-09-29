options(repos = c(
  carlosyanez = 'https://carlosyanez.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('just.install')
install.packages("tidyverse")
packages_to_install  <- tibble::tribble(~package,       ~source,~url,
                                  "librarian",    "Github","DesiQuintans/librarian",             # used to attach tidyverse, optional
                                  "box",          "CRAN","",
                                  "here",         "CRAN","",
                                  "fs",           "CRAN","",
                                  "usethis",      "CRAN","",
                                  "RSQLite",      "CRAN","",
                                  "DBI",          "CRAN","",
                                  "countrycode",  "CRAN","",
                                  "trelliscopejs","CRAN",""
)

just.install::justinstall(packages_to_install)
rm(packages_to_install)
renv::snapshot()

usethis::git_vaccinate()

