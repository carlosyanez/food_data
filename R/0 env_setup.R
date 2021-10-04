options(repos = c(
  carlosyanez = 'https://carlosyanez.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))

install.packages('just.install')
install.packages("tidyverse")
packages_to_install  <- tibble::tribble(~package,       ~source,~url,
                                  "here",         "CRAN","",
                                  "fs",           "CRAN","",
                                  "RSQLite",      "CRAN","",
                                  "DBI",          "CRAN","",
                                  "trelliscopejs","CRAN","",
                                  "htmlwidgets",    "CRAN","",
                                  "gitignore",     "CRAN","",
                                  "usethis",       "CRAN",""
)

just.install::justinstall(packages_to_install)
renv::snapshot(prompt = FALSE)

gitignore.file <- here::here(".gitignore")
new_lines <- gitignore::gi_fetch_templates("r")
gitignore::gi_write_gitignore(fetched_template = new_lines, gitignore_file = gitignore.file)
usethis::git_vaccinate()
write("files", gitignore.file, append = TRUE)
write("db/food.sqlite", gitignore.file, append = TRUE)
write("db/tracker.txt", gitignore.file, append = TRUE)
write("dicts/allergen_dict.csv", gitignore.file, append = TRUE)

rm(list = ls())
