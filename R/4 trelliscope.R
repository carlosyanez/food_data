library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)
library(trelliscopejs)
library(htmlwidgets)

con <-
  dbConnect(RSQLite::SQLite(), here("db", "food_refined.sqlite"))


foods_local <- tbl(con, "foods") %>%
  collect() %>%
  mutate(panel = img_panel(image_url)) %>%
  select(-categories,-countries) %>%
  rename("categories" = "search_category",
         "countries"="search_country") %>%
  mutate(across(where(is.numeric), ~ as.character(if_else(.x == 1, "Yes", "No"))),
         n=row_number())


dir_create(here("html"))


ts <- trelliscope(
  foods_local,
  name = str_c("nut free foods"),
  desc = "Nut free foods from openfoodfacts.org",
  state = list(
    labels = c(
      "product_name",
      "brands",
      "ingredients_text",
      "countries",
      "categories",
      "url"
    )
  ),
  nrow = 2,
  ncol = 8,
  path = here("html")
)


saveWidget(
  ts,
  here("html", "index.html"),
  selfcontained = FALSE,
  libdir = "lib",
  background = "white",
  title = "Nut free foods"
)
rm(list = ls())