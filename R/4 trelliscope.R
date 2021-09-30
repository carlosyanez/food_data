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
  select(-search_country, categories) %>%
  rename("categories" = "serach_category") %>%
  mutate(across(where(is.numeric), ~ as.character(if_else(.x == 1, "Yes", "No"))))

dir_create(here("html"))

ts <- trelliscope(
  foods_local,
  name = "nut free foods",
  desc = "Nut free foods from openfoodfacts.org",
  state = list(
    labels = c(
      "product_name",
      "brands",
      "ingredients_text",
      "labels",
      "allergens",
      "traces",
      "nutriscore_grade",
      "nova_group",
      "pnns_groups_1",
      "pnns_groups_2",
      "stores",
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
