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
  mutate(image_url=if_else(is.na(image_url),"no-image-icon-23483.png",image_url),
          panel = img_panel(image_url)) %>%
  select(-categories,-countries,
         -allergens,-traces) %>%
  rename("categories" = "search_category",
         "countries"="search_country") %>%
  mutate(any_allergen=if_else(rowSums(across(where(is.numeric)))==0,"No","Yes"),
         across(where(is.numeric), ~ as.character(if_else(.x == 1, "Yes", "No"))),
         nutriscore_grade=str_to_upper(nutriscore_grade),
         n=row_number())

foods_local$link <-cog_href(foods_local$url,
                           desc = "openfoodfacts.org entry",
                           default_label = TRUE,
                           default_active=TRUE)

dir_create(here("html"))


ts <- trelliscope(
  foods_local,
  name = str_c("nut free foods"),
  desc = "Nut free foods from openfoodfacts.org",
  md_desc='Collection of peanut and tree nut free foods from selected countries.
  
          Data obtained from [Open Food Facts](https://world.openfoodfacts.org).
  
          This project has been created using public, crowd-sourced food data. I make not claims it is accurate, and it **should not be used** as definitive guide of nut-free or allergen free food.
  
          Food ingredients constantly change. Something that was allergen free yesterday may not be tomorrow. Allergies reactions can be serious, thus they should not be treated lightly. **Always check the labels, even in products you consume regularly**
  
  <a href="https://www.freeiconspng.com/img/23483">Simple No Png</a>
  ',
  state = list(
    labels = c(
      "product_name",
      "brands",
      "ingredients_text",
      "countries",
      "categories",
      "stores",
      "nova_group",
      "nutriscore_grade",
      "link",
      "any_allergen"
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