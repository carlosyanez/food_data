library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)
library(trelliscopejs)
library(htmlwidgets)

con <- dbConnect(RSQLite::SQLite(), here("db","food_refined.sqlite"))


foods_local <- tbl(con, "foods") %>%
  collect() %>%
  mutate(panel=img_panel(image_url)) %>%
  select(-search_country) %>%
  mutate(across(where(is.numeric), ~ as.character(if_else(.x==1,"Yes","No"))))

dir_create(here("html"))

ts <- trelliscope(foods_local, name="nut free foods",nrow=1,ncol=8,path=here("html"))

saveWidget(ts, here("html","trelliscope.html"),
             selfcontained = FALSE,
             libdir = "lib",
             background = "white",
             title ="Nut free foods")

