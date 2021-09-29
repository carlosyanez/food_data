library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)
library(trelliscopejs)


con <- dbConnect(RSQLite::SQLite(), here("db","food_refined.sqlite"))


foods_local <- tbl(con, "foods") %>%
  collect() %>%
  mutate(panel=img_panel(image_url))

trelliscope(foods_local, name="foods",nrow=1,ncol=6) 
