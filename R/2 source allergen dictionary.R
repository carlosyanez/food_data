library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)


con <- dbConnect(RSQLite::SQLite(), here("db","food.sqlite"))
food_table <- tbl(con, "foods")

allergens <- food_table %>% 
  # select(all_of(selected_attributes)) %>%
  filter(!is.na(allergens)) %>%
  select(allergens) %>%
  distinct() %>%
  collect()

traces <- food_table %>% 
  # select(all_of(selected_attributes)) %>%
  filter(!is.na(traces)) %>%
  select(traces) %>%
  distinct() %>%
  collect()

#Create Allergen Dictionary

allergens %>%
  bind_rows(
    traces %>%
      rename("allergens"="traces")) %>%
  mutate(allergens=str_replace_all(allergens,"037",","),
         allergens=str_split(traces,","),.keep="unused") %>%
  unnest(allergens) %>%
  mutate(allergens=str_remove_all(allergens,"\""),
         allergens=str_remove_all(allergens,"c\\("),
         allergens=str_remove_all(allergens,"\\n")) %>%
  distinct() %>%
  separate(allergens,c("lang","allergen"),sep=":") %>%
  filter(!(allergen=="\\037en")) %>%
  arrange(lang,allergen) %>%
  write_csv(here("dicts","allergen_dict.csv"))


#Create Country dictionary

food_table %>%
  distinct(countries) %>%
  collect() %>%
  separate_rows(countries,sep=",") %>%
  distinct(countries) %>%
  mutate(countries=str_to_title(str_remove_all(countries,"[a-z][a-z]\\:"))) %>%
  distinct(countries) %>%
  arrange(countries)
  write_csv(here("dicts","county_dict.csv"))

  
