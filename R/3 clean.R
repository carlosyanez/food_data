
library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)

con <- dbConnect(RSQLite::SQLite(), here("db", "food.sqlite"))
con1 <-dbConnect(RSQLite::SQLite(), here("db", "food_refined.sqlite"))
food_table <- tbl(con, "foods") 


allergens_edited <-
  read_csv(here("dicts", "allergen_dict_edited.csv")) %>%
  select(-allergen_detailed) %>%
  mutate(allergen = str_to_lower(allergen)) %>%
  distinct()

allergen_list <- allergens_edited %>%
  distinct(standard_allergen) %>%
  filter(!is.na(standard_allergen)) %>%
  pull(standard_allergen)

copy_to(con1, tibble(allergen = allergen_list), "allergens", TRUE)

to_remove <- allergens_edited %>%
  filter(!is.na(free_of)) %>%
  distinct(allergen) %>%
  pull(allergen) %>%
  str_c(collapse = "|")

previous_clean <- ("foods" %in% dbListTables(con1))

if(previous_clean){
  
  
  present <- tbl(con1, "foods") %>%
             select(code) %>%
             collect()
  
  food_edited <-  food_table %>%
                  filter(code %in% present$code) %>%
                  collect()
  
  
}else{
  food_edited <-  food_table %>%
                  collect()
}

food_edited <- food_edited %>%
  filter(!grepl("water|wine|liquor",pnns_groups_1)) %>%
  mutate(allergens = str_remove_all(allergens, str_c(to_remove,collapse="|")),
         traces = str_remove_all(traces, str_c(to_remove,collapse="|"))) %>%
  filter(!is.na(ingredients_text)) %>%
  filter(str_length(ingredients_text) > 0) %>%
  mutate(
    ingredients_text = str_to_lower(str_replace_all(ingredients_text, "_", " ")),
    search_category = str_replace_all(search_category, "\\&", " ")
  )

result_table <- tibble()

for (allergen_i in allergen_list) {
  message(allergen_i)
  allergen_i_options <- allergens_edited %>%
    filter(standard_allergen == allergen_i) %>%
    distinct(allergen) %>%
    pull(allergen)
  
  for (j in 1:nrow(food_edited)) {
    present1 <-
      str_detect(str_to_lower(food_edited[j, ]$allergens),
                 allergen_i_options)
    present2 <-
      str_detect(str_to_lower(food_edited[j, ]$traces), allergen_i_options)
    present3 <-
      str_detect(str_to_lower(food_edited[j, ]$ingredients_text), allergen_i_options)
    
    result <- sum(present1, present2, present3, na.rm = TRUE)
    
    if (result >= 1)
      result_table <- result_table %>%
      bind_rows(tibble(code = food_edited[j, ]$code, allergen = allergen_i))
  }
}

food_edited <- food_edited %>%
  left_join(
    result_table %>%
      mutate(value = TRUE) %>%
      distinct() %>%
      pivot_wider(code, names_from = allergen, values_from = value),
    by = "code"
  ) %>%
  mutate(across(where(is.logical), ~ if_else(is.na(.x), FALSE, .x))) 

if("tree nuts" %in% colnames(food_edited)){
  food_edited <- food_edited %>%
    filter(`tree nuts` == FALSE) %>%
    select(-`tree nuts`)
}
if("peanuts" %in% colnames(food_edited)){
  food_edited <- food_edited %>%
    filter(peanuts== FALSE) %>%
    select(-peanuts)
}

countries <- food_edited %>% select(code,countries=search_country) %>%
  separate_rows(countries,sep=",") %>%
  distinct(code,countries) %>%
  group_by(code) %>%
  summarise(countries=str_c(countries,collapse=","),.groups = "drop")

categories <- food_edited %>% select(code,categories=search_category) %>%
  separate_rows(categories,sep=",") %>%
  distinct(code,categories) %>%
  group_by(code) %>%
  summarise(categories=str_c(categories,collapse=","),.groups = "drop")

food_edited <- food_edited %>%
  mutate(across(
    where(is_character) &
      !contains("url"),
    ~ str_remove_all(.x, "[a-z][a-z]\\:")
  )) %>%
  filter(!is.na(ingredients_text)) %>%
  mutate(
    product_name = if_else(is.na(product_name), ingredients_text, product_name),
    ingredients_text = str_to_lower(str_replace_all(ingredients_text, "_", " "))) %>%
  select(-matches("categor"),-matches("countr")) %>%
  left_join(countries,by="code") %>%
  left_join(categories,by="code")


dbWriteTable(con1, "foods", food_edited, overwrite = !previous_clean,append=previous_clean)

dbDisconnect(con1)
dbDisconnect(con)
rm(list = ls())