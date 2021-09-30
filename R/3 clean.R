
library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)
library(countrycode)
library(cld3)


con <- dbConnect(RSQLite::SQLite(), here("db","food.sqlite"))
con1 <- dbConnect(RSQLite::SQLite(), here("db","food_refined.sqlite"))
food_table <- tbl(con, "foods")


allergens_edited <- read_csv(here("dicts","allergen_dict_edited.csv")) %>%
  select(-allergen_detailed) %>%
  mutate(allergen=str_to_lower(allergen)) %>%
  distinct()

allergen_list <- allergens_edited %>%
  distinct(standard_allergen) %>%
  filter(!is.na(standard_allergen)) %>%
  pull(standard_allergen) 

copy_to(con1,tibble(allergen=allergen_list),"allergens",TRUE)

to_remove <- allergens_edited %>% 
  filter(!is.na(free_of)) %>%
  distinct(allergen) %>%
  pull(allergen) %>%
  str_c(sep="|")


food_edited <-
  food_table %>%
  collect() 

food_edited <- food_edited %>%
  mutate(allergens=str_remove_all(allergens,to_remove),
         traces =str_remove_all(traces,to_remove)) %>%
  filter(!is.na(ingredients_text)) %>%
  filter(str_length(ingredients_text)>0) %>%
  mutate(ingredients_text=str_to_lower(str_replace_all(ingredients_text,"_"," ")),
         search_category=str_replace_all(search_category,"\\&"," "))




result_table <- tibble()

for(allergen_i in allergen_list){
  
  allergen_i_options <- allergens_edited %>%
    filter(standard_allergen==allergen_i) %>%
    distinct(allergen) %>%
    pull(allergen) 
  
  
  
  for(j in 1:nrow(food_edited)){
    
    present1 <- str_detect(str_to_lower(food_edited[j,]$allergens),allergen_i_options) 
    present2 <- str_detect(str_to_lower(food_edited[j,]$traces),allergen_i_options) 
    present3 <- str_detect(str_to_lower(food_edited[j,]$ingredients_text),allergen_i_options) 
    
    result <- sum(present1,present2,present3,na.rm=TRUE)
    
    if(result >1) 
      result_table <- result_table %>%
      bind_rows(tibble(code=food_edited[j,]$code,allergen=allergen_i))
  }
}

food_edited <- food_edited %>%
  left_join(result_table %>% 
              mutate(value=TRUE) %>%
              distinct() %>%
              pivot_wider(code,names_from = allergen,values_from = value),
            by="code") %>%
  mutate(across(where(is.logical), ~ if_else(is.na(.x),FALSE,.x))) %>%
  filter(`tree nuts`==FALSE & peanuts==FALSE) %>%
  select(-peanuts,-`tree nuts`) 



countries <- food_edited %>%
  mutate(countries=str_remove_all(countries,"[a-z][a-z]\\:")) %>%
  separate_rows(countries,sep=",") %>%
  mutate(countries=str_to_title(str_trim(countries)),
         countries=if_else(str_length(countries)==2,str_to_lower(countries),countries)) %>%
  filter(!is.na(countries)) %>%
  distinct(countries)

countries$std <-countryname(countries$countries)

countries <- countries %>%
  mutate(std=if_else(is.na(std),countrycode(countries,origin = 'iso2c', destination = 'cldr.short.en'),std))

food_edited2<- food_edited %>%
  select(-countries) %>%
  left_join(
    food_edited %>%
      mutate(countries=str_remove_all(countries,"[a-z][a-z]\\:")) %>%
      separate_rows(countries,sep=",") %>%
      left_join(countries,by="countries") %>%
      select(-countries) %>%
      rename("countries"="std") %>%
      mutate(countries=str_trim(countries)) %>%
      distinct() %>%
      group_by(code) %>%
      summarise(countries=str_c(countries,collapse=", "),.groups="drop"),
    by="code") %>%
  mutate(across(where(is_character) & !contains("image_url"), ~ str_remove_all(.x,"[a-z][a-z]\\:"))) %>%
  filter(!is.na(ingredients_text)) %>%
  mutate(product_name=if_else(is.na(product_name),ingredients_text,product_name),
         ingredients_text=str_to_lower(str_replace_all(ingredients_text,"_"," "))
         ) 

#food_edited2$ingr_lang <- detect_language(food_edited2$ingredients_text)

dbWriteTable(con1,"foods",food_edited2,overwrite=TRUE)


#get brands, packaging


food_edited2 %>%
  distinct(brands) %>%
  separate_rows(brands,sep=",") %>%
  distinct(brands) %>%
  filter(!is.na(brands)) %>%
  dbWriteTable(con1,"brands",.,overwrite=TRUE)

food_edited2 %>%
  distinct(packaging) %>%
  separate_rows(packaging,sep=",") %>%
  distinct(packaging) %>%
  filter(!is.na(packaging)) %>%
  dbWriteTable(con1,"pockaging",.,overwrite=TRUE)


food_edited2 %>%
  distinct(countries) %>%
  separate_rows(countries,sep=",") %>%
  mutate(countries=str_trim(countries)) %>%
  distinct(countries) %>%
  filter(!is.na(countries)) %>%
  dbWriteTable(con1,"countries",.,overwrite=TRUE)

food_edited2 %>%
  distinct(categories) %>%
  separate_rows(categories,sep=",") %>%
  distinct(categories) %>%
  filter(!is.na(categories)) %>%
  dbWriteTable(con1,"categories",.,overwrite=TRUE)


dbDisconnect(con1)
dbDisconnect(con)
rm(list=ls())