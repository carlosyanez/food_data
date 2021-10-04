
library(tidyverse)
library(fs)
library(here)
library(DBI)
library(dbplyr)

#create csv dir
dir_create(here("files"))


countries <- c(
  "Austria",
  "Switzerland",
  "Germany",
  "Greece",
  "Belgium",
  "Netherlands",
  "Australia",
  "Slovakia",
  "Czechia",
  "Italy",
  "Spain",
  "Denmark",
  "Sweden",
  "Norway",
  "Ireland",
  "Chile",
  "Luxembourg"
)
categories <- c(
  "Snacks",
  "Breakfasts",
  "Spreads",
  "Sauces",
  "Condiments",
  "Beverages",
  "Cocoa%20and%20its%20products",
  "Flatbreads",
  "Canned%20foods",
  "Dairies",
  "Frozen%20foods",
  "Microwave%20meals",
  "Pasta%20dishes",
  "Refrigerated%20meals",
  "Breads",
  "Frozen%20desserts",
  "Ice%20creams",
  "Plant-based%20ice%20creams",
  "Viennoiseries",
  "Confectioneries",
  "Meals",
  "Biscuits%20and%20cakes"
)

selected_attributes <- c(
  "product_name",
  "packaging",
  "brands",
  "categories",
  "origins",
  "labels",
  "countries",
  "ingredients_text",
  "allergens",
  "traces",
  "nutriscore_grade",
  "nova_group",
  "pnns_groups_1",
  "pnns_groups_2",
  "stores",
  "main_category",
  "image_url",
  "code",
  "url"
)

search_grid <- expand_grid(countries, categories)

extras <- tibble::tribble(~countries,~categories,
                          "United%20Kingdom","Cocoa%20and%20its%20products",
                          "United%20Kingdom","Snacks",
                          "United%20Kingdom","Confectioneries",
                          "United%20Kingdom","Biscuits%20and%20cakes"
                          
                          )

search_grid <- bind_rows(search_grid,extras)

for (i in 1:nrow(search_grid)) {
  
  country    <- search_grid[i, ]$countries
  category   <- search_grid[i, ]$categories
  filename   <- here("files", str_c(country, "-", category, ".csv"))
  
  if (!file_exists(filename)) {
    download_string <-
      str_c(
        "https://world.openfoodfacts.org/cgi/search.pl?action=process",
        "&tagtype_0=categories&tag_contains_0=contains&tag_0=",
        category,
        "&tagtype_1=categories&tag_contains_1=does_not_contain&tag_1=Nuts",
        "&tagtype_2=categories&tag_contains_2=does_not_contain&tag_2=peanuts",
        "&tagtype_3=allergens&tag_contains_3=does_not_contain&tag_3=nuts",
        "&tagtype_4=allergens&tag_contains_4=does_not_contain&tag_4=peanuts",
        "&tagtype_5=traces&tag_contains_5=does_not_contain&tag_5=nuts",
        "&tagtype_6=traces&tag_contains_6=does_not_contain&tag_6=peanuts",
        "&tagtype_7=countries&tag_contains_7=contains&tag_7=",
        country,
        "&sort_by=unique_scans_n&page_size=20&download=on&format=csv"
      )
    download.file(download_string, filename)
    
    df <- read_csv(filename) %>%
      mutate(search_category = category, search_country = country) 
    write_csv(df,filename)
    rm(df)
    
    Sys.sleep(30)
  }
}

dir_create(here("db"))
sqlite_file <- here("db", "food.sqlite")
con <- dbConnect(RSQLite::SQLite(), sqlite_file)
files <- dir_ls(here("files"))

uploaded_files <- here("db", "tracker.txt")
if (file.exists(uploaded_files)) {
  u <- read_csv(uploaded_files, col_names = FALSE)[,1] %>% pull(.)
  files <- files[!(files %in% u)]
  prexisting <- 1
  
} else{
  file_create(uploaded_files)
  prexisting <- 0
}



#first file
if (prexisting == 0) {
  df <- read_csv(files[1], col_types = cols(.default = "c")) %>%
        filter(!is.na(ingredients_text)) %>%
        select(all_of(c(selected_attributes, "search_country", "search_category"))) %>%
        mutate(search_category=str_replace_all(search_category,"%20"," ")) %>%
        mutate(search_country=str_replace_all(search_country,"%20"," ")) %>%
        mutate(new_flag=1)

 
  dbWriteTable(con, "foods", df, overwrite = TRUE)
  write(files[1], uploaded_files, append = TRUE)
  j <- 2
} else{
  j <- 1
}

foods <- tbl(con, "foods") 
not_loaded <- c("0")
if(length(files)>0){
#others, appending if they are not there yet
for (i in j:length(files)) {
  
  
  new_df <- read_csv(files[i], col_types = cols(.default = "c")) 

  if(sum(colnames(new_df) %in% c(selected_attributes,"search_country","search_category"))==21){
    
   new_df <- new_df %>%
             filter(!is.na(ingredients_text)) %>%
             select(all_of(c(selected_attributes, "search_country", "search_category"))) %>%
             mutate(search_category=str_replace_all(search_category,"%20"," ")) 
    
   dupes <- foods %>% 
           select(code,search_category,search_country) %>%
           collect() %>%
           filter(code %in% new_df$code) %>%
           rename("cat2"="search_category","country2"="search_country")
  
   if(nrow(dupes)>0){
    dbWriteTable(con, "dupes", dupes, overwrite = TRUE)
    dbExecute(con,"DELETE FROM foods WHERE code in (SELECT code from dupes)")
    
    new_df <- new_df %>%
              left_join(dupes,
                        by="code") %>%
              mutate(
                    search_category=case_when(
                                              is.na(cat2)                      ~ search_category,
                                              is.na(search_category)           ~ cat2,
                                              str_detect(search_category,cat2) ~ search_category,
                                              search_category==cat2            ~ search_category,
                                              TRUE                             ~ str_c(search_category,cat2,sep=",")
                                    ),
                    search_country=case_when(
                                             is.na(country2)                     ~ search_country,
                                             is.na(search_country)               ~ country2,
                                             str_detect(search_country,country2) ~ search_country,
                                             search_country==country2            ~ search_country,
                                             TRUE                                ~ str_c(search_country,country2,sep=","),
                                             
                    ),
                     ) %>%
              select(-cat2,-country2)
    
     dbRemoveTable(con,"dupes")    
  
   }
   
   new_df$new_flag<-1
   dbWriteTable(con, "foods", new_df, append = TRUE)
   write(files[i], uploaded_files, append = TRUE)
   
  }
  else{
    
    message(files[i]," not loaded")
    not_loaded <- c(not_loaded,files[i])}
}
}

dbDisconnect(con)

