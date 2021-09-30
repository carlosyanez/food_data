#library(mongolite)
#food <- mongo("food", url = "mongodb://mongo_root:mongo_root()@localhost:28017/mongodb?authSource=admin&readPreference=primary&ssl=false")
#a<-food$find('{"country" : "en:Australia"}',limit=5)

#full DB too big for my laptop

library(tidyverse)
library(fs)
library(here)
library(DBI)

#create csv dir
dir_create(here("files"))


countries <-c("Austria","Switzerland","Germany","Greece",
              "Belgium","Netherlands","Australia",
              "Slovakia","Czechia","Italy","Spain",
              "Denmark","Sweden",
              "Norway","Ireland")
categories <- c("Snacks","Breakfasts","Spreads",
                "Sauces","Condiments","Beverages",
                "Cocoa&and&its&products","Flatbreads",
                "Canned&foods","Dairies","Frozen&foods",
                "Microwave&meals","Pasta&dishes","Refrigerated&meals")

selected_attributes <- c("product_name","packaging","brands",
                         "categories","origins","labels",
                         "countries",
                         "ingredients_text","allergens","traces",
                         "nutriscore_grade","nova_group","pnns_groups_1",
                         "pnns_groups_2","stores",
                         "main_category","image_url",
                         "code","url")

search_grid <- expand_grid(countries,categories)

for(i in 1:nrow(search_grid)){
  message(i)
  country    <- search_grid[i,]$countries
  category   <- search_grid[i,]$categories
  filename   <- here("files",str_c(country,"-",category,".csv"))
    
  if(!file_exists(filename)){
       download_string <- str_c("https://world.openfoodfacts.org/cgi/search.pl?action=process",
                           "&tagtype_0=categories&tag_contains_0=contains&tag_0=",category,
                           "&tagtype_1=categories&tag_contains_1=does_not_contain&tag_1=Nuts",
                           "&tagtype_2=categories&tag_contains_2=does_not_contain&tag_2=peanuts",
                           "&tagtype_3=allergens&tag_contains_3=does_not_contain&tag_3=nuts",
                           "&tagtype_4=allergens&tag_contains_4=does_not_contain&tag_4=peanuts",
                           "&tagtype_5=traces&tag_contains_5=does_not_contain&tag_5=nuts",
                           "&tagtype_6=traces&tag_contains_6=does_not_contain&tag_6=peanuts",
                           "&tagtype_7=countries&tag_contains_7=contains&tag_7=",country,
                           "&sort_by=unique_scans_n&page_size=20&download=on&format=csv")
      download.file(download_string,filename)
      
      read_csv(filename) %>%
        mutate(search_category=category,search_country=country) %>%
        write_csv(filename)
      
      Sys.sleep(60) 
  }
}

dir_create(here("db"))
sqlite_file <- here("db","food.sqlite")
con <- dbConnect(RSQLite::SQLite(), sqlite_file)
files <- dir_ls(here("files"))

uploaded_files <- here("db","tracker.txt")
if(file.exists(uploaded_files)){
  u <- read_csv(uploaded_files,col_names = FALSE)[1,] %>% pull()
  prexisting <- sum(files %in% u)
  files <- files[!(files %in% u)]
  
}else{
  file_create(uploaded_files)
  prexisting <-0
}
  


#first file
if(prexisting==0){
df <- read_csv(files[1], col_types = cols(.default = "c")) %>%
      select(all_of(c(selected_attributes,"search_country","search_category")))

dbWriteTable(con,"foods",df,overwrite=TRUE)
write(files[1],uploaded_files,append=TRUE)
j<-2
}else{
  j<-1
}

#others, appending if they are not there yet
for(i in j:length(files)){
  df <- read_csv(files[i],col_types = cols(.default = "c")) %>%
        select(all_of(c(selected_attributes,"search_country","search_category"))) %>%
        anti_join(dbReadTable(con,"foods"),by="code")
  dbWriteTable(con,"foods",df, append=TRUE) 
  write(files[i],uploaded_files,append=TRUE)
}

dbDisconnect(con)
rm(list=ls())
