not_loaded <- not_loaded[not_loaded!="0"]

for(filename in not_loaded){
  
  root <- str_remove(filename,"/Volumes/GoogleDrive/My Drive/GitHub/nutfree/files/") %>%
          str_remove(.,".csv") %>%
          str_split(.,"-") %>%
          unlist()
    
  country <- root[1]
  if(length(root==2)){
    category <- root[2]
  }else{
    category<-str_c(root[root!=country],collapse="-")
  }
  
  df <- read_csv(filename) %>%
    mutate(search_category = category, search_country = country) 
  write_csv(df,filename)
}