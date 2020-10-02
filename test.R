library(tidyverse)
library(skimr)
#data load
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}

dataParis<-list.files(path="/Users/e5028514/OneDrive - Victoria University/airbnb/paris/", 
                 full.names=TRUE,
                 recursive=T,
                 pattern="listings.csv$") %>%
  map_df(~read_plus(.))

dataParis$filename1<-str_extract(dirname(dataParis$filename))
  
#data exploration
skim(dataParis)

dataParis$neighbourhood<-factor(dataParis$neighbourhood)
test<-dataParis%>%
  count(neighbourhood) %>%
  mutate(neighbourhood=fct_reorder(neighbourhood,n))

            
