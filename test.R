library(tidyverse)
library(skimr)
library(lubridate)

#data load
read_plus <- function(flnm) {
  read_csv(flnm) %>% 
    mutate(filename = flnm)
}
#data listing short
dataParis<-list.files(path="/Users/e5028514/OneDrive - Victoria University/airbnb/paris/", 
                 full.names=TRUE,
                 recursive=T,
                 pattern="listings.csv$") %>%
  map_df(~read_csv(.))

#dataParis$filename1<-str_extract(dirname(dataParis$filename))
write_csv(dataParis, "/Users/e5028514/OneDrive - Victoria University/airbnb/paris/dataParisShort.csv",
          col_names=TRUE)


#data listing full
dataParisFull<-list.files(path="/Users/e5028514/OneDrive - Victoria University/airbnb/paris/", 
                      full.names=TRUE,
                      recursive=T,
                      pattern="listings 2.csv$") %>%
  map_df(~read_csv(.,col_types = cols(.default = "c" )))

dataParisFull$price <- as.numeric(gsub(",", "", substring(dataParisFull$price, 2)))

dataParisFull$month<- format(as.Date(dataParisFull$last_scraped), "%Y-%m")

write_csv(dataParisFull,  
          "/Users/e5028514/OneDrive - Victoria University/airbnb/paris/dataParisFull.csv",
          col_names=TRUE)
write_csv(listingFull,  
          "/Users/e5028514/OneDrive - Victoria University/airbnb/paris/dataParisFull.csv",
          col_names=TRUE)

#dataParisFull$filename1<-str_extract(dirname(dataParisFull$filename))
#full listings (listings 2.csv)
dataParisFull<-read_csv("/Users/e5028514/OneDrive - Victoria University/airbnb/paris/dataParisFull.csv")

dataParisLight<-dataParisFull%>%
  select(id, neighbourhood_cleansed, property_type, room_type, beds, price, number_)

str(dataParisFull)

#neighbourhoods
neighbours<-list.files(path="/Users/e5028514/OneDrive - Victoria University/airbnb/paris/", 
                       full.names=TRUE,
                       recursive=T,
                       pattern="neighbourhoods.csv$") %>%
  map_df(~read_csv(.,col_types = cols(.default = "c" )))

write_csv(neighbours,  
          "/Users/e5028514/OneDrive - Victoria University/airbnb/paris/neighboursFull.csv",
          col_names=TRUE)

#calendar (selected)
calendar<-list.files(path="/Users/e5028514/OneDrive - Victoria University/airbnb/paris/", 
                       full.names=TRUE,
                       recursive=T,
                       pattern="calendar.csv$") %>%
  map_df(~read_csv(.,col_types = cols(.default = "c" )))

calendar<-calendar %>% mutate(booked = ifelse(available=="f", 1, 0))


write_csv(calendar,  
          "/Users/e5028514/OneDrive - Victoria University/airbnb/paris/calendarFull.csv",
          col_names=TRUE)


#data exploration
skim(dataParis)

dataParisFull$neighbourhood<-factor(dataParisFull$neighbourhood)
dataParisFull %>%
dataParisclean<-dataParis%>%
  mutate(date=format(last_scraped,"%m%Y"))
%>%
  mutate(monthYearLastReview=format(last_review,"%m%Y"))%>%
  filter(!is.na(monthYearLastReview))


test<-dataParisclean%>%
  count(neighbourhood, monthYearLastReview, sort=TRUE) %>%
  mutate(neighbourhood=fct_reorder(neighbourhood,n))



%>%
  ggplot(aes(neighbourhood, n))+
  geom_col()+
  coord_flip()

            
