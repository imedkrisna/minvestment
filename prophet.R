library(tidyverse)
library(readxl)
library(prophet)

fdi<-read_csv('data/US_FdiFlowsStock.csv')|>filter(Flow=="08")|>filter(Direction==1)
fdi<-fdi|>select(Year,Economy,`US$ at current prices in millions`)|>filter(Economy=="360"|Economy=="960")
#fdi<-fdi[nchar(fdi$Economy)<=3,]
fdi<-fdi|>rename(fdi=`US$ at current prices in millions`)|>na.omit()
fdi<-fdi|>select(!Economy)|>rename(ds=Year,y=fdi)
fdi$ds<-as.Date(as.character(fdi$ds),format="%Y")

fdit<-fdi|>filter(ds<as.Date("2022-08-03",format="%Y-%m-%d"))

m<-prophet(fdit) ## seasonality should be disabled automatically
future <- make_future_dataframe(m, periods = 2,freq="year")


forecast <- predict(m, future)
plot(m,forecast)
plot(fdi$ds,fdi$y)
