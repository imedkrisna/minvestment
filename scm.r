library(tidyverse)
library(readxl)
library(tidysynth)
library(scales)
library(patchwork)
library(ggrepel)
## Reading data

# Synthetic control
# https://github.com/edunford/tidysynth
# BKPM Regulation No. 12 of 2009


#ctr<-c("Australia","Austria","Belgium","Brazil","Brunei","Bulgaria","Canada","Chile","China","Colombia","Costa Rica","Cyprus","Denmark","Egypt","Finland","France","Greece","Hungary","Iceland","India","Indonesia","Iran","Ireland","Israel","Italy","Japan","Korea (South)","Laos","Malaysia","Mexico","Myanmar","Netherlands","New Zealand","Nigeria","Norway","Peru","Philippines","Poland","Portugal","Romania","Russia","Saudi Arabia","Singapore","South Africa","Spain","Sweden","Switzerland","Thailand","United Arab Emirates","United Kingdom","United States","Vietnam")
ctr<-c("Brazil","Brunei","Bulgaria","Chile","China","Colombia","Costa Rica","Cyprus","Egypt","Hungary","Iceland","India","Indonesia","Laos","Malaysia","Mexico","Myanmar","Nigeria","Peru","Philippines","Poland","Romania","Russia","Saudi Arabia","South Africa","Thailand","United Arab Emirates","Vietnam")
tag<-read_csv('data/US_Cpi_A_20230707022429.csv')|>filter(Year==1990)|>select(Economy,`Economy Label`)
tag<-tag|>filter(`Economy Label`%in%ctr)
ec<-tag$Economy
ec<-append(ec,c("360","960"))

# FDI
## inflow current flow=="08"
## instick current flow=="09"
## `US$ at current prices in millions` for current
## `US$ at current prices per capita` for per capita
## `Percentage of Gross Domestic Product` for %GDP
## `Percentage of Gross Fixed Capital Formation` for %GFCF
fdi<-read_csv('data/US_FdiFlowsStock.csv')|>filter(Flow=="08")|>filter(Direction==1)
fdi<-fdi|>select(Year,Economy,`US$ at current prices in millions`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#fdi<-fdi[nchar(fdi$Economy)<=3,]
fdi<-fdi|>rename(fdi=`US$ at current prices in millions`)

cpi<-read_csv('data/US_Cpi_A_20230707022429.csv')
cpi<-cpi|>select(Year,Economy,`Annual average growth rate`)
cpi<-cpi|>filter(Economy%in%ec)|>filter(Year>=1990)
#cpi<-cpi[nchar(cpi$Economy)<=3,]
cpi<-cpi|>rename(cpi=`Annual average growth rate`)
idn<-tibble(Year=2003,Economy="360",cpi=6.8) # Data cpi indo 2003 missing
cpi<-rbind(cpi,idn)|>arrange(Year,Economy)

xr<-read_csv('data/US_ExchangeRateCrosstab_20230726120002.csv')|>filter(ForeignEconomy==842)
xr<-xr|>select(Year,Economy,`National currency at current prices`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#xr<-xr[nchar(xr$Economy)<=3,]
xr<-xr|>rename(xr=`National currency at current prices`)

gdp<-read_csv('data/US_GDPTotal.csv')
gdp<-gdp|>select(Year,Economy,`US$ at current prices in millions`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#gdp<-gdp[nchar(gdp$Economy)<=3,]
gdp<-gdp|>rename(gdp=`US$ at current prices in millions`)

pop<-read_csv('data/US_PopTotal.csv')
pop<-pop|>select(Year,Economy,`Absolute value in thousands`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#pop<-pop[nchar(pop$Economy)<=3,]
pop<-pop|>rename(pop=`Absolute value in thousands`)

## exchange rate and inflation stops at 2022. It's not too useful anyway

sin<-inner_join(fdi,gdp,by=c("Year","Economy"))
sin<-inner_join(sin,pop,by=c("Year","Economy"))
#sin<-inner_join(sin,cpi,by=c("Year","Economy"))
#sin<-inner_join(sin,xr,by=c("Year","Economy"))

sin<-sin|>mutate(Economy=replace(Economy,Economy=="960","360"))|>na.omit() # omit 2 indos

# 10M paid-up capital di 2021

sin_out<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit="360",
                    i_time=2021,
                    generate_placebos = T)|>
  generate_predictor(time_window=2004:2015,
  #                   acpi=mean(cpi,na.rm=T),
  #                   axr=mean(xr,na.rm=T),
                     agdp=mean(gdp,na.rm=T),
                     apop=mean(pop,na.rm=T))|>
  generate_predictor(time_window=1995,
                     fdi1=fdi)|>
  generate_predictor(time_window=2005,
                     fdi2=fdi)|>
  generate_predictor(time_window=2015,
                     fdi3=fdi)|>
  generate_weights(optimization_window = 1990:2021,
                   margin_ipop=.02,sigf_ipop=7,bound_ipop=6)|>
  generate_control()


sb<-sin_out|>plot_trends()
sb<-sb+theme_classic()
wb<-sin_out|>plot_weights()
wb<-wb+theme_classic()

## Ganti nama
# fdi = flow, nominal
# fdis = stock, nominal
# fdic = flow, per capita
# fdisc = stock, per capita
# gdp = % GDP
# gfcf = % GFCF
ggsave("fig/weight_fdi.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_fdi.png",sb,width = 10, height = 6, dpi = 300)
