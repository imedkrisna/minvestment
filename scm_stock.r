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
ctr<-c("Brunei","Bulgaria","Chile","China","Colombia","Costa Rica","Egypt","Hungary","Iceland","India","Indonesia","Laos","Malaysia","Mexico","Myanmar","Nigeria","Peru","Philippines","Poland","Romania","Russia","Saudi Arabia","South Africa","Thailand","United Arab Emirates","Vietnam")
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
fdi<-read_csv('data/US_FdiFlowsStock.csv')|>filter(Flow=="09")|>filter(Direction==1)
fdi<-fdi|>select(Year,Economy,`US$ at current prices in millions`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#fdi<-fdi[nchar(fdi$Economy)<=3,]
fdi<-fdi|>rename(fdi=`US$ at current prices in millions`)

## per capita
fdic<-read_csv('data/US_FdiFlowsStock.csv')|>filter(Flow=="09")|>filter(Direction==1)
fdic<-fdic|>select(Year,Economy,`US$ at current prices per capita`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#fdi<-fdi[nchar(fdi$Economy)<=3,]
fdic<-fdic|>rename(fdi=`US$ at current prices per capita`)

## GDP

fdig<-read_csv('data/US_FdiFlowsStock.csv')|>filter(Flow=="09")|>filter(Direction==1)
fdig<-fdig|>select(Year,Economy,`Percentage of Gross Domestic Product`)|>filter(Economy%in%ec)|>filter(Year>=1990)
#fdi<-fdi[nchar(fdi$Economy)<=3,]
fdig<-fdig|>rename(fdi=`Percentage of Gross Domestic Product`)

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


# 10M paid-up capital di 2021

## SCM stock 

sin<-inner_join(fdi,gdp,by=c("Year","Economy"))
sin<-inner_join(sin,pop,by=c("Year","Economy"))
#sin<-inner_join(sin,cpi,by=c("Year","Economy"))
#sin<-inner_join(sin,xr,by=c("Year","Economy"))

sin<-sin|>mutate(Economy=replace(Economy,Economy=="960","360"))|>na.omit() # omit 2 indos


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
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  scale_y_continuous(labels=label_number(scale=1e-3),breaks=pretty_breaks())+
  labs(y="FDI stock (in Billion current USD)",x="",title="")+
  theme_classic()
wb<-sin_out|>plot_weights()
wb<-wb+theme_classic()

## Ganti nama
# fdi = flow, nominal
# fdis = stock, nominal
# fdic = flow, per capita
# fdisc = stock, per capita
# gdp = % GDP
# gfcf = % GFCF
ggsave("fig/weight_stock.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_stock.png",sb,width = 10, height = 6, dpi = 300)

## SCM per capita

sin<-inner_join(fdic,gdp,by=c("Year","Economy"))
sin<-inner_join(sin,pop,by=c("Year","Economy"))
#sin<-inner_join(sin,cpi,by=c("Year","Economy"))
#sin<-inner_join(sin,xr,by=c("Year","Economy"))

sin<-sin|>mutate(Economy=replace(Economy,Economy=="960","360"))|>na.omit() # omit 2 indos


sin_c<-sin|>
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


sb<-sin_c|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI stock per capita in current USD",x="",title="")+
  theme_classic()
wb<-sin_c|>plot_weights()
wb<-wb+theme_classic()

## Ganti nama
# fdi = flow, nominal
# fdis = stock, nominal
# fdic = flow, per capita
# fdisc = stock, per capita
# gdp = % GDP
# gfcf = % GFCF
ggsave("fig/weight_stockc.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_stockc.png",sb,width = 10, height = 6, dpi = 300)

## SCM FDI per GDP

sin<-inner_join(fdig,gdp,by=c("Year","Economy"))
sin<-inner_join(sin,pop,by=c("Year","Economy"))
#sin<-inner_join(sin,cpi,by=c("Year","Economy"))
#sin<-inner_join(sin,xr,by=c("Year","Economy"))

sin<-sin|>mutate(Economy=replace(Economy,Economy=="960","360"))|>na.omit() # omit 2 indos


sin_g<-sin|>
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


sb<-sin_g|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI stock per GDP (%)",x="",title="")+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()


ggsave("fig/weight_stockg.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_stockg.png",sb,width = 10, height = 6, dpi = 300)

# Investment by sector
## ex  = extractive 
## mc   = manufacture capital intensive
## ml   = manufacture labour intensive
## sc   = service capital intensive
## sl   =service labour intensive
sector<-"extractive"
fdi<-fdi|>filter(Economy!=360)|>filter(Economy!=960)
fdis<-read_excel('data/inves_group.xlsx')|>filter(Economy==sector)|>
  filter(Year>=1990)|>filter(Year<=2023)
fdis<-rbind(fdi,fdis)|>arrange(Economy,Year)
gdps<-gdp|>filter(!(Economy==360 & is.na(gdp)))|>filter(!(Economy==960 & is.na(gdp)))|>
  mutate(Economy=case_when(
    Economy=="360"~"extractive",
    Economy=="960"~"extractive",
    TRUE~Economy
  ))
pops<-pop|>filter(!(Economy==360 & is.na(pop)))|>filter(!(Economy==960 & is.na(pop)))|>
  mutate(Economy=case_when(
    Economy=="360"~"extractive",
    Economy=="960"~"extractive",
    TRUE~Economy
  ))
                                                
## SCM Extractive

sin<-inner_join(fdis,gdps,by=c("Year","Economy"))
sin<-inner_join(sin,pops,by=c("Year","Economy"))

sin_ex<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit="extractive",
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


sb<-sin_ex|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI flow (Million Current USD)",x="",title=sector)+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()
ggsave("fig/weight_ex.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_ex.png",sb,width = 10, height = 6, dpi = 300)

## End of extractive
## Start if manufacture capital intensive

# Investment by sector
## ex  = extractive 
## mc   = manufacture capital intensive
## ml   = manufacture labour intensive
## sc   = service capital intensive
## sl   =service labour intensive
sector<-"manufacture capital intensive"
fdi<-fdi|>filter(Economy!=360)|>filter(Economy!=960)
fdis<-read_excel('data/inves_group.xlsx')|>filter(Economy==sector)|>
  filter(Year>=1990)|>filter(Year<=2023)
fdis<-rbind(fdi,fdis)|>arrange(Economy,Year)
gdps<-gdp|>filter(!(Economy==360 & is.na(gdp)))|>filter(!(Economy==960 & is.na(gdp)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))
pops<-pop|>filter(!(Economy==360 & is.na(pop)))|>filter(!(Economy==960 & is.na(pop)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))

## SCM MC

sin<-inner_join(fdis,gdps,by=c("Year","Economy"))
sin<-inner_join(sin,pops,by=c("Year","Economy"))

sin_ex<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit=sector,
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


sb<-sin_ex|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI flow (Million Current USD)",x="",title=sector)+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()
ggsave("fig/weight_mc.png",wb,width = 10, height = 6, dpi = 300)
ggsave("fig/synth_mc.png",sb,width = 10, height = 6, dpi = 300)

## End of manufacture capital intensive
## Start if manufacture labor intensive

# Investment by sector
## ex  = extractive 
## mc   = manufacture capital intensive
## ml   = manufacture labour intensive
## sc   = service capital intensive
## sl   =service labour intensive
sector<-"manufacture labour intensive"
fdi<-fdi|>filter(Economy!=360)|>filter(Economy!=960)
fdis<-read_excel('data/inves_group.xlsx')|>filter(Economy==sector)|>
  filter(Year>=1990)|>filter(Year<=2023)
fdis<-rbind(fdi,fdis)|>arrange(Economy,Year)
gdps<-gdp|>filter(!(Economy==360 & is.na(gdp)))|>filter(!(Economy==960 & is.na(gdp)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))
pops<-pop|>filter(!(Economy==360 & is.na(pop)))|>filter(!(Economy==960 & is.na(pop)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))


sin<-inner_join(fdis,gdps,by=c("Year","Economy"))
sin<-inner_join(sin,pops,by=c("Year","Economy"))

sin_ex<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit=sector,
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


sb<-sin_ex|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI flow (Million Current USD)",x="",title=sector)+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()
ggsave("fig/weight_ml.png",wb,width = 10, height = 6, dpi = 300) ## Change name
ggsave("fig/synth_ml.png",sb,width = 10, height = 6, dpi = 300) ## Change name

# Investment by sector
## ex  = extractive 
## mc   = manufacture capital intensive
## ml   = manufacture labour intensive
## sc   = service capital intensive
## sl   =service labour intensive
sector<-"service capital intensive"
fdi<-fdi|>filter(Economy!=360)|>filter(Economy!=960)
fdis<-read_excel('data/inves_group.xlsx')|>filter(Economy==sector)|>
  filter(Year>=1990)|>filter(Year<=2023)
fdis<-rbind(fdi,fdis)|>arrange(Economy,Year)
gdps<-gdp|>filter(!(Economy==360 & is.na(gdp)))|>filter(!(Economy==960 & is.na(gdp)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))
pops<-pop|>filter(!(Economy==360 & is.na(pop)))|>filter(!(Economy==960 & is.na(pop)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))


sin<-inner_join(fdis,gdps,by=c("Year","Economy"))
sin<-inner_join(sin,pops,by=c("Year","Economy"))

sin_ex<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit=sector,
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


sb<-sin_ex|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI flow (Million Current USD)",x="",title=sector)+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()
ggsave("fig/weight_sc.png",wb,width = 10, height = 6, dpi = 300) ## Change name
ggsave("fig/synth_sc.png",sb,width = 10, height = 6, dpi = 300) ## Change name

# Investment by sector
## ex  = extractive 
## mc   = manufacture capital intensive
## ml   = manufacture labour intensive
## sc   = service capital intensive
## sl   =service labour intensive
sector<-"service labour intensive"
fdi<-fdi|>filter(Economy!=360)|>filter(Economy!=960)
fdis<-read_excel('data/inves_group.xlsx')|>filter(Economy==sector)|>
  filter(Year>=1990)|>filter(Year<=2023)
fdis<-rbind(fdi,fdis)|>arrange(Economy,Year)
gdps<-gdp|>filter(!(Economy==360 & is.na(gdp)))|>filter(!(Economy==960 & is.na(gdp)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))
pops<-pop|>filter(!(Economy==360 & is.na(pop)))|>filter(!(Economy==960 & is.na(pop)))|>
  mutate(Economy=case_when(
    Economy=="360"~sector,
    Economy=="960"~sector,
    TRUE~Economy
  ))


sin<-inner_join(fdis,gdps,by=c("Year","Economy"))
sin<-inner_join(sin,pops,by=c("Year","Economy"))

sin_ex<-sin|>
  synthetic_control(outcome = fdi,
                    unit=Economy,
                    time=Year,
                    i_unit=sector,
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


sb<-sin_ex|>plot_trends()
sb<-sb+scale_x_continuous(breaks = seq(1990,2023,3))+
  labs(y="FDI flow (Million Current USD)",x="",title=sector)+
  theme_classic()
wb<-sin_g|>plot_weights()
wb<-wb+theme_classic()
ggsave("fig/weight_sl.png",wb,width = 10, height = 6, dpi = 300) ## Change name
ggsave("fig/synth_sl.png",sb,width = 10, height = 6, dpi = 300) ## Change name