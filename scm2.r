library(tidyverse)
library(readxl)
library(tidysynth)
library(scales)
library(patchwork)
library(ggrepel)
## Reading data

# Synthetic control
# Here, we normalize the data by dividing with the first observation

dats<-read_excel("data.xlsx",sheet="price3")
weks<-dats[1,]
dat<-tibble(sweep(dats, 2, as.numeric(dats[1, ]), "/"))
dat<-tibble(dat*100)
dat<-dat|>mutate(mo=seq(as.Date("2018/7/1"), by = "month", length.out = 81))
beras<-dat|>select(!c(gula,sapi))|>
  pivot_longer(!mo,names_to = "what",values_to = "val")
#beras<-dat|>select(!c(gula,sapi))
#beras<-log(beras)
#beras<-beras|>mutate(mo=seq(as.Date("2018/7/1"), by = "month", length.out = 81))
#beras<-beras|>pivot_longer(!mo,names_to = "what",values_to = "val")
gula<-dat|>select(!c(beras,sapi))|>
  pivot_longer(!mo,names_to = "what",values_to = "val")
sapi<-dat|>select(!c(gula,beras))|>
  pivot_longer(!mo,names_to = "what",values_to = "val")
#dat<-dat|>pivot_longer(!mo,names_to = "what",values_to = "val")



## Synth
synth_beras <-
  beras |> 
  #mutate(val=log(wval)) |>
  # initiate the synthetic control object
  synthetic_control(outcome = val, # outcome
                    unit = what, # unit index in the panel data
                    time = mo, # time index in the panel data
                    i_unit = "beras", # unit where the intervention occurred
                    i_time = as.Date("2022-01-01"), # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  )

# Using Lags of dependent var to be predictors
synth_beras <- synth_beras |>
  generate_predictor(time_window = as.Date("2019-01-01"),
                     val1 = val) |>
  generate_predictor(time_window = as.Date("2021-01-01"),
                     val2 = val) |>
  generate_predictor(time_window = as.Date("2022-01-01"),
                     val3 = val)

# Generate the fitted weights for the synthetic control
synth_beras <- synth_beras |>
  generate_weights(optimization_window = seq(as.Date("2018/7/1"), by = "month", length.out = 42), # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  )

synth_beras <- synth_beras |>
  generate_control()

sb<-synth_beras|>plot_trends()
sb<-sb+theme_classic()
wb<-synth_beras|>plot_weights()
wb<-wb+theme_classic()
ggsave("synth_beras.png",sb,width = 10, height = 6, dpi = 300)
ggsave("weight_beras.png",wb,width = 10, height = 6, dpi = 300)
## Synth price
synth_gula <-
  gula |>
  #mutate(val=log(wval)) |>
  # initiate the synthetic control object
  synthetic_control(outcome = val, # outcome
                    unit = what, # unit index in the panel data
                    time = mo, # time index in the panel data
                    i_unit = "gula", # unit where the intervention occurred
                    i_time = as.Date("2022-01-01"), # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  )

# Using Lags of dependent var to be predictors
synth_gula <- synth_gula |>
  generate_predictor(time_window = as.Date("2019-01-01"),
                     val1 = val) %>%
  generate_predictor(time_window = as.Date("2021-01-01"),
                     val2 = val) %>%
  generate_predictor(time_window = as.Date("2022-01-01"),
                     val3 = val)

# Generate the fitted weights for the synthetic control
synth_gula <- synth_gula |>
  generate_weights(optimization_window = seq(as.Date("2018/7/1"), by = "month", length.out = 42), # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  )

synth_gula <- synth_gula |>
  generate_control()
sb<-synth_gula|>plot_trends()
sb<-sb+theme_classic()
wb<-synth_gula|>plot_weights()
wb<-wb+theme_classic()

ggsave("weight_gula.png",wb,width = 10, height = 6, dpi = 300)
ggsave("synth_gula.png",sb,width = 10, height = 6, dpi = 300)

## Synth
synth_sapi <-
  sapi |> 
  #mutate(val=log(wval)) |>
  # initiate the synthetic control object
  synthetic_control(outcome = val, # outcome
                    unit = what, # unit index in the panel data
                    time = mo, # time index in the panel data
                    i_unit = "sapi", # unit where the intervention occurred
                    i_time = as.Date("2022-01-01"), # time period when the intervention occurred
                    generate_placebos=T # generate placebo synthetic controls (for inference)
  )

# Using Lags of dependent var to be predictors
synth_sapi <- synth_sapi |>
  generate_predictor(time_window = as.Date("2019-01-01"),
                     val1 = val) %>%
  generate_predictor(time_window = as.Date("2021-01-01"),
                     val2 = val) %>%
  generate_predictor(time_window = as.Date("2022-01-01"),
                     val3 = val)

# Generate the fitted weights for the synthetic control
synth_sapi <- synth_sapi |>
  generate_weights(optimization_window = seq(as.Date("2018/7/1"), by = "month", length.out = 42), # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 1,bound_ipop = 6 # optimizer options
  )

synth_sapi <- synth_sapi |>
  generate_control()
sb<-synth_sapi|>plot_trends()
sb<-sb+theme_classic()
wb<-synth_sapi|>plot_weights()
wb<-wb+theme_classic()

ggsave("weight_sapi.png",wb,width = 10, height = 6, dpi = 300)
ggsave("synth_sapi.png",sb,width = 10, height = 6, dpi = 300)

