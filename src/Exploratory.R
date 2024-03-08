library(tidyverse)
library(tsibble)

library(stringr) # For regular expression functions
library(lubridate)
library(readr)


pob_act <- read_csv("data/Indicadores20240306174031- Población_activa.csv",
                    skip = 4) %>% 
  slice(0: 76) %>% 
  mutate(date = yearmonth(Periodos), indice = 'pob_eco_act') %>% 
  select(-Periodos,value = pob_act )
  


##
## EXTRAER POBLACIÓN TOTAL MÉXICO
##

pob_tot = read_csv(file = "data/mexico-population-2024-03-07.csv", skip = 15) %>% 
  filter(between( year(date),2000,2023)) %>% 
  mutate(ym = yearmonth(date))  


pob_tot %>% 
  as.data.frame() %>% 
  mutate(date =date +days(1)) %>% 
  pull(date ) ->x

pob_tot %>% 
  as.data.frame() %>% 
  mutate(date =date +days(1)) %>% 
  pull(Population ) ->y

f <- approxfun(x , y)


tot_periods <- seq.Date(as.Date("2001-01-1"), as.Date('2024-1-1') ,by = 'month')


pob_total <- tibble(
  date = as.Date(tot_periods),
  value =f(tot_periods)
  ) %>%
  mutate(date = yearmonth(date), indice = 'Pob_total_pais')

remove(pob_tot)






inf <- read_excel('data/infl_bnmx.xlsx',col_types  = c('date', 'numeric'),skip = 17) %>% 
  mutate(date = yearmonth(Fecha), indice ='Inflacion') %>% 
  select(-Fecha, value = SP30577) %>% filter(year(date)>2000)



dolar <- read_csv('data/Datos históricos USD_MXN.csv') %>% 
  mutate(date = dmy(str_replace_all(Fecha, "[.]",'/')), indice = 'Cierre_prom_dolar') %>% 
  select(Cierre, date, indice ) %>% 
  group_by(yearmonth(date)) %>% 
  summarise(value = mean(Cierre) )%>% 
  mutate(date = `yearmonth(date)`, indice = 'TC_Dolar') %>% 
  select(- `yearmonth(date)`)


all_Data <-  pob_total %>% 
  rbind(pob_act) %>% 
  rbind(dolar) %>% 
  rbind(inf) %>%  
  filter(year(date)>=2010) %>% as_tsibble(key = indice ,index = date)

all_Data %>% 
  pivot_wider(values_from=value, names_from=k) %>% 
  mutate(pob_eco_act_prop =  pob_eco_act/Pob_total_pais) %>% 
  GGally::ggpairs(columns = 2:6)




all_Data %>% autoplot()

pob_act %>% pivot_wider(values_from=value, names_from=k) %>% 
  as_tsibble() %>% autoplot(value)


pob_act %>% filter(is.na(value))
