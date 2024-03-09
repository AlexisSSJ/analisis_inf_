
pob_act <- read_csv("data/Indicadores20240306174031- Población_activa.csv",
                    skip = 4) %>% 
  slice(0: 76) %>% 
  separate(Periodos, into = c('Year', 'Q'),sep = "/") %>% 
  mutate(
    Q = str_replace_all(Q ,c('01' = "Jan",
                             '02' = "Apr",
                             '03' = "Jun",
                             '04' = "Sep")),
  ) %>% unite('date', c(Year, Q), sep = " ") %>% 
  mutate(date = yearmonth(date),
         indice = 'pob_eco_act')
  
f <- approxfun( as.Date(pob_act$date), pob_act$pob_act)


r = tibble(
  date = seq.Date( as.Date(min(pob_act$date)),as.Date( max(pob_act$date)) +months(3), by = 'month')
)

pob_act <- pob_act %>% 
  full_join(r,by =join_by(date)) %>% 
  arrange(date) %>% 
  mutate(
    value = f(date),
    indice = 'pob_eco_act',
    date = yearmonth(date)) %>% 
  select(-pob_act) %>% 
  as_tsibble(index= date, key = indice)





cache("pob_act")

##
## EXTRAER POBLACIÓN TOTAL MÉXICO
##

pob_tot = read_csv(file = "data/mexico-population-2024-03-07.csv", skip = 15) %>% 
  filter(between( year(date),2000,2023)) %>% 
  mutate(ym = yearmonth(date))  


pob_tot %>% 
  as.data.frame() %>% 
  mutate(date =date +days(1)) %>% 
  pull(date ) -> x

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
cache("pob_total")

remove(pob_tot,x,y)






inflation_data <- read_excel('data/infl_bnmx.xlsx',col_types  = c('date', 'numeric'),skip = 17) %>% 
  mutate(date = yearmonth(Fecha), indice ='Inflacion') %>% 
  select(-Fecha, value = SP30577) %>% filter(year(date)>2000)

cache("inflation_data")

dolar_tc_prom <- read_csv('data/Datos históricos USD_MXN.csv') %>% 
  mutate(
    Fecha = dmy(Fecha), 
    date = yearmonth(Fecha ), 
    year = year( Fecha),
    month = month(Fecha) 
    ) %>% 
  filter(Fecha < "2024-01-01") %>% 
  group_by(date) %>% 
  dplyr::summarise(
    close_mean_price = mean(Cierre) ,
    n_ = n()
    ) %>% 
  select(value = close_mean_price, date ) %>% 
  mutate( indice = 'close_mean_price') 
cache("dolar_tc_prom")





IPGN <- read_excel("data/IPGN.xlsx") %>% mutate(
  Mes = as.integer(Mes %>% str_replace_all(c(
    "Enero" = "1", 
    "Febrero" = "2",
    "Marzo" = "3",
    "Abril" = "4",
    "Mayo" = "5",
    "Junio" = "6",
    "Julio" = "7",
    "Agosto" = "8",
    "Septiembre" = "9",
    "Octubre" = "10",
    "Noviembre" = "11",
    "Diciembre" = "12"
    ))),
  date = yearmonth(make_datetime(Año, Mes))
  ) %>%
  select(-c(Tipo, Mes,Año)) %>% 
  pivot_longer(0:7) %>% as_tibble(key = name, index = date) %>% 
  mutate(
    name = name %>% str_replace_all('\\s', '_')
  )


cache("IPGN")

df_ts <-  pob_total %>% 
  rbind(pob_act) %>% 
  rbind(dolar_tc_prom) %>% 
  rbind(IPGN %>% dplyr::rename(indice = name)) %>% 
  rbind(inflation_data) %>%  
  filter(year(date)>=2010) %>% as_tsibble(key = indice ,index = date)

cache("df_ts",depends = c("dolar_tc_prom","inflation_data","pob_total","pob_act", "IPGN"))



tib_df <- df_ts %>% 
  pivot_wider(values_from=value, names_from=indice) %>% 
  as_tibble() %>% filter(between(year(date), 2017, 2023))


























