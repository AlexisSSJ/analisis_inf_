
library(feasts)




tib_df <- tib_df %>% 
  arrange(date) %>% 
  tidyr::fill(Número_de_Comercializadores, .direction = "up" ) %>% 
  tidyr::fill(Transacciones_reportadas  , .direction = "up" ) %>% 
  tidyr::fill(Tipo_de_cambio , .direction = "up" ) %>% 
  tidyr::fill(Transacciones_atípicas , .direction = "up" ) %>% 
  tidyr::fill(`Índice_(MXN/GJ)` , .direction = "up" ) %>%
  tidyr::fill(`Volumen_comercializado_total_(GJ)` , .direction = "up" ) %>%
  tidyr::fill(`Índice_(USD/MBtu)`  , .direction = "up" ) %>% 
  tidyr::fill(pob_eco_act  , .direction = "down" ) 



ggcorrplot(corr_matrix,lab = TRUE) +
  ggtitle("Correlation Heatmap") 


tib_df<- tib_df %>%
  arrange(date) %>% 
  tidyr::fill(Número_de_Comercializadores, .direction = "up" ) %>% 
  tidyr::fill(Transacciones_reportadas  , .direction = "up" ) %>% 
  tidyr::fill(Tipo_de_cambio , .direction = "up" ) %>% 
  tidyr::fill(Transacciones_atípicas , .direction = "up" ) %>% 
  tidyr::fill(`Índice_(MXN/GJ)` , .direction = "up" ) %>%
  tidyr::fill(`Volumen_comercializado_total_(GJ)` , .direction = "up" ) %>%
  tidyr::fill(`Índice_(USD/MBtu)`  , .direction = "up" ) %>% 
  tidyr::fill(pob_eco_act  , .direction = "down" ) %>% 
  ggplot(aes(x =Tipo_de_cambio, y = close_mean_price))+
  geom_point()






tib_df <- df_ts %>% 
  pivot_wider(values_from=value, names_from=indice) %>% 
  as_tibble() %>% filter(between(year(date), 2017, 2023))




tib_df %>%
  full_join(IPC, by = join_by(date)) %>% 
  full_join(SP500, by = join_by(date)) %>% 
  full_join(oro, by = join_by(date)) %>% 
  filter(between(year(date), 2017, 2023)) %>% 
  select(-date) %>% 
  cor() %>%  
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(cols = -var1, names_to = "var2", values_to = "correlation") %>% 
  filter(var1== 'Inflacion',var2 != "Inflacion") %>% 
  ggplot(aes(x= reorder(var2, correlation), y = correlation )) +
  geom_col() +
  geom_text(aes(label = round(correlation,4) ), hjust = -0.25)+
  ylim(0,0.3)+
  labs(x = "", y = "Correlación", title = "Correlación TS inflación vs otras")+
  coord_flip()






# GGally::ggpairs(columns =2:13)
# pivot_longer(cols = -date) %>% 
# as_tsibble(index = date, key = name) %>% autoplot()
# 



tib_df %>%
  full_join(IPC, by = join_by(date)) %>% 
  full_join(SP500, by = join_by(date)) %>% 
  full_join(oro, by = join_by(date)) %>% 
  filter(between(year(date), 2017, 2023)) %>% 
  filter(if_any(var_prom_oro_mensual, is.na)) %>% 
  glimpse()





SP500 %>% as_tsibble() %>% autoplot(diff2_mean_SP500)
