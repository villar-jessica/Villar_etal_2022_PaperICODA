##############################
# Figura 1: evolução dos óbitos por estado
#   Opção 1: Fazer isso com 0% da população brasileira vacinada; 25%; 50% e 75%
#   Opção 2: Fazer isso com 0% da população do estado vacinada; 25%; 50% e 75%
##############################

library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(cowplot) # para plotar os gráficos lado a lado
library(geobr)

##############################
# lendo dados de óbitos e cobertura vacinal
##############################
df <- read.csv('BR.csv')


##############################
# definindo configurações do mapa
##############################
states <- read_rds('shp_state_brazil_map.rds')
no_axis <- theme(axis.title = element_blank(),
                 axis.text = element_blank(),
                 axis.ticks = element_blank())

estados <- left_join(states, df, by = c('abbrev_state' = 'uf'))

##############################
#   Opção 1: Fazer isso com 0% da população brasileira vacinada; 25%; 50% e 75%
# lendo arquivo com a cobertura vacinal do Brasil
##############################
df_cob_vacinal_BR <- read.csv('cob_vacinal_BR.csv',
                              col.names = c('data', 'cob_vacinal'))


##############################
# 0%
##############################
df_0 <- estados %>%
          filter(data == '2021-01-16')


##############################
# 25%
##############################
df_cob_vacinal_BR_25 <- df_cob_vacinal_BR %>%
                          filter(cob_vacinal > 0.249 & cob_vacinal < 0.25002)

df_25 <- estados %>%
          filter(data == df_cob_vacinal_BR_25$data)



##############################
# 50%
##############################
df_cob_vacinal_BR_50 <- df_cob_vacinal_BR %>%
                          filter(cob_vacinal > 0.499 & cob_vacinal < 0.50002)

df_50 <- estados %>%
          filter(data == df_cob_vacinal_BR_50$data)



##############################
# 75%
##############################
df_cob_vacinal_BR_75 <- df_cob_vacinal_BR %>%
                          filter(cob_vacinal > 0.749 & cob_vacinal < 0.7519)

df_75 <- estados %>%
          filter(data == df_cob_vacinal_BR_75$data)



##############################
# plotando os mapas
# 0%
##############################
map_0 <- ggplot() +
  geom_sf(data = df_0, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '0% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.2)) +
  no_axis 


##############################
# 25% 
##############################
map_25 <- ggplot() +
  geom_sf(data = df_25, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '25% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis


##############################
# 50%
##############################
map_50 <- ggplot() +
  geom_sf(data = df_50, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '50% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis 


##############################
# 75%
##############################
map_75 <- ggplot() +
  geom_sf(data = df_75, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '75% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis


plot_grid(map_0, map_25, map_50, map_75,
          nrow = 1,
          labels = 'Deaths per 100k habitants',
          label_x = -0.4)


##############################
#   Opção 2: Fazer isso com 0% da população do estado vacinada; 25%; 50% e 75%
# separando a cobertura vacinal por estados em df onde a cobertura estava em 0%, 25%, 50% e 75%
##############################
df_0 <- estados %>%
          filter(data == '2021-01-16')

df_25 <- estados %>%
          filter(cob_vacinal > 0.249 & cob_vacinal < 0.2599) %>%
          arrange(data,cob_vacinal)
df_25 <- df_25[match(unique(df_25$name_state), df_25$name_state),]

df_50 <- estados %>%
          filter(cob_vacinal > 0.499 & cob_vacinal < 0.5099) %>%
          arrange(data,cob_vacinal)
df_50 <- df_50[match(unique(df_50$name_state), df_50$name_state),]


# nem todos os estados atingiram 75% da cobertura vacinal
df_75 <- df %>%
          filter(cob_vacinal > 0.7499) %>%
          arrange(data,cob_vacinal)
df_75 <- df_75[match(unique(df_75$uf), df_75$uf),]
df_75 <- left_join(states,df_75, by = c("abbrev_state" = "uf"))

##############################
# plotando os mapas
# 0%
##############################
map_0 <- ggplot() +
  geom_sf(data = df_0, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '0% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = c(0.2, 0.2)) +
  no_axis  +
  labs(size = 15,
       caption = "") # acrescentando só pra ficar igual ao gráfico de 75% que tem anotação


##############################
# 25% 
##############################
map_25 <- ggplot() +
  geom_sf(data = df_25, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '25% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis +
  labs(size = 15,
       caption = "") # acrescentando só pra ficar igual ao gráfico de 75% que tem anotação


##############################
# 50%
##############################
map_50 <- ggplot() +
  geom_sf(data = df_50, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '50% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis +
  labs(size = 15,
       caption = "") # acrescentando só pra ficar igual ao gráfico de 75% que tem anotação


##############################
# 75%
##############################
map_75 <- ggplot() +
  geom_sf(data = df_75, 
          aes_string(fill = 'obitos_100k'), 
          color = NA) +
  labs(title = '75% vaccination coverage') +
  scale_fill_distiller(palette = 'BuPu',
                       direction = 1,
                       limits = c(0, max(df$obitos_100k))) +
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = 'none') +
  no_axis +
  labs(size = 15,
       caption = "*The states in gray are states that have not reached 75% vaccination coverage")


plot_grid(map_0, map_25, map_50, map_75,
          nrow = 1,
          labels = 'COVID-19 deaths per 100k habitants',
          label_x = -0.6)
  
