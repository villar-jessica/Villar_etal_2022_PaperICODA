##############################
# Figura 2: 3 gráficos mostrando vacinações por clusters
##############################

library(dplyr)
library(ggplot2)
library(cowplot) # para plotar os gráficos lado a lado
library(RColorBrewer)

##############################
# lendo dados de óbitos e cobertura vacinal
##############################
df <- read.csv('BR.csv')
df$data <- as.Date(df$data)

##############################
# criando coluna de cluster
##############################
df_w_cluster <- df %>%
                  mutate(cluster = case_when(uf == 'AC' ~ 1,
                                             uf == 'AL' ~ 0,
                                             uf == 'AM' ~ 1,
                                             uf == 'AP' ~ 1,
                                             uf == 'BA' ~ 0,
                                             uf == 'CE' ~ 0,
                                             uf == 'DF' ~ 0,
                                             uf == 'ES' ~ 0,
                                             uf == 'GO' ~ 0,
                                             uf == 'MA' ~ 1,
                                             uf == 'MG' ~ 0,
                                             uf == 'MS' ~ 2,
                                             uf == 'MT' ~ 0,
                                             uf == 'PA' ~ 1,
                                             uf == 'PB' ~ 0,
                                             uf == 'PE' ~ 0,
                                             uf == 'PI' ~ 0,
                                             uf == 'PR' ~ 0,
                                             uf == 'RJ' ~ 0,
                                             uf == 'RN' ~ 0,
                                             uf == 'RO' ~ 0,
                                             uf == 'RR' ~ 1,
                                             uf == 'RS' ~ 0,
                                             uf == 'SC' ~ 0,
                                             uf == 'SE' ~ 0,
                                             uf == 'SP' ~ 2,
                                             uf == 'TO' ~ 0))


##############################
# criando visualização de vacinação por cluster
##############################

##############################
# cluster 0
##############################
df_w_cluster_0 <- filter(df_w_cluster, cluster == 0)
g0 <- ggplot(data = df_w_cluster_0,
            aes(x = data, y = doses_100k_mm14, group = uf, color = uf)) +
        geom_line(size = 0.71) +
        ggtitle('Cluster 0') +
        xlab("") +
        ylab("Vaccinations per 100k habitants") +
        ylim(c(0,1000)) +
        scale_color_manual(values = colorRampPalette(brewer.pal(9,name = 'Reds'))(length(unique(df_w_cluster_0$uf))))+
        theme(legend.text = element_text(size = 10),
              legend.key.size = unit(0.3, 'cm')) +
        scale_x_date(date_labels = "%Y %b %d")


##############################
# cluster 1
##############################
df_w_cluster_1 <- filter(df_w_cluster, cluster == 1)
g1 <- ggplot(data = df_w_cluster_1,
            aes(x = data, y = doses_100k_mm14, group = uf, color = uf)) +
        geom_line(size = 0.71) +
        ggtitle('Cluster 1') +
        xlab("") +
        ylab("Vaccinations per 100k habitants") +
        ylim(c(0,1000)) +
        scale_colour_brewer(palette = "PuBu") +
        scale_x_date(date_labels = "%Y %b %d")


##############################
# cluster 2
##############################
df_w_cluster_2 <- filter(df_w_cluster, cluster == 2)
g2 <- ggplot(data = df_w_cluster_2,
             aes(x = data, y = doses_100k_mm14, group = uf, color = uf)) +
        geom_line(size = 0.71) +
        ggtitle('Cluster 2') +
        xlab("") +
        ylab("Vaccinations per 100k habitants") +
        ylim(c(0,1000)) +
        scale_color_manual(values=c("seagreen4","seagreen3")) +
        scale_x_date(date_labels = "%Y %b %d")


plot_grid(g0, g1, g2,
          nrow = 3,
          ncol = 1)
