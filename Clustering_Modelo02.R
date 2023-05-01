###############################################################################
#           Criar as Variárias para Análise de Agrupamento (Clustering)       #
###############################################################################

#
#--------------------- Instaçalação e Carregamento dos Pacotes -----------------
# Listar os pacotes que serão utilizados

pacotes <- c("tidyverse", "readxl","writexl","cluster","dendextend","factoextra",
             "fpc","gridExtra","knitr","kableExtra","lubridate","dbscan")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

rm(pacotes)

#
#----------------- Carregar datasets devedores  --------------------------------
#

load(file = "creditos_cluster.RData")

creditos_bkp <- creditos

################################################################################
#
#             Aplicação Método Agrupamento não Hierarquico K-means
#
################################################################################

#
#---------------- Seleção variáveis pra criação dos clusters -------------------
#

creditos <- creditos_bkp

var_cluster <- c(11,13,14,15,16,17)

#
#----------------- Padronização/Normalização das Variáveis - z-score -----------
#

# Padronização/Normalização por meio da função scale()

creditos[,var_cluster] <- scale(creditos[,var_cluster])

#
#----------------- Cluster Não Hierárquico K-means -----------------------------
#

# VERIFICANDO ELBOW 
fviz_nbclust(creditos[,var_cluster], kmeans, method = "wss")

# Modelo Kmeans

creditos_kmeans <- kmeans(creditos[,var_cluster], centers = 6)

# Visualizar os clusters
fviz_cluster(creditos_kmeans, geom = "point",  data = creditos[,var_cluster]) + ggtitle("K-MEANS - número de grupos = 6")

  table(creditos_kmeans[["cluster"]])

# Extrair os clusters do modelo

grupos <- data.frame(creditos_kmeans$cluster)
colnames(grupos) <- "grupos"

resultado_kmeans <- cbind(creditos_bkp, grupos)

# creditos_pagos_orig <- cbind(creditos_pagos_orig,grupos)

# Analisar o resultado

grupo1 <- filter(resultado_kmeans, grupos == 1)
grupo2 <- filter(resultado_kmeans, grupos == 2)
grupo3 <- filter(resultado_kmeans, grupos == 3)
grupo4 <- filter(resultado_kmeans, grupos == 4)
grupo5 <- filter(resultado_kmeans, grupos == 5)
grupo6 <- filter(resultado_kmeans, grupos == 6)
