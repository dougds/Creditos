###############################################################################
#           Modelo Regressão Logística Multinomial                            #
###############################################################################


#
#--------------------- Instaçalação e Carregamento dos Pacotes -----------------
# Listar os pacotes que serão utilizados

pacotes <- c("plotly","tidyverse","knitr","kableExtra","fastDummies","rgl","car",
  "reshape2","jtools","lmtest","caret","pROC","ROCR","nnet","magick",
  "cowplot", "readxl","writexl","data.table","PerformanceAnalytics","correlation")

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
#--------------------- Importar dataframe --------------------------------------
#

load("Dados_RLM.RData")


#               Detalhamento das variáveis:

# ID_Devedor:   código único de identificação do devedor;
#   Tipo da Variável: caractere

# ID_Credito:   código único de identificação do crédito;
#   Tipo da Variável: caractere

# VT_GRM:       valor principal (original) do crédito, sem correções e
#                 atualizações financeiras;
#   Tipo da Variável: numérica

# Situacao:     situação do crédito. Pode assumir: Não Arrecadado, Arrecadado Antes
#                 das Ações de Cobrança e Arrecadado Após Alguma Ação de Cobrança;
#   Tipo da Variável: Fator

# Parcelamento: quantidade de parcelamentos em que o crédito foi submetido;
#   Tipo da Variável: numérica

# Porte:        porte da empresa (CNPJ) registro na Receita Federal. Pode assumir:
#                 01 - Micro Empresa;
#                 03 - Empresa de Pequeno Porte;
#                 05 - Demais;
#   Tipo da Variável: Fator

# Transito:     ano em que ocorreu o Trânsito em Julgado do crédito;
#   Tipo da Variável: numérica


# Duracao:      quantidade de dias decorrido entre a data do Trânsito em Julgado
#                 e a data da situação (data em que ocorreu a arrecadação ou,
#                 para créditos não arrecadados, a data de coleta dos dados - 
#                 01/03/2023)
#   Tipo da Variável: númerica


#
##############################################################################
#               OBSERVAÇÃO DA BASE DE DADOS data_multi                       #
##############################################################################

#Visualizando a base de dados

data_multi %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Estatísticas descritivas univariadas da base de dados
summary(data_multi)

##############################################################################
#               ESTIMAÇÃO DO MODELO LOGÍSTICO MULTINOMIAL                    #
##############################################################################

# Determinando a categoria de referência --------------------------------------

data_multi$Situacao <- relevel(data_multi$Situacao, 
                                        ref = "Arrecadado Antes das Ações de Cobrança")

#Estimação do modelo - função multinom do pacote nnet -------------------------

modelo_multinomial <- multinom(formula = Situacao ~ VT_GRM + Parcelamento +
                                 Porte + Duracao, data = data_multi)

# Parâmetros do modelo ---------------------------------------------------------

summary(modelo_multinomial)

# O relatório é dividido em duas partes: 'Coefficients' e 'Std. Errors'. 
# Cada linha da seção 'Coefficients' informa um logito para cada categoria da
# variável dependente, com exceção da categoria de referência. Já a seção 
# 'Std. Errors' informa o erro-padrão de cada parâmetro em cada logito.

# LL do modelo -----------------------------------------------------------------
logLik(modelo_multinomial)

# Definir a função Qui2 para se extrair a estatística geral do modelo ----------

Qui2 <- function(x) {
  maximo <- logLik(x)
  minimo <- logLik(update(x, ~1, trace = F))
  Qui.Quadrado <- -2*(minimo - maximo)
  pvalue <- pchisq(Qui.Quadrado, df = 1, lower.tail = F)
  df <- data.frame()
  df <- cbind.data.frame(Qui.Quadrado, pvalue)
  return(df)
}

#Estatística geral do modelo multinomial ---------------------------------------

Qui2(modelo_multinomial)

# Estatística z de Wald -------------------------------------------------------


# Para calcular as estatísticas z de Wald, há que se dividir os valores da 
# seção 'Coefficients' pelos valores da seção 'Std. Errors.' do modelo multinomial   

zWald_modelo_atrasado <- (summary(modelo_multinomial)$coefficients / 
                            summary(modelo_multinomial)$standard.errors)

zWald_modelo_atrasado

# Cálculo p-values dos parâmetros ----------------------------------------------

# Os valores das probabilidades associadas às abscissas de uma distribuição 
# normal-padrão é dada pela função pnorm(), considerando os valores em módulo 
# - abs().
# Em seguida, multiplica-se por dois os valores obtidos para considerar os dois
# lados da distribuição normal padronizada (distribuição bicaudal).

round((pnorm(abs(zWald_modelo_atrasado), lower.tail = F) * 2), 4)



##############################################################################
#                   A EFETIVIDADE GERAL DO MODELO                            #
##############################################################################

# Adicionar a coluna Predicao, contendo os eventos apontados pelo modelo -------
#    multinomial, à base de dados ----------------------------------------------


data_multi$Predicao <- predict(modelo_multinomial, 
                                        newdata = data_multi, 
                                        type = "class")

# Visualizar a nova base de dados com a variável "Predicao" 

data_multi %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# fixar a base de dados para os próximos comandos

attach(data_multi)

# Tabela de Classificação das ocorrências reais com as predições ---------------

EGM <- as.data.frame.matrix(table(Situacao, Predicao))

#Visualizando a tabela EGM
EGM %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

#Eficiência global do modelo ---------------------------------------------------

acuracia <- (round((sum(diag(table(Situacao, Predicao))) / 
                      sum(table(Situacao, Predicao))), 2))

acuracia

detach(data_multi)


##############################################################################
#                 Prevendo a Situação do Crédito                             #
##############################################################################

# Qual a probabilidade média da situação do crédito para cada categoria da
# variável dependente, se o valor original do crédito é de R$ 20.000,00, consta
# histórico de 2 parcelamentos, o Porte da Empresa é "5" e Duração igual a 
# 543 dias


predict(modelo_multinomial, 
        data.frame(VT_GRM = 20000, Parcelamento = 2, Porte= "5", Duracao = 543), 
        type = "probs") # types = "probs" -> retorna a probabilidade

# Qual a categoria da variável dependente para o cenário anterio

predict(modelo_multinomial, 
        data.frame(VT_GRM = 20000, Parcelamento = 2, Porte= "5", Duracao = 543), 
        type = "class") # types = "class" -> retorna a categoria da variável dependente
