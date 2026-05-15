
######################### Teste de Wilcoxon #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr") # Instalação do pacote caso não esteja instalado
library(dplyr)                                # Carregamento do pacote
if(!require(dplyr)) install.packages("rstatix") # Instalação do pacote caso não esteja instalado
library(rstatix)                                # Carregamento do pacote

# Passo 2: Carregar o banco de dados

# Importante: selecionar o diretório de trabalho (working directory)
# Isso pode ser feito manualmente: Session > Set Working Directory > Choose Directory
# Ou usando a linha de código abaixo:
# setwd("C:/Users/ferna/Desktop")

dados <- read.csv('Banco de Dados 4.csv', sep=";", dec=",", stringsAsFactors = T,
                  fileEncoding = "latin1") %>%
  dplyr::rename(Convulsoes_PT = Convulsões_PT, Convulsoes_S1 = Convulsões_S1,
         Convulsoes_S6 = Convulsões_S6, Genero = Gênero)

View(dados)                                # Visualização dos dados em janela separada
glimpse(dados)                             # Visualização de um resumo dos dados


# Passo 3: Realização do teste de Wilcoxon

wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1, paired = TRUE)


# Observação:
  # O teste bicaudal é o default; caso deseje unicaudal, necessário incluir:
    # alternative = "greater" ou alternative = "less"
  # Exemplo: wilcox.test(dados$Convulsoes_PT, dados$Convulsoes_S1,
              # paired = TRUE, alternative="greater")
    # Nesse caso, o teste verificará se é a mediana das Convulsoes_PT é maior que a
              # mediana das Convulsoes_S1


# Passo 4: Análise descritiva dos dados

dados$dif <- dados$Convulsoes_PT - dados$Convulsoes_S1
View(dados)

dados %>% get_summary_stats(Convulsoes_PT, Convulsoes_S1, dif, type = "median_iqr")

# Dados paramétricos?
# dados %>% group_by(Posicao_Sala) %>% 
#  get_summary_stats(Nota_Biol, Nota_Hist, Nota_Fis, type = "mean_sd")
