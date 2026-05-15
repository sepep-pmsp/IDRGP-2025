library(dplyr)
pasta_dados <- "dados"
arquivo_base_central <- file.path(pasta_dados, "tabela_conferencia_completa.xlsx")
df <- readxl::read_excel(arquivo_base_central)
print(names(df))
print(head(df))
