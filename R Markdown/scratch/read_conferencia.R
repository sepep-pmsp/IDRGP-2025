library(readxl)
df <- read_excel('dados/tabela_conferencia_completa.xlsx')
cat('Dimensions:', paste(dim(df), collapse='x'), '\n')
print(head(df))
