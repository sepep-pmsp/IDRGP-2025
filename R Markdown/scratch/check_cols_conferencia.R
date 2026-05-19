library(readxl)
library(janitor)

file_path <- "C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP-2025/R Markdown/dados/tabela_conferencia_multi_ano.xlsx"
if (file.exists(file_path)) {
  df <- read_excel(file_path, n_max = 1) |> clean_names()
  cat("Cols:", paste(names(df), collapse=", "), "\n\n")
} else {
  cat("Arquivo não encontrado\n")
}
