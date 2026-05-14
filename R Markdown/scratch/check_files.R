library(readxl)
library(dplyr)
library(stringr)

pasta_dados <- "dados"
arquivo_2025 <- file.path(pasta_dados, "base_2025_SEPLAN_IDRGP_2025.xlsx")
arquivo_ref <- file.path(pasta_dados, "Despesas_IDRGP_2024.xlsx")

cat("Checking arquivo_2025 sheets:\n")
if (file.exists(arquivo_2025)) {
  print(excel_sheets(arquivo_2025))
} else {
  cat("arquivo_2025 not found\n")
}

cat("\nChecking arquivo_ref sheets:\n")
if (file.exists(arquivo_ref)) {
  print(excel_sheets(arquivo_ref))
} else {
  cat("arquivo_ref not found\n")
}

cat("\nReading 2025 data (first 5 rows):\n")
if (file.exists(arquivo_2025)) {
  df <- read_excel(arquivo_2025, n_max = 5)
  print(names(df))
  print(head(df))
}

cat("\nReading Ref data (first 5 rows):\n")
if (file.exists(arquivo_ref)) {
  df_ref <- read_excel(arquivo_ref, n_max = 5)
  print(names(df_ref))
  print(head(df_ref))
}
