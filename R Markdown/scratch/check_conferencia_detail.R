library(readxl)
library(dplyr)

df <- read_excel("dados/tabela_conferencia_completa.xlsx")

cat("Row count in conference table:", nrow(df), "\n")

# Check if there are duplicates for action and subprefecture
dup_counts <- df |>
  group_by(codigo_proj_ativ_2025, subprefeitura) |>
  summarise(n = n(), .groups = "drop") |>
  count(n)

cat("\nCounts of duplicates per action + subprefecture:\n")
print(dup_counts)
