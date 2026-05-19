library(readxl)
library(janitor)
library(dplyr)

caminho_2025 <- "dados/base_2025_SEPLAN_IDRGP_2025.xlsx"
df <- read_excel(caminho_2025) |> clean_names()

cat("Raw rows in 2025:", nrow(df), "\n")

# Check row counts when grouped by action, subprefecture, and regionalization type
df_grouped <- df |>
  group_by(codigo_proj_ativ, descricao_proj_ativ, tipo_regionalizacao, subprefeitura) |>
  summarise(n = n(), .groups = "drop")

cat("Grouped rows in 2025:", nrow(df_grouped), "\n")
