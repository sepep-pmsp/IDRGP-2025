library(readxl)
library(janitor)
library(dplyr)

caminho_2025 <- "dados/base_2025_SEPLAN_IDRGP_2025.xlsx"
df <- read_excel(caminho_2025) |> clean_names()
cat("Raw dimensions of 2025:", paste(dim(df), collapse="x"), "\n")

# Check unique values in tipo_regionalizacao
cat("\nUnique values in tipo_regionalizacao:\n")
print(df |> count(tipo_regionalizacao))

# Check counts after filtering Despesa Regionalizável
df_filt <- df |> filter(stringi::stri_trans_general(tipo_regionalizacao, "Latin-ASCII") |> stringr::str_to_lower() |> stringr::str_squish() == "despesa regionalizavel")
cat("\nFiltered dimensions (Despesa Regionalizável only):", paste(dim(df_filt), collapse="x"), "\n")
