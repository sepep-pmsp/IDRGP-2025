library(dplyr)
library(stringr)

norm_name <- function(x) {
  x |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all("[áàâãä]", "a") |>
    str_replace_all("[éèêë]",  "e") |>
    str_replace_all("[íìîï]",  "i") |>
    str_replace_all("[óòôõö]", "o") |>
    str_replace_all("[úùûü]",  "u") |>
    str_replace_all("ç",       "c") |>
    str_replace_all("^subprefeitura de ", "") |>
    str_replace_all("^subprefeitura ",    "") |>
    str_replace_all("[^a-z0-9]",          "")
}

padronizar_subprefeitura <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "^Subprefeitura de ", "")
  x <- stringr::str_replace_all(x, "^Subprefeitura ", "")
  x <- dplyr::case_when(
    x %in% c("Supra Centro", "Supra Subprefeitura Centro") ~ "Sé",
    TRUE ~ x
  )
}

pasta_dados <- "dados"
arquivo_base_central <- file.path(pasta_dados, "tabela_conferencia_completa.xlsx")
df <- readxl::read_excel(arquivo_base_central)

df_2025 <- df |>
  mutate(
    subprefeitura = padronizar_subprefeitura(subprefeitura),
    valor_liquidado = as.numeric(valor_detalhamento_acao)
  )

df_agr <- df_2025 |>
  filter(!is.na(subprefeitura) & subprefeitura != "") |>
  group_by(subprefeitura) |>
  summarise(valor = sum(valor_liquidado, na.rm = TRUE), .groups = "drop") |>
  mutate(join_key = norm_name(subprefeitura))

print(df_agr)
