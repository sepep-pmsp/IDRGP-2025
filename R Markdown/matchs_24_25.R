library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# 1. Configurações de caminhos (rode com getwd() em ".../R Markdown")
caminho_dados <- "dados"
arquivo_ref_2024 <- file.path(caminho_dados, "Despesas_IDRGP_2024.xlsx")
arquivo_base_2025 <- file.path(caminho_dados, "base_2025_SEPLAN_IDRGP_2025.xlsx")

# Se existir a versão com nome 2026, usa ela
if (file.exists(file.path(caminho_dados, "base_2025_SEPLAN_2026.xlsx"))) {
  arquivo_base_2025 <- file.path(caminho_dados, "base_2025_SEPLAN_2026.xlsx")
}

output_dir <- "outputs_25_DA/tabelas_25_DA"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Leitura das bases
cat("Lendo base de referência 2024...\n")
df_ref_2024 <- read_excel(arquivo_ref_2024) |>
  clean_names() |>
  select(codigo_proj_ativ, descricao_proj_ativ)

cat("Lendo base de 2025...\n")
df_base_2025 <- read_excel(arquivo_base_2025) |>
  clean_names() |>
  # VALOR_DETALHAMENTO_AÇÃO -> valor_detalhamento_acao
  # TIPO_REGIONALIZAÇÃO     -> tipo_regionalizacao
  # SUBPREFEITURA           -> subprefeitura
  select(
    codigo_proj_ativ,
    descricao_proj_ativ,
    any_of(c(
      "valor_empenhado",
      "empenhado",
      "valor_detalhamento_acao",
      "tipo_regionalizacao",
      "subprefeitura"
    ))
  )

# Garante que as colunas existam, mesmo quando não vierem na base
if (!"valor_empenhado" %in% names(df_base_2025)) {
  if ("empenhado" %in% names(df_base_2025)) {
    df_base_2025 <- df_base_2025 |> rename(valor_empenhado = empenhado)
  } else {
    df_base_2025 <- df_base_2025 |> mutate(valor_empenhado = NA_real_)
  }
}

if (!"valor_detalhamento_acao" %in% names(df_base_2025)) {
  df_base_2025 <- df_base_2025 |> mutate(valor_detalhamento_acao = NA)
}

if (!"tipo_regionalizacao" %in% names(df_base_2025)) {
  df_base_2025 <- df_base_2025 |> mutate(tipo_regionalizacao = NA_character_)
}

if (!"subprefeitura" %in% names(df_base_2025)) {
  df_base_2025 <- df_base_2025 |> mutate(subprefeitura = NA_character_)
}

# 3. Padronização mínima (preservando originais)
df_ref_2024 <- df_ref_2024 |>
  mutate(
    codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
    descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish()
  )

df_base_2025 <- df_base_2025 |>
  mutate(
    codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
    descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish(),
    tipo_regionalizacao = as.character(tipo_regionalizacao) |> trimws() |> str_squish(),
    subprefeitura = as.character(subprefeitura) |> trimws() |> str_squish(),
    valor_empenhado = suppressWarnings(as.numeric(valor_empenhado))
  )

# 4. Join simples e rastreável
# Join por código (prioritário)
match_codigo <- df_ref_2024 |>
  left_join(
    df_base_2025 |>
      select(
        codigo_proj_ativ,
        desc_2025 = descricao_proj_ativ,
        valor_empenhado,
        valor_detalhamento_acao,
        tipo_regionalizacao,
        subprefeitura
      ),
    by = "codigo_proj_ativ"
  ) |>
  mutate(
    status_match = if_else(!is.na(desc_2025), "match_codigo", NA_character_)
  )

# O que sobrou sem match por código, tenta por descrição
sem_match_codigo <- match_codigo |>
  filter(is.na(status_match)) |>
  select(
    -desc_2025,
    -valor_empenhado,
    -valor_detalhamento_acao,
    -tipo_regionalizacao,
    -subprefeitura,
    -status_match
  )

# Join por descrição (secundário)
match_descricao <- sem_match_codigo |>
  left_join(
    df_base_2025 |>
      select(
        cod_2025 = codigo_proj_ativ,
        descricao_proj_ativ,
        valor_empenhado,
        valor_detalhamento_acao,
        tipo_regionalizacao,
        subprefeitura
      ),
    by = "descricao_proj_ativ"
  ) |>
  mutate(
    status_match = if_else(!is.na(cod_2025), "match_descricao", "sem_match")
  )

# 5. Consolidação da tabela de conferência
conferencia <- bind_rows(
  match_codigo |>
    filter(!is.na(status_match)) |>
    rename(
      codigo_proj_ativ_2024 = codigo_proj_ativ,
      descricao_proj_ativ_2024 = descricao_proj_ativ,
      descricao_proj_ativ_2025 = desc_2025
    ) |>
    mutate(codigo_proj_ativ_2025 = codigo_proj_ativ_2024),

  match_descricao |>
    rename(
      codigo_proj_ativ_2024 = codigo_proj_ativ,
      descricao_proj_ativ_2024 = descricao_proj_ativ,
      codigo_proj_ativ_2025 = cod_2025
    ) |>
    mutate(
      descricao_proj_ativ_2025 = if_else(
        status_match == "match_descricao",
        descricao_proj_ativ_2024,
        NA_character_
      )
    )
) |>
  select(
    codigo_proj_ativ_2024,
    descricao_proj_ativ_2024,
    codigo_proj_ativ_2025,
    descricao_proj_ativ_2025,
    valor_empenhado,
    valor_detalhamento_acao,
    tipo_regionalizacao,
    subprefeitura,
    status_match
  )

# 6. Cálculo de totais
resumo <- conferencia |>
  summarise(
    valor_total_empenhado_match = sum(valor_empenhado[status_match != "sem_match"], na.rm = TRUE),
    qtd_acoes_compatibilizadas = sum(status_match != "sem_match"),
    qtd_acoes_sem_correspondencia = sum(status_match == "sem_match")
  )

cat("\n--- Resumo da Compatibilização ---\n")
print(resumo)

# 7. Exportação (somente ações "Despesa Regionalizável")
normaliza_tipo_regionalizacao <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_squish()
}

conferencia_export <- conferencia |>
  filter(normaliza_tipo_regionalizacao(tipo_regionalizacao) == "despesa regionalizavel")

write_xlsx(conferencia_export, file.path(output_dir, "tabela_conferencia_completa.xlsx"))
write_xlsx(
  conferencia_export |> filter(status_match != "sem_match"),
  file.path(output_dir, "tabela_apenas_matches.xlsx")
)
write_xlsx(
  conferencia_export |> filter(status_match == "sem_match"),
  file.path(output_dir, "tabela_apenas_sem_matches.xlsx")
)

# Exporta as colunas pedidas da base 2025 (somente "Despesa Regionalizável")
base_2025_colunas_pedidas <- df_base_2025 |>
  filter(normaliza_tipo_regionalizacao(tipo_regionalizacao) == "despesa regionalizavel") |>
  select(
    codigo_proj_ativ,
    descricao_proj_ativ,
    valor_detalhamento_acao,
    tipo_regionalizacao,
    subprefeitura
  ) |>
  rename(
    VALOR_DETALHAMENTO_AÇÃO = valor_detalhamento_acao,
    TIPO_REGIONALIZAÇÃO = tipo_regionalizacao,
    SUBPREFEITURA = subprefeitura
  )

write_xlsx(
  base_2025_colunas_pedidas,
  file.path(output_dir, "base_2025_colunas_VALOR_DETALHAMENTO_ACAO_TIPO_REGIONALIZACAO.xlsx")
)

cat("\nArquivos exportados para: ", output_dir, "\n", sep = "")

View(conferencia_export)
