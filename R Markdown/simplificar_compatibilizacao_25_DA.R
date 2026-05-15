
library(tidyverse)
library(readxl)
library(writexl)
library(janitor)

# 1. Configurações de Caminhos
# O usuário mencionou base_2025_SEPLAN_2026.xlsx, mas no diretório existe base_2025_SEPLAN_IDRGP_2025.xlsx
# Vou tentar o nome fornecido e fallback para o existente se necessário.
caminho_dados <- "dados"
arquivo_ref_2024 <- file.path(caminho_dados, "Despesas_IDRGP_2024.xlsx")
arquivo_base_2025 <- file.path(caminho_dados, "base_2025_SEPLAN_IDRGP_2025.xlsx") 

# Se o usuário insistir no nome 2026, tentamos ele primeiro
if (file.exists(file.path(caminho_dados, "base_2025_SEPLAN_2026.xlsx"))) {
  arquivo_base_2025 <- file.path(caminho_dados, "base_2025_SEPLAN_2026.xlsx")
}

output_dir <- "outputs_25_DA/tabelas_25_DA"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Leitura das Bases
cat("Lendo base de referência 2024...\n")
df_ref_2024 <- read_excel(arquivo_ref_2024) |> 
  clean_names() |> 
  select(codigo_proj_ativ, descricao_proj_ativ)

cat("Lendo base de 2025...\n")
df_base_2025 <- read_excel(arquivo_base_2025) |> 
  clean_names() |> 
  # O usuário quer valor_empenhado. Se a coluna for diferente, tentamos mapear.
  # Na base original SEPLAN costuma ser 'valor_empenhado' ou 'empenhado'.
  select(codigo_proj_ativ, descricao_proj_ativ, any_of(c("valor_empenhado", "empenhado")))

# Se 'valor_empenhado' não existir, renomeia a que existir ou avisa
if ("empenhado" %in% names(df_base_2025) && !"valor_empenhado" %in% names(df_base_2025)) {
  df_base_2025 <- df_base_2025 |> rename(valor_empenhado = empenhado)
}

# 3. Padronização Mínima (Preservando originais)
df_ref_2024 <- df_ref_2024 |> 
  mutate(
    codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
    descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish()
  )

df_base_2025 <- df_base_2025 |> 
  mutate(
    codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
    descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish(),
    valor_empenhado = as.numeric(valor_empenhado)
  )

# 4. Join Simples e Rastreável
# Join por Código (Prioritário)
match_codigo <- df_ref_2024 |> 
  left_join(
    df_base_2025 |> select(codigo_proj_ativ, desc_2025 = descricao_proj_ativ, valor_empenhado),
    by = "codigo_proj_ativ"
  ) |> 
  mutate(
    status_match = if_else(!is.na(desc_2025), "match_codigo", NA_character_)
  )

# Identificar o que sobrou sem match de código para tentar por descrição
sem_match_codigo <- match_codigo |> filter(is.na(status_match)) |> select(-desc_2025, -valor_empenhado, -status_match)

# Join por Descrição (Secundário)
match_descricao <- sem_match_codigo |> 
  left_join(
    df_base_2025 |> select(cod_2025 = codigo_proj_ativ, descricao_proj_ativ, valor_empenhado),
    by = "descricao_proj_ativ"
  ) |> 
  mutate(
    status_match = if_else(!is.na(cod_2025), "match_descricao", "sem_match")
  )

# 5. Consolidação da Tabela de Conferência
# Ajustando nomes para o formato solicitado
conferencia <- bind_rows(
  match_codigo |> filter(!is.na(status_match)) |> 
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
    mutate(descricao_proj_ativ_2025 = if_else(status_match == "match_descricao", descricao_proj_ativ_2024, NA_character_))
) |> 
  select(
    codigo_proj_ativ_2024,
    descricao_proj_ativ_2024,
    codigo_proj_ativ_2025,
    descricao_proj_ativ_2025,
    valor_empenhado,
    status_match
  )

# 6. Cálculo de Totais
resumo <- conferencia |> 
  summarise(
    valor_total_empenhado_match = sum(valor_empenhado[status_match != "sem_match"], na.rm = TRUE),
    qtd_acoes_compatibilizadas = sum(status_match != "sem_match"),
    qtd_acoes_sem_correspondencia = sum(status_match == "sem_match")
  )

cat("\n--- Resumo da Compatibilização ---\n")
print(resumo)

# 7. Exportação
write_xlsx(conferencia, file.path(output_dir, "tabela_conferencia_completa.xlsx"))
write_xlsx(conferencia |> filter(status_match != "sem_match"), file.path(output_dir, "tabela_apenas_matches.xlsx"))
write_xlsx(conferencia |> filter(status_match == "sem_match"), file.path(output_dir, "tabela_apenas_sem_matches.xlsx"))

cat("\nArquivos exportados para:", output_dir, "\n")
