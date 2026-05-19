library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(stringi)

# -------------------------------------------------------------------------
# 1. CONFIGURAÇÕES DE CAMINHOS
# -------------------------------------------------------------------------
caminho_dados <- "dados"
output_dir <- "outputs_25_DA/tabelas_25_DA"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# -------------------------------------------------------------------------
# 2. FUNÇÕES AUXILIARES DE TRATAMENTO E COMPATIBILIZAÇÃO
# -------------------------------------------------------------------------

# Função para normalizar texto de forma robusta (remover acentos, minúsculas, trim)
normalizar_texto <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  # Transliteração de acentos para ASCII
  x_trans <- tryCatch({
    stringi::stri_trans_general(x, "Latin-ASCII")
  }, error = function(e) {
    # Fallback caso stringi falhe
    iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  })
  x_trans |>
    str_to_lower() |>
    str_squish()
}

# Função para buscar dinamicamente o arquivo de um ano com possíveis fallbacks de nomes
obter_caminho_base <- function(ano) {
  padroes <- c(
    sprintf("base_%d_SEPLAN_IDRGP_2025.xlsx", ano),
    sprintf("base_%d_SEPLAN_2026.xlsx", ano),
    sprintf("base_%d_SEPLAN.xlsx", ano)
  )
  for (padrao in padroes) {
    caminho <- file.path(caminho_dados, padrao)
    if (file.exists(caminho)) {
      return(caminho)
    }
  }
  # Retorna o padrão default se nenhum for encontrado
  return(file.path(caminho_dados, sprintf("base_%d_SEPLAN_IDRGP_2025.xlsx", ano)))
}

# Função para ler, limpar e padronizar as colunas de cada base anual de forma defensiva
ler_e_padronizar <- function(caminho_arquivo, ano) {
  cat(sprintf("Lendo base do ano %d (%s)...\n", ano, basename(caminho_arquivo)))
  
  if (!file.exists(caminho_arquivo)) {
    stop(sprintf("Erro: O arquivo para o ano %d não foi encontrado no caminho: %s", ano, caminho_arquivo))
  }
  
  # Leitura inicial e limpeza de nomes com janitor
  df <- read_excel(caminho_arquivo) |> 
    clean_names()
  
  # 1. Tratamento da coluna valor_empenhado (aceita também "empenhado")
  if (!"valor_empenhado" %in% names(df)) {
    if ("empenhado" %in% names(df)) {
      df <- df |> rename(valor_empenhado = empenhado)
    } else {
      df <- df |> mutate(valor_empenhado = NA_real_)
    }
  }
  
  # 2. Tratamento da coluna valor_detalhamento_acao (com possíveis variações de acento/grafia)
  col_val_det <- intersect(names(df), c("valor_detalhamento_acao", "valor_detalhamento_a_cao", "detalhamento_acao"))[1]
  if (!is.na(col_val_det)) {
    df <- df |> rename(valor_detalhamento_acao = !!sym(col_val_det))
  } else if (!"valor_detalhamento_acao" %in% names(df)) {
    df <- df |> mutate(valor_detalhamento_acao = NA_real_)
  }
  
  # 3. Tratamento da coluna tipo_regionalizacao
  col_tipo_reg <- intersect(names(df), c("tipo_regionalizacao", "tipo_regionalizac_ao"))[1]
  if (!is.na(col_tipo_reg)) {
    df <- df |> rename(tipo_regionalizacao = !!sym(col_tipo_reg))
  } else if (!"tipo_regionalizacao" %in% names(df)) {
    df <- df |> mutate(tipo_regionalizacao = NA_character_)
  }
  
  # 4. Tratamento da coluna subprefeitura
  if (!"subprefeitura" %in% names(df)) {
    df <- df |> mutate(subprefeitura = NA_character_)
  }
  
  # 5. Garantia de colunas obrigatórias codigo_proj_ativ e descricao_proj_ativ
  if (!"codigo_proj_ativ" %in% names(df)) {
    stop(sprintf("Erro: A coluna 'codigo_proj_ativ' não foi encontrada na base do ano %d.", ano))
  }
  if (!"descricao_proj_ativ" %in% names(df)) {
    stop(sprintf("Erro: A coluna 'descricao_proj_ativ' não foi encontrada na base do ano %d.", ano))
  }
  
  # Padronização final de tipos e remoção de espaços nas bordas
  df_padronizado <- df |> 
    mutate(
      codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
      descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish(),
      tipo_regionalizacao = as.character(tipo_regionalizacao) |> trimws() |> str_squish(),
      subprefeitura = as.character(subprefeitura) |> trimws() |> str_squish(),
      valor_empenhado = suppressWarnings(as.numeric(valor_empenhado)),
      valor_detalhamento_acao = suppressWarnings(as.numeric(valor_detalhamento_acao)),
      ano = as.integer(ano)
    ) |> 
    select(
      ano,
      codigo_proj_ativ,
      descricao_proj_ativ,
      valor_empenhado,
      valor_detalhamento_acao,
      tipo_regionalizacao,
      subprefeitura
    )
  
  return(df_padronizado)
}

# -------------------------------------------------------------------------
# 3. LEITURA E PREPARAÇÃO DAS BASES (2022 A 2025)
# -------------------------------------------------------------------------

anos <- c(2022, 2023, 2024, 2025)
caminhos_arquivos <- map_chr(anos, obter_caminho_base)

# Leitura e padronização de cada arquivo
bases_anuais <- map2(caminhos_arquivos, anos, ler_e_padronizar) |> 
  set_names(paste0("df_", anos))

# Extrai a base de referência (2025)
df_2025 <- bases_anuais$df_2025

# Cria o catálogo de ações únicas de 2025 (referência para o match)
df_ref_2025 <- df_2025 |> 
  select(codigo_proj_ativ, descricao_proj_ativ) |> 
  distinct() |> 
  filter(!is.na(codigo_proj_ativ) & codigo_proj_ativ != "")

# -------------------------------------------------------------------------
# 4. LÓGICA DE COMPATIBILIZAÇÃO CONTRA A BASE DE REFERÊNCIA (2025)
# -------------------------------------------------------------------------

compatibilizar_com_2025 <- function(df_ano, df_ref) {
  # Caso o ano seja o próprio ano de referência (2025), o match é trivial
  if (unique(df_ano$ano) == 2025) {
    return(
      df_ano |> 
        mutate(
          status_match = "match_codigo",
          codigo_proj_ativ_2025 = codigo_proj_ativ,
          descricao_proj_ativ_2025 = descricao_proj_ativ
        )
    )
  }
  
  # Join 1: Tenta correspondência por código_proj_ativ
  match_codigo <- df_ano |> 
    left_join(
      df_ref |> select(codigo_proj_ativ, desc_2025 = descricao_proj_ativ),
      by = "codigo_proj_ativ"
    ) |> 
    mutate(
      status_match = if_else(!is.na(desc_2025), "match_codigo", NA_character_)
    )
  
  # Isola o que não deu match por código para a segunda rodada
  sem_match_codigo <- match_codigo |> 
    filter(is.na(status_match)) |> 
    select(-desc_2025, -status_match)
  
  # Join 2: Tenta correspondência por descricao_proj_ativ
  match_descricao <- sem_match_codigo |> 
    left_join(
      df_ref |> select(cod_2025 = codigo_proj_ativ, descricao_proj_ativ),
      by = "descricao_proj_ativ"
    ) |> 
    mutate(
      status_match = if_else(!is.na(cod_2025), "match_descricao", "sem_match")
    )
  
  # Consolidação dos dois fluxos de join
  df_conferido <- bind_rows(
    # Registros que bateram por código
    match_codigo |> 
      filter(!is.na(status_match)) |> 
      mutate(
        codigo_proj_ativ_2025 = codigo_proj_ativ,
        descricao_proj_ativ_2025 = desc_2025
      ) |> 
      select(-desc_2025),
    
    # Registros que foram para o match por descrição (ou ficaram sem correspondência)
    match_descricao |> 
      mutate(
        codigo_proj_ativ_2025 = if_else(status_match == "match_descricao", cod_2025, NA_character_),
        descricao_proj_ativ_2025 = if_else(status_match == "match_descricao", descricao_proj_ativ, NA_character_)
      ) |> 
      select(-cod_2025)
  )
  
  return(df_conferido)
}

# Aplica a compatibilização para cada ano
bases_compatibilizadas <- map(bases_anuais, compatibilizar_com_2025, df_ref = df_ref_2025)

# Consolida todas as bases em uma única tabela
tabela_consolidada_raw <- bind_rows(bases_compatibilizadas)

# -------------------------------------------------------------------------
# 5. FILTRAGEM E ORGANIZAÇÃO FINAL
# -------------------------------------------------------------------------

# Mantém apenas as ações que são "Despesa Regionalizável" (com comparação robusta sem acentos)
tabela_consolidada <- tabela_consolidada_raw |> 
  filter(normalizar_texto(tipo_regionalizacao) == "despesa regionalizavel") |> 
  select(
    ano,
    codigo_proj_ativ,
    descricao_proj_ativ,
    codigo_proj_ativ_2025,
    descricao_proj_ativ_2025,
    valor_empenhado,
    valor_detalhamento_acao,
    tipo_regionalizacao,
    subprefeitura,
    status_match
  ) |> 
  arrange(ano, codigo_proj_ativ, subprefeitura)

# -------------------------------------------------------------------------
# 6. RESUMO ESTATÍSTICO DOS TOTALIZADORES
# -------------------------------------------------------------------------

resumo_estatistico <- tabela_consolidada |> 
  group_by(ano) |> 
  summarise(
    total_registros = n(),
    acoes_unicas = n_distinct(codigo_proj_ativ),
    valor_total_empenhado = sum(valor_empenhado, na.rm = TRUE),
    valor_total_detalhamento = sum(valor_detalhamento_acao, na.rm = TRUE),
    qtd_match_codigo = sum(status_match == "match_codigo"),
    qtd_match_descricao = sum(status_match == "match_descricao"),
    qtd_sem_match = sum(status_match == "sem_match"),
    .groups = "drop"
  )

cat("\n--- Resumo Estatístico por Ano (Apenas Despesa Regionalizável) ---\n")
print(resumo_estatistico)

# -------------------------------------------------------------------------
# 7. EXPORTAÇÃO EXCEL COM TRÊS ABAS
# -------------------------------------------------------------------------

# Separação dos conjuntos de dados
tabela_completa <- tabela_consolidada
tabela_matches  <- tabela_consolidada |> filter(status_match %in% c("match_codigo", "match_descricao"))
tabela_sem_match <- tabela_consolidada |> filter(status_match == "sem_match")

caminho_exportacao <- file.path(output_dir, "tabela_conferencia_multi_ano.xlsx")

cat(sprintf("\nExportando dados para %s...\n", caminho_exportacao))

write_xlsx(
  list(
    "Tabela Completa"     = tabela_completa,
    "Apenas Matches"      = tabela_matches,
    "Apenas Sem Matches"  = tabela_sem_match
  ),
  path = caminho_exportacao
)

cat("Exportação concluída com sucesso!\n")
