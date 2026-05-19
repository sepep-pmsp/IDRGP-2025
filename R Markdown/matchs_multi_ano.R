library(tidyverse)
library(readxl)
library(writexl)
library(janitor)
library(stringi)

# -------------------------------------------------------------------------
# 1. CONFIGURAÇÕES DE CAMINHOS
# -------------------------------------------------------------------------
# Detecção dinâmica dos caminhos de entrada e saída conforme o diretório de trabalho
if (dir.exists("dados")) {
  caminho_dados <- "dados"
  output_dir <- "outputs_25_DA/tabelas_25_DA"
} else if (dir.exists("R Markdown/dados")) {
  caminho_dados <- "R Markdown/dados"
  output_dir <- "R Markdown/outputs_25_DA/tabelas_25_DA"
} else {
  caminho_dados <- "dados"
  output_dir <- "outputs_25_DA/tabelas_25_DA"
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

arquivo_ref_2024 <- file.path(caminho_dados, "Despesas_IDRGP_2024.xlsx")

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

# Função para ler, limpar e padronizar as colunas de cada base anual (mantendo todas as linhas de detalhe)
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
# 3. LEITURA DA REFERÊNCIA FIXA (DESPESAS_IDRGP_2024.XLSX)
# -------------------------------------------------------------------------

cat(sprintf("Lendo base de referência fixa (2024) em %s...\n", arquivo_ref_2024))
if (!file.exists(arquivo_ref_2024)) {
  stop(sprintf("Erro: O arquivo de referência fixa não foi encontrado em: %s", arquivo_ref_2024))
}

df_ref_2024 <- read_excel(arquivo_ref_2024) |>
  clean_names() |>
  select(codigo_proj_ativ, descricao_proj_ativ) |>
  mutate(
    codigo_proj_ativ = as.character(codigo_proj_ativ) |> trimws() |> str_squish(),
    descricao_proj_ativ = as.character(descricao_proj_ativ) |> trimws() |> str_squish()
  ) |>
  distinct()

# -------------------------------------------------------------------------
# 4. LEITURA E PREPARAÇÃO DAS BASES (2022 A 2025)
# -------------------------------------------------------------------------

anos <- c(2022, 2023, 2024, 2025)
caminhos_arquivos <- map_chr(anos, obter_caminho_base)

# Leitura e padronização de cada arquivo anual
bases_anuais <- map2(caminhos_arquivos, anos, ler_e_padronizar) |> 
  set_names(paste0("df_", anos))

# -------------------------------------------------------------------------
# 5. LÓGICA DO MATCH CONTRA A REFERÊNCIA FIXA (DE 2024)
# -------------------------------------------------------------------------

compatibilizar_com_referencia <- function(df_ano, df_ref) {
  # Join 1: Tenta correspondência por codigo_proj_ativ (referência na esquerda, base anual na direita)
  match_codigo <- df_ref |> 
    left_join(
      df_ano |> 
        select(
          codigo_proj_ativ,
          desc_ano = descricao_proj_ativ,
          valor_empenhado,
          valor_detalhamento_acao,
          tipo_regionalizacao,
          subprefeitura,
          ano
        ),
      by = "codigo_proj_ativ"
    ) |> 
    mutate(
      status_match = if_else(!is.na(desc_ano), "match_codigo", NA_character_)
    )
  
  # Isola o que não deu match por código para tentar por descrição
  sem_match_codigo <- match_codigo |> 
    filter(is.na(status_match)) |> 
    select(
      -desc_ano,
      -valor_empenhado,
      -valor_detalhamento_acao,
      -tipo_regionalizacao,
      -subprefeitura,
      -ano,
      -status_match
    )
  
  # Join 2: Tenta correspondência por descricao_proj_ativ
  match_descricao <- sem_match_codigo |> 
    left_join(
      df_ano |> 
        select(
          cod_ano = codigo_proj_ativ,
          descricao_proj_ativ,
          valor_empenhado,
          valor_detalhamento_acao,
          tipo_regionalizacao,
          subprefeitura,
          ano
        ),
      by = "descricao_proj_ativ"
    ) |> 
    mutate(
      status_match = if_else(!is.na(cod_ano), "match_descricao", "sem_match")
    )
  
  # Consolidação dos dois fluxos de join
  df_conferido <- bind_rows(
    # Registros que bateram por código
    match_codigo |> 
      filter(!is.na(status_match)) |> 
      rename(
        codigo_proj_ativ_2024 = codigo_proj_ativ,
        descricao_proj_ativ_2024 = descricao_proj_ativ,
        descricao_proj_ativ_ano = desc_ano
      ) |> 
      mutate(codigo_proj_ativ_ano = codigo_proj_ativ_2024),
    
    # Registros que foram para o match por descrição (ou ficaram sem_match)
    match_descricao |> 
      rename(
        codigo_proj_ativ_2024 = codigo_proj_ativ,
        descricao_proj_ativ_2024 = descricao_proj_ativ,
        codigo_proj_ativ_ano = cod_ano
      ) |> 
      mutate(
        descricao_proj_ativ_ano = if_else(
          status_match == "match_descricao",
          descricao_proj_ativ_2024,
          NA_character_
        )
      )
  )
  
  # Preenche o ano de origem caso venha como NA nos registros sem_match
  ano_corrente <- unique(df_ano$ano)
  df_conferido <- df_conferido |> 
    mutate(ano = if_else(is.na(ano), as.integer(ano_corrente), as.integer(ano)))
  
  return(df_conferido)
}

# Executa a compatibilização para cada um dos quatro anos
bases_compatibilizadas <- map(bases_anuais, compatibilizar_com_referencia, df_ref = df_ref_2024)

# Consolida todas as bases em uma única tabela
tabela_consolidada_raw <- bind_rows(bases_compatibilizadas)

# -------------------------------------------------------------------------
# 6. FILTRAGEM E ORGANIZAÇÃO FINAL
# -------------------------------------------------------------------------

# Mantém apenas registros "Despesa Regionalizável" ou sem_match (que são ações da ref não encontradas)
tabela_consolidada <- tabela_consolidada_raw |> 
  filter(
    status_match == "sem_match" | 
    normalizar_texto(tipo_regionalizacao) == "despesa regionalizavel"
  ) |> 
  select(
    ano,
    codigo_proj_ativ_2024,
    descricao_proj_ativ_2024,
    codigo_proj_ativ_ano,
    descricao_proj_ativ_ano,
    valor_empenhado,
    valor_detalhamento_acao,
    tipo_regionalizacao,
    subprefeitura,
    status_match
  ) |> 
  arrange(ano, codigo_proj_ativ_2024, subprefeitura)

# -------------------------------------------------------------------------
# 7. RESUMO ESTATÍSTICO NO CONSOLE
# -------------------------------------------------------------------------

resumo_estatistico <- tabela_consolidada |> 
  group_by(ano) |> 
  summarise(
    total_registros = n(),
    acoes_unicas_da_ref_encontradas = n_distinct(codigo_proj_ativ_ano[status_match != "sem_match"]),
    valor_total_empenhado = sum(valor_empenhado, na.rm = TRUE),
    valor_total_detalhamento = sum(valor_detalhamento_acao, na.rm = TRUE),
    qtd_match_codigo = sum(status_match == "match_codigo"),
    qtd_match_descricao = sum(status_match == "match_descricao"),
    qtd_sem_match = sum(status_match == "sem_match"),
    .groups = "drop"
  )

cat("\n--- Resumo Estatístico por Ano (Filtrado por Despesa Regionalizável / Sem Match) ---\n")
print(resumo_estatistico)

# -------------------------------------------------------------------------
# 8. EXPORTAÇÃO EXCEL COM TRÊS ABAS
# -------------------------------------------------------------------------

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


# Possíveis razões para "sem_match":
# Mudança de código: O projeto mudou de código de um ano para outro (ex: era 1001 em 2024, mas em 2022 era 2001)
# Projeto novo: O projeto foi criado depois daquele ano (ex: projeto iniciou em 2024, não existia em 2022)
# Projeto encerrado: Existia na referência de 2024 mas já foi concluído
# Erro de digitação/nomenclatura: Código ou descrição com pequenas variações
# Reestruturação orçamentária: O projeto foi desmembrado ou agrupado

cat("Exportação concluída com sucesso!\n")
