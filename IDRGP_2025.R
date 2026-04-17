# ==============================================================================
# IDRGP 2025 — VERSÃO AUTOMATIZADA — V8
# ==============================================================================
# Índice de Distribuição Regional do Gasto Público
# Secretaria Municipal de Planejamento Urbano — SEPLAN/CPMA
# Prefeitura de São Paulo
# Atualizado em: 2025
# ==============================================================================

# 01. APRESENTAÇÃO ============================================================

## 1.1 Introdução --------------------------------------------------------------

# Este script tem o objetivo de atualizar o Índice de Distribuição Regional
# do Gasto Público (IDRGP) de 2025 e consolidar o balanço quadrienal 2022–2025.
#
# O IDRGP mede se a distribuição territorial dos gastos públicos municipais
# está alinhada às prioridades sociais e de infraestrutura de cada subprefeitura,
# expressas no Índice de Vulnerabilidade Urbana (IVU/SEPLAN/CPMA).
#
# O script considera, essencialmente:
#   - a região (subprefeitura)
#   - o valor do gasto público por projeto/atividade
#
# Fontes de dados:
#   1. Dados regionalizados de despesas — Secretaria Municipal da Fazenda
#   2. IDRGP Alvo — SGM/SEPLAN/CPMA (baseado no IVU 2025)
#   3. Bases históricas processadas (2022–2024) — df_integrado_2022/2023/2024.xlsx
#
# Organização do script (fases CRISP-DM em cada setor):
#   01. APRESENTAÇÃO   — bibliotecas, funções auxiliares e parâmetros globais
#   02. ORÇAMENTO 2025 — importação e processamento da base de despesas 2025
#   03. IDRGP ALVO     — importação do índice de referência
#   04. MAPA           — dados cartográficos das 32 subprefeituras
#   05. INTEGRAÇÃO     — consolidação dos dados em df_integrado (2025)
#   06. HISTÓRICO      — importação e consolidação dos dados 2022–2024
#   07. TESTES         — testes de normalidade e classificação estatística
#   08. MAPAS          — 5 mapas temáticos (2022, 2023, 2024, 2025, agregado)
#   09. GRÁFICOS       — 4 gráficos (IDRGP %, valores R$, série histórica)
#   10. ANEXOS         — tabela detalhada por projeto/atividade
#   11. EXPORTAÇÃO     — todos os produtos em PNG, XLSX e SHP/GPKG
#   12. CONCLUSÃO      — verificações finais e mensagem de encerramento

## 1.2 Bibliotecas ------------------------------------------------------------

library(tidyverse)   # manipulação e visualização de dados
library(readxl)      # leitura de arquivos Excel
library(janitor)     # padronização de nomes de colunas
library(scales)      # formatação de eixos em gráficos
library(writexl)     # exportação para Excel
library(sf)          # dados geoespaciais e mapas
library(ggplot2)     # visualização de dados

## 1.3 Pasta de trabalho ------------------------------------------------------

setwd("C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP 2025")

## 1.4 Função auxiliar de normalização de nomes (join_key) -------------------

# Utilizada para criar uma chave de integração robusta entre as bases de dados.
# Remove acentos, converte para minúsculas e elimina caracteres não alfabéticos.

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

## 1.5 Parâmetros globais -----------------------------------------------------

# Ano de referência do relatório
ano_indice <- 2025

# Intervalo de análise do ciclo PPA
anos_analise <- 2022:2025

# Valor total previsto para o ciclo quadrienal (R$ 5 bilhões)
valor_total_ciclo <- 5000000000

# Número de anos do ciclo
n_anos_ciclo <- 4

# Valor previsto médio por ano
valor_previsto_ano <- valor_total_ciclo / n_anos_ciclo

# Valor previsto para o ciclo completo (R$ 3,75 bi para 2022–2024; R$ 5 bi para 2022–2025)
valor_previsto_ciclo_2022_2024 <- valor_total_ciclo * (3 / 4)
valor_previsto_ciclo_completo  <- valor_total_ciclo

## 1.6 Controle de filtros opcionais ------------------------------------------

# Filtros de despesa (TRUE = aplicar)
aplicar_filtro_projetos      <- FALSE  # restringe a projetos/atividades específicos
aplicar_filtro_funcoes       <- FALSE  # restringe a funções orçamentárias selecionadas
aplicar_filtro_investimentos <- FALSE  # restringe ao grupo 4 (Investimentos)

# Seleção do índice de referência (TRUE em apenas 1 opção)
aplicar_indice_2022 <- FALSE
aplicar_indice_2024 <- FALSE
aplicar_indice_2025 <- TRUE

## 1.7 Verificações de consistência -------------------------------------------

# Verifica se exatamente 1 índice foi selecionado
n_indices <- sum(aplicar_indice_2022, aplicar_indice_2024, aplicar_indice_2025)
if (n_indices != 1) {
  stop("ERRO: selecione exatamente 1 índice de referência nas configurações iniciais.")
} else {
  cat("OK: índice de referência selecionado corretamente.\n")
}

# Verifica se o ano de referência está dentro do ciclo de análise
if (!ano_indice %in% anos_analise) {
  stop("ERRO: ano_indice não está dentro do intervalo de anos_analise.")
} else {
  cat("OK: ano de referência (", ano_indice, ") está dentro do ciclo", min(anos_analise), "–", max(anos_analise), ".\n")
}


# 02. ORÇAMENTO 2025 ===========================================================

## 2.1 Introdução --------------------------------------------------------------

# Nesta seção serão importados e processados os dados regionalizados de
# despesas de 2025, provenientes da base validada da Secretaria da Fazenda.
# A base é o fundamento do cálculo do IDRGP Real 2025.

## 2.2 Coleta ------------------------------------------------------------------

# Verifica as abas disponíveis no arquivo
excel_sheets("basedadosDA_1225_validado.xlsx")

# Importa a base de despesas
original_orcamento <- read_excel(
  "basedadosDA_1225_validado.xlsx",
  sheet = "dbo_ajl_execucao_det_acao_2025"
)

# Cria réplica de trabalho com ano de referência
df_orcamento <- original_orcamento |>
  clean_names() |>
  mutate(ano_referencia = ano_indice)

## 2.3 Exploração --------------------------------------------------------------

# Estrutura geral da base
glimpse(df_orcamento)

# Anos presentes na base
df_orcamento |> count(ano_empenho,    sort = TRUE)
df_orcamento |> count(ano_liquidacao, sort = TRUE)

# Principais campos (com drop_na para evitar erro em NA)
df_orcamento |> select(descricao_funcao)         |> distinct() |> drop_na() |> arrange(descricao_funcao)         |> print(n = 100)
df_orcamento |> select(descricao_subfuncao)      |> distinct() |> drop_na() |> arrange(descricao_subfuncao)      |> print(n = 100)
df_orcamento |> select(descricao_proj_ativ)      |> distinct() |> drop_na() |> arrange(descricao_proj_ativ)      |> print(n = 100)
df_orcamento |> select(descricao_conta_despesa)  |> distinct() |> drop_na() |> arrange(descricao_conta_despesa)  |> print(n = 100)
df_orcamento |> select(tipo_regionalizacao)      |> distinct() |> drop_na() |> arrange(tipo_regionalizacao)      |> print(n = 100)
df_orcamento |> select(subprefeitura)            |> distinct() |> drop_na() |> arrange(subprefeitura)            |> print(n = 100)
df_orcamento |> select(regiao)                   |> distinct() |> drop_na() |> arrange(regiao)                   |> print(n = 100)

## 2.4 Verificação da qualidade ------------------------------------------------

# Tabela de NAs nos campos principais
tabela_na_campos <- tibble(
  variavel = c(
    "valor_detalhamento_acao", "subprefeitura", "regiao",
    "tipo_regionalizacao", "descricao_funcao", "descricao_subfuncao",
    "descricao_programa", "descricao_proj_ativ",
    "descricao_conta_despesa", "descricao_fonte"
  ),
  n_na = c(
    sum(is.na(df_orcamento$valor_detalhamento_acao)),
    sum(is.na(df_orcamento$subprefeitura)),
    sum(is.na(df_orcamento$regiao)),
    sum(is.na(df_orcamento$tipo_regionalizacao)),
    sum(is.na(df_orcamento$descricao_funcao)),
    sum(is.na(df_orcamento$descricao_subfuncao)),
    sum(is.na(df_orcamento$descricao_programa)),
    sum(is.na(df_orcamento$descricao_proj_ativ)),
    sum(is.na(df_orcamento$descricao_conta_despesa)),
    sum(is.na(df_orcamento$descricao_fonte))
  )
) |> arrange(desc(n_na))

print(tabela_na_campos)

# Valor total bruto da base
cat("Valor total bruto (com NA):", sum(df_orcamento$valor_detalhamento_acao, na.rm = FALSE), "\n")
cat("Valor total útil (sem NA):", sum(df_orcamento$valor_detalhamento_acao, na.rm = TRUE), "\n")

# Percentual de linhas com subprefeitura especificada
perc_regionalizado <- (
  df_orcamento |>
    filter(str_detect(subprefeitura, "Subprefeitura"), na.rm = TRUE) |>
    filter(!str_detect(subprefeitura, "Supra"), na.rm = TRUE) |>
    nrow()
) / nrow(df_orcamento)

cat("Percentual regionalizado por subprefeitura:", scales::percent(perc_regionalizado, accuracy = 0.1), "\n")

## 2.5 Saneamento --------------------------------------------------------------

# Cria coluna de orçamento com NA substituído por 0
df_orcamento <- df_orcamento |>
  mutate(orcamento = replace_na(valor_detalhamento_acao, 0))

# Padroniza nomes das subprefeituras
df_orcamento <- df_orcamento |>
  mutate(
    subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
    subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    "")
  )

# Corrige "Supra Centro" → Sé
df_orcamento <- df_orcamento |>
  mutate(
    subprefeitura = case_when(
      subprefeitura == "Supra Centro"              ~ "Sé",
      subprefeitura == "Supra Subprefeitura Centro"~ "Sé",
      TRUE ~ subprefeitura
    )
  )

## 2.6 Filtro de linhas --------------------------------------------------------

# Remove Supra Subprefeituras não identificáveis e registros sem subprefeitura
df_orcamento <- df_orcamento |>
  filter(
    !subprefeitura %in% c(
      "Supra Subprefeitura", "Supra Leste", "Supra Norte",
      "Supra Oeste", "Supra Sul", "Supra Subprefeitura Leste",
      "Supra Subprefeitura Norte", "Supra Subprefeitura Oeste",
      "Supra Subprefeitura Sul"
    ),
    !str_detect(replace_na(subprefeitura, ""), "Supra")
  ) |>
  drop_na(subprefeitura)

# Verifica resultado
cat("Subprefeituras após filtro:", n_distinct(df_orcamento$subprefeitura), "\n")
df_orcamento |> count(subprefeitura, sort = TRUE) |> print(n = 40)

### 2.6.1 Filtro de projetos e atividades (opcional) ---------------------------
if (aplicar_filtro_projetos) {
  df_projetos_e_atividades <- read_excel("Análise_Despesas_IDRGP_2025 3.xlsx")
  lista_proj <- df_projetos_e_atividades$descricao_proj_ativ
  df_orcamento <- df_orcamento |> filter(descricao_proj_ativ %in% lista_proj)
  cat("Filtro projetos/atividades aplicado. Valor restante:",
      sum(df_orcamento$orcamento, na.rm = TRUE), "\n")
}

### 2.6.2 Filtro de investimentos (opcional) -----------------------------------
# MCASP, 11ª ed., p. 77–78: grupo de natureza 4 = Investimentos
if (aplicar_filtro_investimentos) {
  df_orcamento <- df_orcamento |>
    mutate(
      codigo_conta_despesa = as.character(codigo_conta_despesa),
      grupo_despesa = str_sub(codigo_conta_despesa, 2, 2)
    ) |>
    filter(grupo_despesa == "4")
  cat("Filtro investimentos aplicado. Valor restante:",
      sum(df_orcamento$orcamento, na.rm = TRUE), "\n")
}

### 2.6.3 Filtro de funções (opcional) ----------------------------------------
if (aplicar_filtro_funcoes) {
  funcoes_idrgp <- c(
    "Assistência Social", "Trabalho", "Segurança Pública",
    "Saúde", "Educação", "Saneamento", "Transporte", "Urbanismo", "Habitação"
  )
  df_orcamento <- df_orcamento |> filter(descricao_funcao %in% funcoes_idrgp)
  cat("Filtro funções aplicado. Valor restante:",
      sum(df_orcamento$orcamento, na.rm = TRUE), "\n")
}

## 2.7 Seleção de colunas e agregação ------------------------------------------

# Seleciona colunas necessárias
df_orcamento <- df_orcamento |>
  select(descricao_proj_ativ, subprefeitura, valor_detalhamento_acao, orcamento)

# Agrega por subprefeitura
df_agregado <- df_orcamento |>
  group_by(subprefeitura) |>
  summarise(valor = sum(valor_detalhamento_acao, na.rm = TRUE), .groups = "drop")

## 2.8 Controle de qualidade ---------------------------------------------------

controle_orcamento <- tibble(
  total_orc = sum(df_orcamento$valor_detalhamento_acao, na.rm = TRUE),
  total_agr = sum(df_agregado$valor, na.rm = TRUE),
  n_subpref = n_distinct(df_agregado$subprefeitura),
  iguais    = round(sum(df_orcamento$valor_detalhamento_acao, na.rm = TRUE), 2) ==
    round(sum(df_agregado$valor, na.rm = TRUE), 2)
)

print(controle_orcamento)

## 2.9 Conclusão ---------------------------------------------------------------

cat("\n✅ Dados de orçamento 2025 processados com sucesso.\n")
cat("   Total regionalizado: R$",
    scales::label_number(big.mark = ".", decimal.mark = ",")(sum(df_agregado$valor)), "\n")
cat("   Subprefeituras:      ", n_distinct(df_agregado$subprefeitura), "\n\n")


# 03. IDRGP ALVO ===============================================================

## 3.1 Introdução --------------------------------------------------------------

# O IDRGP Alvo é o índice de referência que indica qual proporção dos gastos
# públicos deveria ser destinada a cada subprefeitura. É calculado com base no
# Índice de Vulnerabilidade Urbana (IVU), elaborado pela SEPLAN/CPMA.

## 3.2 IDRGP 2022 (índice de referência original do ciclo PPA) ----------------

if (aplicar_indice_2022) {
  
  df_idrgp <- tibble(
    subprefeitura = c(
      "Subprefeitura Capela do Socorro",    "Subprefeitura M'Boi Mirim",
      "Subprefeitura Campo Limpo",          "Subprefeitura São Mateus",
      "Subprefeitura Itaquera",             "Subprefeitura Cidade Ademar",
      "Subprefeitura Freguesia/Brasilândia","Subprefeitura São Miguel Paulista",
      "Subprefeitura Itaim Paulista",       "Subprefeitura Pirituba/Jaraguá",
      "Subprefeitura Parelheiros",          "Subprefeitura Jaçanã/Tremembé",
      "Subprefeitura Sapopemba",            "Subprefeitura de Guaianases",
      "Subprefeitura Penha",                "Subprefeitura Ipiranga",
      "Subprefeitura Cidade Tiradentes",    "Subprefeitura Casa Verde/Cachoeirinha",
      "Subprefeitura Perus/Anhanguera",     "Subprefeitura Butantã",
      "Subprefeitura Ermelino Matarazzo",   "Subprefeitura de Vila Prudente",
      "Subprefeitura Sé",                   "Subprefeitura Vila Maria/Vila Guilherme",
      "Subprefeitura Aricanduva/Formosa/Carrão", "Subprefeitura Jabaquara",
      "Subprefeitura Mooca",                "Subprefeitura Santana/Tucuruvi",
      "Subprefeitura Lapa",                 "Subprefeitura Santo Amaro",
      "Subprefeitura Vila Mariana",         "Subprefeitura Pinheiros"
    ),
    idrgp_alvo = c(
      0.0708, 0.0706, 0.0616, 0.0511, 0.0487, 0.0483, 0.0456, 0.0419,
      0.0406, 0.0377, 0.0374, 0.0364, 0.0353, 0.0346, 0.0346, 0.0291,
      0.0278, 0.0270, 0.0258, 0.0250, 0.0210, 0.0183, 0.0179, 0.0167,
      0.0157, 0.0150, 0.0150, 0.0146, 0.0113, 0.0094, 0.0086, 0.0068
    )
  ) |>
    mutate(
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    ""),
      subprefeitura = str_replace_all(subprefeitura, "-", "/")
    ) |>
    arrange(desc(idrgp_alvo))
  
  cat("IDRGP 2022 carregado. Soma do índice:", round(sum(df_idrgp$idrgp_alvo), 6), "\n")
}

## 3.3 IDRGP 2024 (baseado no IVU 2024) ----------------------------------------

if (aplicar_indice_2024) {
  
  df_idrgp <- read_excel("df_integrado_2024.xlsx", sheet = "Sheet1") |>
    clean_names() |>
    select(subprefeitura, idrgp_alvo) |>
    mutate(
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    "")
    )
  
  cat("IDRGP 2024 carregado. Soma do índice:", round(sum(df_idrgp$idrgp_alvo, na.rm = TRUE), 6), "\n")
}

## 3.4 IDRGP 2025 (baseado no IVU 2025 — índice ativo) ------------------------

if (aplicar_indice_2025) {
  
  df_idrgp <- read_excel("ivu_subprefeituras_2025.xlsx") |>
    clean_names() |>
    select(nome_subpref, ivu_geral) |>
    rename(subprefeitura = nome_subpref) |>
    mutate(
      ivu_geral     = as.numeric(ivu_geral),
      idrgp_alvo    = ivu_geral / sum(ivu_geral, na.rm = TRUE),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    ""),
      # Corrige nomes divergentes detectados no diagnóstico
      subprefeitura = case_when(
        subprefeitura == "São Miguel"     ~ "São Miguel Paulista",
        subprefeitura == "Perus"          ~ "Perus/Anhanguera",
        subprefeitura == "Itaim"          ~ "Itaim Paulista",
        TRUE ~ subprefeitura
      )
    ) |>
    select(subprefeitura, idrgp_alvo)
  
  cat("IDRGP 2025 carregado. Soma do índice:", round(sum(df_idrgp$idrgp_alvo, na.rm = TRUE), 6), "\n")
}

## 3.5 Controle de qualidade ---------------------------------------------------

soma_idrgp <- sum(df_idrgp$idrgp_alvo, na.rm = TRUE)

if (abs(soma_idrgp - 1) > 0.001) {
  warning("ATENÇÃO: o IDRGP Alvo não soma 1 (soma = ", round(soma_idrgp, 6), "). Verifique a base.")
} else {
  cat("✅ IDRGP Alvo verificado: soma =", round(soma_idrgp, 6), "\n")
}

# Adiciona join_key ao df_idrgp
df_idrgp <- df_idrgp |>
  mutate(
    subprefeitura = str_squish(subprefeitura),
    join_key      = norm_name(subprefeitura)
  )

# Status do índice em uso
cat(if_else(aplicar_indice_2022, "✅ IDRGP 2022 em uso.\n", "   IDRGP 2022 não está em uso.\n"))
cat(if_else(aplicar_indice_2024, "✅ IDRGP 2024 em uso.\n", "   IDRGP 2024 não está em uso.\n"))
cat(if_else(aplicar_indice_2025, "✅ IDRGP 2025 em uso.\n", "   IDRGP 2025 não está em uso.\n"))

cat("\n✅ Tabela do IDRGP Alvo processada com sucesso.\n\n")


# 04. MAPA ====================================================================

## 4.1 Introdução --------------------------------------------------------------

# Nesta seção são importados e processados os dados cartográficos das
# 32 subprefeituras de São Paulo, necessários para a produção dos mapas temáticos.

## 4.2 Coleta ------------------------------------------------------------------

subprefeitura_sf <- read_sf("subprefeitura_v2.shp") |>
  clean_names() |>
  st_make_valid()

# Confere sistema de referência de coordenadas
st_crs(subprefeitura_sf)

## 4.3 Padronização de nomes ---------------------------------------------------

# Colunas confirmadas no shapefile:
#   sg_subpref  → sigla da subprefeitura (ex: "AD", "BT", ...)
#   nm_subpref  → nome em MAIÚSCULAS (ex: "CIDADE ADEMAR")
# A tabela df_siglas garante nomes com grafia oficial (acentos, barras, etc.)

# Tabela de correspondência sigla → nome oficial
df_siglas <- tibble(
  sigla = c("AF","BT","CL","CS","CV","AD","CT","EM","FO","GU","IP","TP","IQ",
            "JA","JT","LA","MB","MO","PA","PE","PR","PI","PJ","ST","SA","SM",
            "MP","SB","SE","MG","VM","VP"),
  subprefeitura = c(
    "Aricanduva/Formosa/Carrão", "Butantã",               "Campo Limpo",
    "Capela do Socorro",          "Casa Verde/Cachoeirinha","Cidade Ademar",
    "Cidade Tiradentes",          "Ermelino Matarazzo",    "Freguesia/Brasilândia",
    "Guaianases",                 "Ipiranga",              "Itaim Paulista",
    "Itaquera",                   "Jabaquara",             "Jaçanã/Tremembé",
    "Lapa",                       "M'Boi Mirim",           "Mooca",
    "Parelheiros",                "Penha",                 "Perus/Anhanguera",
    "Pinheiros",                  "Pirituba/Jaraguá",      "Santana/Tucuruvi",
    "Santo Amaro",                "São Mateus",            "São Miguel Paulista",
    "Sapopemba",                  "Sé",                    "Vila Maria/Vila Guilherme",
    "Vila Mariana",               "Vila Prudente"
  )
)

# Padroniza o shapefile usando sg_subpref como chave estável
subprefeitura_sf <- subprefeitura_sf |>
  rename(sigla = sg_subpref) |>
  left_join(df_siglas, by = "sigla") |>
  mutate(
    subprefeitura = str_squish(subprefeitura),
    join_key      = norm_name(subprefeitura)
  ) |>
  select(sigla, subprefeitura, join_key, geometry)

## 4.4 Controle de qualidade ---------------------------------------------------

cat("Linhas no shapefile:", nrow(subprefeitura_sf), "(esperado: 32)\n")
cat("Subprefeituras com NA:", sum(is.na(subprefeitura_sf$subprefeitura)), "\n")

## 4.5 Conclusão ---------------------------------------------------------------

cat("\n✅ Dados cartográficos processados com sucesso.\n\n")


# 05. INTEGRAÇÃO 2025 =========================================================

## 5.1 Introdução --------------------------------------------------------------

# Nesta seção os dados de orçamento, IDRGP Alvo e mapa são integrados
# em um único data frame geoespacial: df_integrado.

## 5.2 Adiciona join_key ao orçamento agregado --------------------------------

df_agregado_k <- df_agregado |>
  mutate(
    subprefeitura = str_squish(subprefeitura),
    join_key      = norm_name(subprefeitura)
  )

## 5.3 Verificação pré-integração ---------------------------------------------

# Subprefeituras no mapa sem correspondência no IDRGP
mapa_sem_idrgp <- anti_join(
  subprefeitura_sf |> st_drop_geometry() |> select(subprefeitura, join_key),
  df_idrgp |> select(subprefeitura, join_key),
  by = "join_key"
)

# Subprefeituras no IDRGP sem correspondência no mapa
idrgp_sem_mapa <- anti_join(
  df_idrgp |> select(subprefeitura, join_key),
  subprefeitura_sf |> st_drop_geometry() |> select(subprefeitura, join_key),
  by = "join_key"
)

# Subprefeituras no mapa sem correspondência no orçamento
mapa_sem_orc <- anti_join(
  subprefeitura_sf |> st_drop_geometry() |> select(subprefeitura, join_key),
  df_agregado_k |> select(subprefeitura, join_key),
  by = "join_key"
)

cat("Divergências mapa ↔ IDRGP:", nrow(mapa_sem_idrgp), "\n")
cat("Divergências IDRGP ↔ mapa:", nrow(idrgp_sem_mapa), "\n")
cat("Divergências mapa ↔ orçamento:", nrow(mapa_sem_orc), "\n")

if (nrow(mapa_sem_idrgp) > 0) print(mapa_sem_idrgp)
if (nrow(idrgp_sem_mapa) > 0) print(idrgp_sem_mapa)

## 5.4 Integração via join_key ------------------------------------------------

df_integrado <- subprefeitura_sf |>
  left_join(df_idrgp    |> select(join_key, idrgp_alvo), by = "join_key") |>
  left_join(df_agregado_k |> select(join_key, valor),    by = "join_key") |>
  mutate(
    valor      = replace_na(valor,      0),
    idrgp_alvo = replace_na(idrgp_alvo, 0)
  )

## 5.5 Transformação ----------------------------------------------------------

total_valor <- sum(df_integrado$valor, na.rm = TRUE)

df_integrado <- df_integrado |>
  mutate(
    idrgp_real           = if (total_valor > 0) valor / total_valor else NA_real_,
    idrgp_diferenca      = idrgp_real - idrgp_alvo,
    idrgp_var_percentual = if_else(
      idrgp_alvo == 0, 0, idrgp_diferenca / idrgp_alvo
    ),
    valor_previsto       = total_valor * idrgp_alvo,
    status_valor         = if_else(
      valor < valor_previsto,
      "Valor previsto (R$) não atingido",
      "Valor previsto (R$) atingido"
    )
  )

## 5.6 Controle de qualidade --------------------------------------------------

cat("Soma IDRGP Real  :", round(sum(df_integrado$idrgp_real,  na.rm = TRUE), 6), "\n")
cat("Soma IDRGP Alvo  :", round(sum(df_integrado$idrgp_alvo, na.rm = TRUE), 6), "\n")
cat("Total valor (R$) :", sum(df_integrado$valor, na.rm = TRUE), "\n")

cat("\n✅ Integração 2025 concluída com sucesso.\n\n")


# 06. HISTÓRICO 2022–2024 =====================================================

## 6.1 Introdução --------------------------------------------------------------

# Nesta seção são importados os dados históricos já processados dos anos
# 2022, 2023 e 2024, que serão usados para a análise do ciclo PPA e
# para a composição dos mapas e gráficos históricos.

## 6.2 Coleta ------------------------------------------------------------------

# Os arquivos df_integrado_20XX.xlsx já estão processados e contêm:
# subprefeitura, valor, idrgp_alvo, idrgp_real, status (teste_t ou wilcoxon)

df_hist_2022 <- read_excel("df_integrado_2022.xlsx") |>
  clean_names() |>
  mutate(ano = 2022)

df_hist_2023 <- read_excel("df_integrado_2023.xlsx") |>
  clean_names() |>
  mutate(ano = 2023)

df_hist_2024 <- read_excel("df_integrado_2024.xlsx") |>
  clean_names() |>
  mutate(ano = 2024)

# Verifica colunas disponíveis (importante para renomear corretamente)
cat("Colunas 2022:", names(df_hist_2022), "\n")
cat("Colunas 2023:", names(df_hist_2023), "\n")
cat("Colunas 2024:", names(df_hist_2024), "\n")

## 6.3 Padronização e extração das colunas necessárias ------------------------

# Padroniza cada base para usar sempre: subprefeitura, valor, idrgp_alvo, idrgp_real, status
# O campo de status pode variar entre anos (status_teste_t vs status_wilcoxon)

padroniza_hist <- function(df, ano_ref) {
  # Coluna de status preferida: status_wilcoxon (disponível em 2022, 2023 e 2024),
  # com fallback para status_teste_t e depois status_var_percentual.
  col_status <- intersect(
    c("status_wilcoxon", "status_teste_t", "status_var_percentual", "status"),
    names(df)
  )[1]
  
  df |>
    mutate(subprefeitura = str_squish(as.character(subprefeitura))) |>
    select(
      subprefeitura,
      valor,
      idrgp_alvo,
      idrgp_real,
      status = all_of(col_status)
    ) |>
    rename_with(
      ~ paste0(., "_", ano_ref),
      .cols = c("valor", "idrgp_real", "status")
    )
}

df_h22 <- padroniza_hist(df_hist_2022, 2022)
df_h23 <- padroniza_hist(df_hist_2023, 2023)
df_h24 <- padroniza_hist(df_hist_2024, 2024)

## 6.4 Dados do ano atual (2025) para série histórica -------------------------

df_h25 <- df_integrado |>
  st_drop_geometry() |>
  # status_wilcoxon ou status_estatistico — será inserido após seção 07
  select(subprefeitura, valor, idrgp_alvo, idrgp_real) |>
  rename(valor_2025 = valor, idrgp_real_2025 = idrgp_real)

## 6.5 Consolidação do ciclo completo -----------------------------------------

# Consolidação com join sequencial por subprefeitura
df_ciclo <- df_h22 |>
  left_join(df_h23 |> select(-idrgp_alvo), by = "subprefeitura") |>
  left_join(df_h24 |> select(-idrgp_alvo), by = "subprefeitura") |>
  left_join(df_h25 |> select(-idrgp_alvo), by = "subprefeitura") |>
  mutate(
    # Totais do ciclo
    valor_2022_a_2025 = rowSums(
      across(c(valor_2022, valor_2023, valor_2024, valor_2025)),
      na.rm = TRUE
    ),
    # IDRGP Real agregado do ciclo
    idrgp_real_2022_a_2025 = valor_2022_a_2025 / sum(valor_2022_a_2025, na.rm = TRUE),
    # Diferença e variação percentual do ciclo
    idrgp_diferenca_ciclo = idrgp_real_2022_a_2025 - idrgp_alvo,
    idrgp_var_pct_ciclo   = if_else(
      idrgp_alvo == 0, 0, idrgp_diferenca_ciclo / idrgp_alvo
    ),
    # Status de valor do ciclo
    valor_previsto_ciclo = idrgp_alvo * valor_previsto_ciclo_completo,
    status_valor_ciclo   = if_else(
      valor_2022_a_2025 < valor_previsto_ciclo,
      "Valor previsto (R$) não atingido",
      "Valor previsto (R$) atingido"
    ),
    # Classificação estatística do ciclo por variação percentual (±30%)
    # Criada aqui em df_ciclo para uso tanto em gráficos quanto em df_ciclo_sf
    status_ciclo = case_when(
      idrgp_var_pct_ciclo >  0.3  ~ "Acima do IDRGP Alvo",
      idrgp_var_pct_ciclo < -0.3  ~ "Abaixo do IDRGP Alvo",
      TRUE                        ~ "Dentro do IDRGP Alvo"
    )
  )

## 6.6 Join com o mapa para versão geoespacial --------------------------------

# df_ciclo_sf herda status_ciclo diretamente de df_ciclo via join
df_ciclo_sf <- subprefeitura_sf |>
  mutate(join_key = norm_name(subprefeitura)) |>
  left_join(
    df_ciclo |> mutate(join_key = norm_name(subprefeitura)),
    by = "join_key"
  )

## 6.7 Formato longo para gráficos históricos ---------------------------------

# Formato longo: 1 linha por (subprefeitura × ano)
# Nota: padroniza_hist() renomeia 'valor' → 'valor_XXXX' e 'idrgp_real' → 'idrgp_real_XXXX'
df_longo <- bind_rows(
  df_h22 |>
    select(subprefeitura, valor = valor_2022, idrgp_real = idrgp_real_2022, idrgp_alvo) |>
    mutate(ano = 2022L),
  df_h23 |>
    select(subprefeitura, valor = valor_2023, idrgp_real = idrgp_real_2023, idrgp_alvo) |>
    mutate(ano = 2023L),
  df_h24 |>
    select(subprefeitura, valor = valor_2024, idrgp_real = idrgp_real_2024, idrgp_alvo) |>
    mutate(ano = 2024L),
  df_integrado |> st_drop_geometry() |>
    select(subprefeitura, valor, idrgp_real, idrgp_alvo) |>
    mutate(ano = 2025L)
) |>
  mutate(ano = as.factor(ano))

## 6.8 Controle de qualidade --------------------------------------------------

cat("Ciclo completo — somas de valor por ano:\n")
cat("  2022: R$", round(sum(df_ciclo$valor_2022, na.rm = TRUE) / 1e9, 2), "bi\n")
cat("  2023: R$", round(sum(df_ciclo$valor_2023, na.rm = TRUE) / 1e9, 2), "bi\n")
cat("  2024: R$", round(sum(df_ciclo$valor_2024, na.rm = TRUE) / 1e9, 2), "bi\n")
cat("  2025: R$", round(sum(df_ciclo$valor_2025, na.rm = TRUE) / 1e9, 2), "bi\n")
cat("  Total ciclo: R$", round(sum(df_ciclo$valor_2022_a_2025, na.rm = TRUE) / 1e9, 2), "bi\n")

cat("\n✅ Dados históricos 2022–2024 processados com sucesso.\n\n")


# 07. TESTES ESTATÍSTICOS =====================================================

## 7.1 Introdução --------------------------------------------------------------

# Cada subprefeitura é classificada quanto ao atingimento do IDRGP Alvo por
# dois critérios complementares:
#
#   a) Variação percentual: tolerância de ±30% em relação ao IDRGP Alvo.
#      Critério operacional, de fácil comunicação.
#
#   b) Critério estatístico (intervalo de confiança a 95%):
#      Aplica-se teste t pareado ou Wilcoxon pareado sobre as diferenças
#      (idrgp_real − idrgp_alvo), conforme resultado dos testes de normalidade.
#
# Justificativa da abordagem:
#   - Os testes de normalidade são aplicados sobre o VETOR DE DIFERENÇAS PAREADAS
#     (idrgp_real − idrgp_alvo), e não sobre cada variável individualmente.
#   - Isso é metodologicamente correto: o que determina o teste a usar é a
#     distribuição das diferenças, não a dos vetores originais (Field, 2013;
#     Hollander et al., 2014).
#   - Critério conservador: se qualquer teste indicar não normalidade (p < 0,05),
#     utiliza-se o teste de Wilcoxon, mais robusto para distribuições assimétricas.

## 7.2 Preparação da base analítica -------------------------------------------

df_teste <- df_integrado |>
  st_drop_geometry() |>
  select(subprefeitura, valor, idrgp_alvo, idrgp_real, idrgp_diferenca, idrgp_var_percentual) |>
  filter(!is.na(idrgp_alvo), !is.na(idrgp_real), !is.na(idrgp_diferenca))

cat("Observações para teste estatístico:", nrow(df_teste), "(esperado: 32)\n")

## 7.3 Classificação por variação percentual (±30%) ---------------------------

df_teste <- df_teste |>
  mutate(
    status_var_percentual = case_when(
      idrgp_var_percentual >  0.3  ~ "Acima do IDRGP Alvo",
      idrgp_var_percentual < -0.3  ~ "Abaixo do IDRGP Alvo",
      TRUE                         ~ "Dentro do IDRGP Alvo"
    )
  )

cat("\nClassificação por variação percentual (±30%):\n")
print(df_teste |> count(status_var_percentual))

## 7.4 Testes de normalidade sobre as diferenças pareadas ---------------------

# Vetor de diferenças pareadas: é sobre ISSO que se aplica o teste de normalidade.
vetor_dif <- df_teste$idrgp_diferenca

# Teste de Shapiro-Wilk (indicado para n ≤ 50)
teste_shapiro <- shapiro.test(vetor_dif)

# Teste de Kolmogorov-Smirnov com vetor padronizado (comparação com N(0,1))
vetor_dif_z <- scale(vetor_dif)[, 1]
teste_ks     <- ks.test(vetor_dif_z, "pnorm")

cat("\n--- Teste de Shapiro-Wilk (diferenças pareadas) ---\n")
print(teste_shapiro)

cat("\n--- Teste de Kolmogorov-Smirnov (diferenças padronizadas) ---\n")
print(teste_ks)

## 7.5 Seleção do método estatístico ------------------------------------------

# Critério conservador: basta um dos testes indicar não normalidade.
if (teste_shapiro$p.value < 0.05 | teste_ks$p.value < 0.05) {
  metodo_estatistico <- "wilcoxon"
  mensagem_metodo <- paste0(
    "Distribuição das diferenças não normal ",
    "(Shapiro-Wilk p = ", formatC(teste_shapiro$p.value, format = "e", digits = 3),
    "; KS p = ",           formatC(teste_ks$p.value,     format = "e", digits = 3), "). ",
    "Método selecionado: Wilcoxon pareado (não paramétrico)."
  )
} else {
  metodo_estatistico <- "t_pareado"
  mensagem_metodo <- paste0(
    "Distribuição das diferenças compatível com normalidade ",
    "(Shapiro-Wilk p = ", formatC(teste_shapiro$p.value, format = "f", digits = 3),
    "; KS p = ",           formatC(teste_ks$p.value,     format = "f", digits = 3), "). ",
    "Método selecionado: teste t pareado (paramétrico)."
  )
}

cat("\n", mensagem_metodo, "\n\n")

## 7.6 Teste principal --------------------------------------------------------

if (metodo_estatistico == "t_pareado") {
  
  # Teste t pareado
  teste_principal <- t.test(
    df_teste$idrgp_real, df_teste$idrgp_alvo,
    paired = TRUE, conf.level = 0.95
  )
  
  medida_central <- unname(teste_principal$estimate)  # média das diferenças
  ic_inf <- teste_principal$conf.int[1]
  ic_sup <- teste_principal$conf.int[2]
  
} else {
  
  # Teste de Wilcoxon pareado
  # exact = FALSE evita aviso quando há empates
  teste_principal <- wilcox.test(
    df_teste$idrgp_real, df_teste$idrgp_alvo,
    paired = TRUE, conf.int = TRUE, conf.level = 0.95, exact = FALSE
  )
  
  medida_central <- median(df_teste$idrgp_diferenca, na.rm = TRUE)  # mediana
  ic_inf <- teste_principal$conf.int[1]
  ic_sup <- teste_principal$conf.int[2]
}

cat("--- Resultado do teste", toupper(metodo_estatistico), "---\n")
print(teste_principal)

# Tabela-resumo do teste
tabela_resultado_teste <- tibble(
  metodo              = metodo_estatistico,
  p_valor             = teste_principal$p.value,
  estimativa_central  = medida_central,
  ic_inferior_95pct   = ic_inf,
  ic_superior_95pct   = ic_sup,
  n                   = nrow(df_teste)
)

cat("\n--- Tabela-resumo do teste estatístico ---\n")
print(tabela_resultado_teste)

## 7.7 Classificação estatística individual -----------------------------------

# Cada subprefeitura é classificada comparando sua diferença individual
# com os limites do intervalo de confiança do teste principal.

df_teste <- df_teste |>
  mutate(
    status_estatistico = case_when(
      idrgp_diferenca < ic_inf ~ "Abaixo do IDRGP Alvo",
      idrgp_diferenca > ic_sup ~ "Acima do IDRGP Alvo",
      TRUE                     ~ "Dentro do IDRGP Alvo"
    )
  )

cat("\nClassificação estatística (IC do teste", toupper(metodo_estatistico), "):\n")
print(df_teste |> count(status_estatistico))

## 7.8 Incorpora classificações ao df_integrado --------------------------------

df_integrado <- df_integrado |>
  select(-any_of(c("status_var_percentual", "status_estatistico"))) |>
  left_join(
    df_teste |> select(subprefeitura, status_var_percentual, status_estatistico),
    by = "subprefeitura"
  )

## 7.9 Comparação entre critérios de classificação ---------------------------

divergencias <- df_integrado |>
  st_drop_geometry() |>
  select(subprefeitura, status_var_percentual, status_estatistico) |>
  filter(status_var_percentual != status_estatistico)

cat("\nSubprefeituras com classificação divergente entre os critérios:\n")
if (nrow(divergencias) == 0) {
  cat("  Nenhuma divergência encontrada.\n")
} else {
  print(divergencias)
}

## 7.10 Visualização da distribuição das diferenças ---------------------------

grafico_dist <- ggplot(df_teste, aes(x = idrgp_var_percentual)) +
  geom_histogram(
    aes(y = after_stat(density)), binwidth = 0.05,
    fill = "#0ea1cf", color = "white", alpha = 0.75
  ) +
  geom_density(color = "#1A1442", linewidth = 1) +
  geom_vline(xintercept = c(-0.3, 0.3), linetype = "dashed", color = "#d97a0f", linewidth = 0.8) +
  annotate("text", x = -0.32, y = Inf, label = "−30%", hjust = 1, vjust = 1.5,
           size = 3.5, color = "#d97a0f", fontface = "bold") +
  annotate("text", x =  0.32, y = Inf, label = "+30%", hjust = 0, vjust = 1.5,
           size = 3.5, color = "#d97a0f", fontface = "bold") +
  theme_minimal(base_size = 12) +
  labs(
    title    = "Distribuição da Variação Percentual do IDRGP Real em relação ao Alvo — 2025",
    subtitle = paste("Método estatístico utilizado:", toupper(metodo_estatistico),
                     "| Linhas tracejadas: limites de tolerância de ±30%"),
    x        = "Variação percentual (IDRGP Real − IDRGP Alvo) / IDRGP Alvo",
    y        = "Densidade",
    caption  = paste0(
      "Elaboração: SEPLAN/CPMA.\n",
      "Nota metodológica: testes de normalidade aplicados sobre o vetor de diferenças pareadas\n",
      "(Shapiro-Wilk W = ", round(teste_shapiro$statistic, 4),
      ", p = ", formatC(teste_shapiro$p.value, format = "e", digits = 2),
      "; KS D = ", round(teste_ks$statistic, 4),
      ", p = ", formatC(teste_ks$p.value, format = "e", digits = 2), ").\n",
      mensagem_metodo
    )
  )

print(grafico_dist)

## 7.11 Conclusão do bloco de testes ------------------------------------------

cat("\n=== CONCLUSÃO DOS TESTES ESTATÍSTICOS ===\n")
cat(mensagem_metodo, "\n\n")
cat("Intervalo de confiança (95%) utilizado para classificação:\n")
cat("  Limite inferior:", round(ic_inf, 6), "\n")
cat("  Limite superior:", round(ic_sup, 6), "\n\n")

if (metodo_estatistico == "wilcoxon") {
  cat("Justificativa técnica:\n")
  cat("  A distribuição das diferenças não é normal (pelo menos um teste p < 0,05).\n")
  cat("  A SEPLAN/CPMA optou pelo teste de Wilcoxon pareado (não paramétrico),\n")
  cat("  mais robusto para distribuições assimétricas e na presença de outliers.\n\n")
} else {
  cat("Justificativa técnica:\n")
  cat("  A distribuição das diferenças é compatível com a normalidade (ambos p ≥ 0,05).\n")
  cat("  O teste t pareado (paramétrico) foi utilizado.\n\n")
}

cat("✅ Testes estatísticos aplicados com sucesso.\n\n")


# 08. MAPAS ===================================================================

# Paleta e tema base comuns a todos os mapas
cores_status <- c(
  "Abaixo do IDRGP Alvo" = "#d97a0f",
  "Dentro do IDRGP Alvo" = "#0ea1cf",
  "Acima do IDRGP Alvo"  = "#1A1442"
)

tema_mapa <- theme_void(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle      = element_text(size = 10, hjust = 0),
    plot.caption       = element_text(size = 7,  hjust = 0),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.margin        = margin(10, 10, 10, 10),
    legend.position    = "right",
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8)
  )

caption_base <- "Elaboração: SEPLAN/CPMA.\nFonte: Secretaria Municipal da Fazenda — https://orcamento.sf.prefeitura.sp.gov.br"

## M1 – Mapa IDRGP Real 2022 --------------------------------------------------

# Integra dados históricos 2022 com geometria
df_mapa_2022 <- subprefeitura_sf |>
  left_join(
    df_h22 |>
      mutate(join_key = norm_name(subprefeitura)) |>
      select(join_key, idrgp_real_2022, status_2022),
    by = "join_key"
  )

mapa1 <- ggplot(df_mapa_2022) +
  geom_sf(aes(fill = status_2022), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
  labs(
    title    = "IDRGP 2022 — Status quanto ao IDRGP Alvo",
    subtitle = "Classificação estatística de cada subprefeitura em relação ao índice de referência",
    caption  = paste0(caption_base, "\nNota: classificação baseada no teste estatístico aplicado ao ciclo 2022.")
  ) +
  tema_mapa

print(mapa1)

## M2 – Mapa IDRGP Real 2023 --------------------------------------------------

df_mapa_2023 <- subprefeitura_sf |>
  left_join(
    df_h23 |>
      mutate(join_key = norm_name(subprefeitura)) |>
      select(join_key, idrgp_real_2023, status_2023),
    by = "join_key"
  )

mapa2 <- ggplot(df_mapa_2023) +
  geom_sf(aes(fill = status_2023), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
  labs(
    title    = "IDRGP 2023 — Status quanto ao IDRGP Alvo",
    subtitle = "Classificação estatística de cada subprefeitura em relação ao índice de referência",
    caption  = paste0(caption_base, "\nNota: classificação baseada no teste estatístico aplicado ao ciclo 2023.")
  ) +
  tema_mapa

print(mapa2)

## M3 – Mapa IDRGP Real 2024 --------------------------------------------------

df_mapa_2024 <- subprefeitura_sf |>
  left_join(
    df_h24 |>
      mutate(join_key = norm_name(subprefeitura)) |>
      select(join_key, idrgp_real_2024, status_2024),
    by = "join_key"
  )

mapa3 <- ggplot(df_mapa_2024) +
  geom_sf(aes(fill = status_2024), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
  labs(
    title    = "IDRGP 2024 — Status quanto ao IDRGP Alvo",
    subtitle = "Classificação estatística de cada subprefeitura em relação ao índice de referência",
    caption  = paste0(caption_base, "\nNota: classificação baseada no teste estatístico aplicado ao ciclo 2024.")
  ) +
  tema_mapa

print(mapa3)

## M4 – Mapa IDRGP Real 2025 --------------------------------------------------

mapa4 <- ggplot(df_integrado) +
  geom_sf(aes(fill = status_estatistico), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
  labs(
    title    = "IDRGP 2025 — Status quanto ao IDRGP Alvo",
    subtitle = "Classificação estatística de cada subprefeitura em relação ao índice de referência",
    caption  = paste0(
      caption_base, "\n",
      "Nota técnica: classificação pelo IC do teste ", toupper(metodo_estatistico),
      " (95%). Subprefeituras 'Dentro do IDRGP Alvo' apresentam diferença entre",
      " os limites [", round(ic_inf, 5), "; ", round(ic_sup, 5), "]."
    )
  ) +
  tema_mapa

print(mapa4)

## M5 – Mapa IDRGP Agregado 2022–2025 -----------------------------------------

# Classificação do ciclo completo: usa variação percentual de ±30%
df_ciclo_sf <- df_ciclo_sf |>
  mutate(
    status_ciclo = case_when(
      idrgp_var_pct_ciclo >  0.3  ~ "Acima do IDRGP Alvo",
      idrgp_var_pct_ciclo < -0.3  ~ "Abaixo do IDRGP Alvo",
      TRUE                        ~ "Dentro do IDRGP Alvo"
    )
  )

mapa5 <- ggplot(df_ciclo_sf) +
  geom_sf(aes(fill = status_ciclo), color = "white", linewidth = 0.3) +
  geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
  scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
  labs(
    title    = "IDRGP Agregado 2022–2025 — Balanço do Ciclo PPA",
    subtitle = "Status de cada subprefeitura considerando o total do quadriênio",
    caption  = paste0(
      caption_base, "\n",
      "Nota técnica: classificação pela variação percentual (±30%) do IDRGP Real",
      " acumulado 2022–2025 em relação ao IDRGP Alvo."
    )
  ) +
  tema_mapa

print(mapa5)

cat("✅ 5 mapas gerados com sucesso.\n\n")


# 09. GRÁFICOS ================================================================

tema_barras <- theme_minimal(base_size = 12) +
  theme(
    legend.position    = "bottom",
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 7, hjust = 0),
    axis.text.y   = element_text(size = 8)
  )

## G1 – IDRGP 2025: Alvo vs. Real (escala %) ----------------------------------

grafico1 <- ggplot(df_integrado, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(aes(x = idrgp_real, fill = status_estatistico), width = 0.65, alpha = 0.9) +
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), color = "#c0392b", size = 3) +
  scale_fill_manual(values = cores_status, name = "Status:") +
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title    = "IDRGP 2025 — Distribuição Percentual do Gasto por Subprefeitura",
    subtitle = "IDRGP Real (barras) e IDRGP Alvo (losango) — ordenado pelo índice de referência",
    x        = "IDRGP (%)",
    y        = NULL,
    caption  = paste0(
      caption_base, "\n",
      "Nota técnica: classificação pelo IC do teste ", toupper(metodo_estatistico),
      " (95%); p = ", formatC(teste_principal$p.value, format = "e", digits = 2), "."
    )
  ) +
  tema_barras

print(grafico1)

## G2 – IDRGP Agregado 2022–2025: Alvo vs. Real (escala %) -------------------

grafico2 <- ggplot(df_ciclo, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(
    aes(x = idrgp_real_2022_a_2025, fill = status_ciclo),
    width = 0.65, alpha = 0.9
  ) +
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), color = "#c0392b", size = 3) +
  scale_fill_manual(
    values = cores_status, name = "Status:",
    limits = names(cores_status)
  ) +
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title    = "IDRGP 2022–2025 — Balanço do Ciclo PPA (Distribuição Percentual)",
    subtitle = "IDRGP Real acumulado (barras) e IDRGP Alvo (losango) — ordenado pelo índice de referência",
    x        = "IDRGP Acumulado (%)",
    y        = NULL,
    caption  = paste0(
      caption_base, "\n",
      "Nota técnica: classificação pela variação percentual (±30%) do IDRGP Real acumulado."
    )
  ) +
  tema_barras

print(grafico2)

## G3 – IDRGP 2025: Valores Absolutos (R$) ------------------------------------

grafico3 <- ggplot(df_integrado, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(
    aes(x = valor, fill = status_valor),
    width = 0.65, alpha = 0.9
  ) +
  geom_point(
    aes(x = valor_previsto, shape = "Valor Previsto (R$)"),
    color = "#c0392b", size = 3
  ) +
  scale_fill_manual(
    values = c(
      "Valor previsto (R$) atingido"     = "#1A1442",
      "Valor previsto (R$) não atingido" = "#d97a0f"
    ),
    name = "Status:"
  ) +
  scale_shape_manual(values = c("Valor Previsto (R$)" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ","
    )
  ) +
  labs(
    title    = "IDRGP 2025 — Valor Previsto e Executado por Subprefeitura (R$)",
    subtitle = paste0(
      "Valor previsto para 2025: R$ ",
      scales::label_number(big.mark = ".", decimal.mark = ",")(valor_previsto_ano),
      " (1/4 do total do ciclo PPA)"
    ),
    x        = "Valor executado (R$)",
    y        = NULL,
    caption  = caption_base
  ) +
  tema_barras

print(grafico3)

## G4 – IDRGP 2022–2025: Valores Absolutos por ano (R$) — empilhado ----------

# Formato longo para barras empilhadas por ano
df_longo_plot <- df_longo |>
  mutate(
    subprefeitura = factor(
      subprefeitura,
      levels = df_ciclo |> arrange(idrgp_alvo) |> pull(subprefeitura)
    ),
    ano = factor(ano, levels = c("2022","2023","2024","2025"))
  )

grafico4 <- ggplot(df_longo_plot, aes(y = subprefeitura, x = valor, fill = ano)) +
  geom_col(width = 0.65, alpha = 0.9, position = "stack") +
  geom_point(
    data = df_ciclo |>
      mutate(
        subprefeitura = factor(subprefeitura, levels = levels(df_longo_plot$subprefeitura)),
        valor_prev_ciclo_total = idrgp_alvo * valor_previsto_ciclo_completo
      ),
    aes(y = subprefeitura, x = valor_prev_ciclo_total, shape = "Valor Previsto Ciclo (R$)"),
    color = "#c0392b", size = 3, inherit.aes = FALSE
  ) +
  scale_fill_manual(
    values = c(
      "2022" = "#0ea1cf", "2023" = "#0d6fa8",
      "2024" = "#1A3D7C", "2025" = "#1A1442"
    ),
    name = "Ano"
  ) +
  scale_shape_manual(values = c("Valor Previsto Ciclo (R$)" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ","
    )
  ) +
  labs(
    title    = "IDRGP 2022–2025 — Evolução do Gasto Real por Subprefeitura (R$)",
    subtitle = paste0(
      "Barras empilhadas por ano | Losango: valor previsto total do ciclo (R$ ",
      scales::label_number(big.mark = ".", decimal.mark = ",")(valor_previsto_ciclo_completo), ")"
    ),
    x        = "Valor executado acumulado (R$)",
    y        = NULL,
    caption  = paste0(caption_base, "\nNota: base histórica 2022–2024 proveniente de df_integrado_20XX.xlsx.")
  ) +
  tema_barras

print(grafico4)



cat("✅ 4 gráficos gerados com sucesso.\n\n")


# 10. ANEXOS ==================================================================

## 10.1 Introdução -------------------------------------------------------------

# O data frame de anexos detalha o valor liquidado por projeto/atividade
# em cada subprefeitura — insumo direto para os anexos do relatório IDRGP.

## 10.2 Criação do df_anexo ---------------------------------------------------

df_anexo <- df_orcamento |>
  group_by(subprefeitura, descricao_proj_ativ) |>
  summarise(valor_liquidado = sum(valor_detalhamento_acao, na.rm = TRUE), .groups = "drop") |>
  rename(projeto_atividade = descricao_proj_ativ) |>
  select(subprefeitura, projeto_atividade, valor_liquidado) |>
  arrange(subprefeitura, projeto_atividade)

## 10.3 Controle de qualidade -------------------------------------------------

cat("Valor df_orcamento:", sum(df_orcamento$valor_detalhamento_acao, na.rm = TRUE), "\n")
cat("Valor df_anexo:    ", sum(df_anexo$valor_liquidado, na.rm = TRUE), "\n")
cat("Subprefeituras em df_anexo:", n_distinct(df_anexo$subprefeitura), "\n")

## 10.4 Conclusão --------------------------------------------------------------

cat("\n✅ Dados de anexos processados com sucesso.\n\n")


# 11. EXPORTAÇÃO ==============================================================

## 11.1 Introdução -------------------------------------------------------------

# Produtos exportados:
#   Mapas   (PNG):  mapa1–mapa5
#   Gráficos (PNG): grafico1–grafico4 + grafico_dist
#   Dados   (XLSX): df_integrado, df_ciclo, df_longo, df_anexo, tabela_resultado_teste
#   Geoespacial:    shapefile e geopackage do IDRGP 2025

## 11.2 Cria pasta de saída ---------------------------------------------------

pasta_saida <- "Resultados IDRGP 2025"
if (!dir.exists(pasta_saida)) dir.create(pasta_saida)

## 11.3 Exporta mapas como PNG (300 dpi) --------------------------------------

ggsave(file.path(pasta_saida, "mapa1_idrgp_2022.png"),         mapa1,        width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapa2_idrgp_2023.png"),         mapa2,        width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapa3_idrgp_2024.png"),         mapa3,        width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapa4_idrgp_2025.png"),         mapa4,        width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapa5_idrgp_agregado_2022_2025.png"), mapa5,  width = 12, height = 10, dpi = 300)

cat("Mapas exportados.\n")

## 11.4 Exporta gráficos como PNG (300 dpi) -----------------------------------

ggsave(file.path(pasta_saida, "grafico1_idrgp_2025_pct.png"),           grafico1,      width = 14, height = 12, dpi = 300)
ggsave(file.path(pasta_saida, "grafico2_idrgp_agregado_2022_2025_pct.png"), grafico2,  width = 14, height = 12, dpi = 300)
ggsave(file.path(pasta_saida, "grafico3_idrgp_2025_reais.png"),         grafico3,      width = 14, height = 12, dpi = 300)
ggsave(file.path(pasta_saida, "grafico4_idrgp_serie_2022_2025_reais.png"), grafico4,   width = 14, height = 12, dpi = 300)
ggsave(file.path(pasta_saida, "grafico_dist_diferencas_2025.png"),      grafico_dist,  width = 10, height = 7,  dpi = 300)

cat("Gráficos exportados.\n")

## 11.5 Exporta dados em Excel ------------------------------------------------

write_xlsx(
  df_integrado |> st_drop_geometry() |> select(-any_of("join_key")),
  path = file.path(pasta_saida, "df_integrado_2025.xlsx")
)

write_xlsx(
  df_ciclo |> select(-any_of("geometry")),
  path = file.path(pasta_saida, "df_ciclo_2022_2025.xlsx")
)

write_xlsx(
  df_longo |> select(-any_of("geometry")),
  path = file.path(pasta_saida, "df_longo_2022_2025.xlsx")
)

write_xlsx(
  df_anexo,
  path = file.path(pasta_saida, "df_anexo_2025.xlsx")
)

write_xlsx(
  tabela_resultado_teste,
  path = file.path(pasta_saida, "tabela_teste_estatistico_2025.xlsx")
)

cat("Planilhas Excel exportadas.\n")

## 11.6 Exporta shapefile e geopackage do IDRGP 2025 --------------------------

# Prepara base com nomes curtos de campos (limite do shapefile: 10 caracteres)
df_shp_2025 <- df_integrado |>
  rename(
    sp_nome  = subprefeitura,
    id_alvo  = idrgp_alvo,
    id_real  = idrgp_real,
    dif_idr  = idrgp_diferenca,
    var_pct  = idrgp_var_percentual,
    vlr_orc  = valor,
    vlr_prev = valor_previsto,
    st_vlr   = status_valor,
    st_var   = status_var_percentual,
    st_est   = status_estatistico
  ) |>
  select(sp_nome, sigla, join_key, vlr_orc, id_alvo, id_real,
         dif_idr, var_pct, st_vlr, st_var, st_est, geometry)

# Geopackage (recomendado para uso no QGIS — sem limite de nome de campo)
arquivo_gpkg <- file.path(pasta_saida, "idrgp_2025.gpkg")
if (file.exists(arquivo_gpkg)) file.remove(arquivo_gpkg)

st_write(
  obj   = df_integrado |> select(-any_of(c("join_key"))),
  dsn   = arquivo_gpkg,
  layer = "idrgp_2025",
  delete_dsn = FALSE
)

# Geopackage do ciclo completo
arquivo_gpkg_ciclo <- file.path(pasta_saida, "idrgp_ciclo_2022_2025.gpkg")
if (file.exists(arquivo_gpkg_ciclo)) file.remove(arquivo_gpkg_ciclo)

st_write(
  obj   = df_ciclo_sf |> select(-any_of(c("join_key", "subprefeitura.y"))),
  dsn   = arquivo_gpkg_ciclo,
  layer = "idrgp_ciclo_2022_2025",
  delete_dsn = FALSE
)

# Shapefile (compatibilidade com SIGs legados)
shp_nome <- paste0("idrgp_2025_", format(Sys.Date(), "%Y%m%d"))

st_write(
  obj          = df_shp_2025,
  dsn          = pasta_saida,
  layer        = shp_nome,
  driver       = "ESRI Shapefile",
  delete_layer = TRUE,
  append       = FALSE
)

cat("Arquivos geoespaciais exportados.\n")

## 11.7 Conclusão da exportação -----------------------------------------------

cat("\n✅ Exportação concluída com sucesso.\n")
cat("   Pasta de saída:", normalizePath(pasta_saida), "\n\n")

# Lista de produtos exportados
cat("=== PRODUTOS GERADOS ===\n")
cat("Mapas (PNG):\n")
cat("  mapa1_idrgp_2022.png\n")
cat("  mapa2_idrgp_2023.png\n")
cat("  mapa3_idrgp_2024.png\n")
cat("  mapa4_idrgp_2025.png\n")
cat("  mapa5_idrgp_agregado_2022_2025.png\n\n")
cat("Gráficos (PNG):\n")
cat("  grafico1_idrgp_2025_pct.png           — IDRGP 2025: alvo vs. real (%)\n")
cat("  grafico2_idrgp_agregado_2022_2025_pct.png — IDRGP ciclo: alvo vs. real (%)\n")
cat("  grafico3_idrgp_2025_reais.png         — IDRGP 2025: valores R$\n")
cat("  grafico4_idrgp_serie_2022_2025_reais.png — IDRGP 2022–2025: série R$\n")
cat("  grafico_dist_diferencas_2025.png      — distribuição das diferenças\n\n")
cat("Dados (XLSX):\n")
cat("  df_integrado_2025.xlsx\n")
cat("  df_ciclo_2022_2025.xlsx\n")
cat("  df_longo_2022_2025.xlsx\n")
cat("  df_anexo_2025.xlsx\n")
cat("  tabela_teste_estatistico_2025.xlsx\n\n")
cat("Geoespacial:\n")
cat("  idrgp_2025.gpkg                       — IDRGP 2025 (GeoPackage/QGIS)\n")
cat("  idrgp_ciclo_2022_2025.gpkg            — Ciclo completo (GeoPackage/QGIS)\n")
cat("  ", shp_nome, ".shp/.dbf/.prj         — Shapefile IDRGP 2025\n\n")


# 11.8 PASTA DE DIVULGAÇÃO — PDFs + SHPs + Tabelas por Subprefeitura =========

## 11.8.1 Cria pasta de divulgação --------------------------------------------

pasta_divulgacao <- file.path(pasta_saida, "Divulgacao_IDRGP_2025")
if (!dir.exists(pasta_divulgacao)) dir.create(pasta_divulgacao)

# Subpastas temáticas
pasta_mapas_div   <- file.path(pasta_divulgacao, "01_Mapas")
pasta_graficos_div<- file.path(pasta_divulgacao, "02_Graficos")
pasta_shp_div     <- file.path(pasta_divulgacao, "03_Shapefiles")
pasta_tabelas_div <- file.path(pasta_divulgacao, "04_Tabelas_Subprefeituras")

for (p in c(pasta_mapas_div, pasta_graficos_div, pasta_shp_div, pasta_tabelas_div)) {
  if (!dir.exists(p)) dir.create(p)
}

## 11.8.2 Exporta mapas em PNG e PDF -------------------------------------------

lista_mapas <- list(
  mapa1_idrgp_2022              = mapa1,
  mapa2_idrgp_2023              = mapa2,
  mapa3_idrgp_2024              = mapa3,
  mapa4_idrgp_2025              = mapa4,
  mapa5_idrgp_agregado_2022_2025= mapa5
)

for (nm in names(lista_mapas)) {
  # PNG (alta resolução)
  ggsave(
    filename = file.path(pasta_mapas_div, paste0(nm, ".png")),
    plot     = lista_mapas[[nm]],
    width    = 12, height = 10, dpi = 300
  )
  # PDF (vetorial, ideal para publicação)
  ggsave(
    filename = file.path(pasta_mapas_div, paste0(nm, ".pdf")),
    plot     = lista_mapas[[nm]],
    width    = 12, height = 10, device = cairo_pdf
  )
}

cat("Mapas exportados (PNG + PDF) para", pasta_mapas_div, "\n")

## 11.8.3 Exporta gráficos em PNG e PDF ----------------------------------------

lista_graficos <- list(
  grafico1_idrgp_2025_pct                = grafico1,
  grafico2_idrgp_agregado_2022_2025_pct  = grafico2,
  grafico3_idrgp_2025_reais              = grafico3,
  grafico4_idrgp_serie_2022_2025_reais   = grafico4,
  grafico_dist_diferencas_2025           = grafico_dist
)

for (nm in names(lista_graficos)) {
  ggsave(
    filename = file.path(pasta_graficos_div, paste0(nm, ".png")),
    plot     = lista_graficos[[nm]],
    width    = 14, height = 12, dpi = 300
  )
  ggsave(
    filename = file.path(pasta_graficos_div, paste0(nm, ".pdf")),
    plot     = lista_graficos[[nm]],
    width    = 14, height = 12, device = cairo_pdf
  )
}

# O gráfico grafico_dist tem proporção diferente
ggsave(
  filename = file.path(pasta_graficos_div, "grafico_dist_diferencas_2025.pdf"),
  plot     = grafico_dist,
  width    = 10, height = 7, device = cairo_pdf
)

cat("Gráficos exportados (PNG + PDF) para", pasta_graficos_div, "\n")

## 11.8.4 Copia shapefiles para pasta de divulgação ----------------------------

# Lista todos os arquivos do shapefile IDRGP 2025 gerado na seção 11.6
arquivos_shp <- list.files(
  path       = pasta_saida,
  pattern    = paste0("^", shp_nome),
  full.names = TRUE
)

for (arq in arquivos_shp) {
  file.copy(from = arq, to = file.path(pasta_shp_div, basename(arq)), overwrite = TRUE)
}

# Copia também os geopackages
for (gpkg in c("idrgp_2025.gpkg", "idrgp_ciclo_2022_2025.gpkg")) {
  origem <- file.path(pasta_saida, gpkg)
  if (file.exists(origem)) {
    file.copy(from = origem, to = file.path(pasta_shp_div, gpkg), overwrite = TRUE)
  }
}

cat("Shapefiles copiados para", pasta_shp_div, "\n")
cat("  Arquivos copiados:", length(arquivos_shp), "componentes do SHP + 2 GeoPackages\n")

## 11.8.5 Tabelas individuais por subprefeitura (2022–2025) --------------------

# Objetivo: 1 planilha Excel por subprefeitura (32 ao total)
# Colunas: PROJETO / ATIVIDADE | VALOR LIQUIDADO
# Última linha: "Total Geral" com a soma
#
# Fonte: df_anexo_2022/2023/2024 (arquivos existentes) + df_anexo (2025 atual)

# Lê os anexos históricos com colunas padronizadas
df_anx_2022 <- read_excel("df_anexo_2022.xlsx") |>
  clean_names() |>
  mutate(ano = 2022L)

df_anx_2023 <- read_excel("df_anexo_2023.xlsx") |>
  clean_names() |>
  mutate(ano = 2023L)

df_anx_2024 <- read_excel("df_anexo_2024.xlsx") |>
  clean_names() |>
  mutate(ano = 2024L)

df_anx_2025 <- df_anexo |>
  mutate(ano = 2025L)

# Consolida todos os anos
df_anexo_ciclo <- bind_rows(df_anx_2022, df_anx_2023, df_anx_2024, df_anx_2025) |>
  # Padroniza nome da coluna de valor se diferente entre anos
  rename_with(~ ifelse(. == "valor_liquidado", "valor_liquidado", .), everything()) |>
  group_by(subprefeitura, projeto_atividade) |>
  summarise(
    valor_liquidado = sum(valor_liquidado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(subprefeitura, desc(valor_liquidado))

# Obtém lista de subprefeituras (ordenada)
lista_subpref <- sort(unique(df_anexo_ciclo$subprefeitura))

cat("\nGerando", length(lista_subpref), "tabelas individuais por subprefeitura...\n")

# Função auxiliar: formata valor como moeda brasileira
formata_reais <- function(x) {
  paste0("R$ ", format(round(x, 2), big.mark = ".", decimal.mark = ",", nsmall = 2))
}

# Gera uma planilha Excel por subprefeitura
for (sp in lista_subpref) {
  
  # Filtra dados da subprefeitura
  df_sp <- df_anexo_ciclo |>
    filter(subprefeitura == sp) |>
    select(`PROJETO / ATIVIDADE` = projeto_atividade,
           `VALOR LIQUIDADO (R$)` = valor_liquidado) |>
    mutate(`VALOR LIQUIDADO (R$)` = round(`VALOR LIQUIDADO (R$)`, 2))
  
  # Calcula o total geral
  total_geral <- sum(df_sp$`VALOR LIQUIDADO (R$)`, na.rm = TRUE)
  
  # Adiciona linha de total
  df_sp_final <- bind_rows(
    df_sp,
    tibble(
      `PROJETO / ATIVIDADE`  = "TOTAL GERAL",
      `VALOR LIQUIDADO (R$)` = total_geral
    )
  )
  
  # Cria nome de arquivo seguro (sem caracteres especiais no nome do arquivo)
  nome_arquivo <- sp |>
    str_to_lower() |>
    str_replace_all("[áàâã]", "a") |>
    str_replace_all("[éèê]",  "e") |>
    str_replace_all("[íìî]",  "i") |>
    str_replace_all("[óòôõ]", "o") |>
    str_replace_all("[úùû]",  "u") |>
    str_replace_all("ç",      "c") |>
    str_replace_all("[^a-z0-9]", "_") |>
    str_replace_all("_+", "_") |>
    str_trim("both")
  
  caminho_xlsx <- file.path(
    pasta_tabelas_div,
    paste0("tabela_", nome_arquivo, "_2022_2025.xlsx")
  )
  
  write_xlsx(df_sp_final, path = caminho_xlsx)
  
  cat("  ✔", sp, "—", nrow(df_sp), "projetos | Total:", formata_reais(total_geral), "\n")
}

cat("\n✅", length(lista_subpref), "tabelas exportadas para", pasta_tabelas_div, "\n")

## 11.8.6 Resumo da pasta de divulgação ----------------------------------------

n_png <- length(list.files(pasta_mapas_div,    pattern = "\\.png$")) +
  length(list.files(pasta_graficos_div,  pattern = "\\.png$"))
n_pdf <- length(list.files(pasta_mapas_div,    pattern = "\\.pdf$")) +
  length(list.files(pasta_graficos_div,  pattern = "\\.pdf$"))
n_shp_arq <- length(list.files(pasta_shp_div))
n_xlsx    <- length(list.files(pasta_tabelas_div, pattern = "\\.xlsx$"))

cat("\n=== PASTA DE DIVULGAÇÃO — RESUMO ===\n")
cat("  Localização:", normalizePath(pasta_divulgacao), "\n\n")
cat("  01_Mapas/:               ", n_png / 2, "mapas × 2 formatos (PNG + PDF)\n")
cat("  02_Graficos/:            ", n_pdf / 1, "gráficos × 2 formatos (PNG + PDF)\n")
cat("  03_Shapefiles/:          ", n_shp_arq, "arquivos (SHP + GPKG)\n")
cat("  04_Tabelas_Subprefeituras/:", n_xlsx, "planilhas (1 por subprefeitura)\n\n")



total_analisado_bi <- sum(df_integrado$valor, na.rm = TRUE) / 1e9
total_ciclo_bi     <- sum(df_ciclo$valor_2022_a_2025, na.rm = TRUE) / 1e9

cat("===========================================================\n")
cat("  IDRGP 2025 — Script executado com sucesso.\n")
cat("===========================================================\n\n")
cat("  Ano de referência:     ", ano_indice, "\n")
cat("  Ciclo analisado:        2022 a 2025\n")
cat("  Valor executado 2025:  R$", round(total_analisado_bi, 2), "bilhões\n")
cat("  Valor acumulado ciclo: R$", round(total_ciclo_bi, 2), "bilhões\n")
cat("  Subprefeituras:         32\n")
cat("  Método estatístico:    ", toupper(metodo_estatistico), "\n")
cat("  p-valor (teste):       ", formatC(teste_principal$p.value, format = "e", digits = 2), "\n")
cat("  IC 95% utilizado:      [",
    round(ic_inf, 6), ";", round(ic_sup, 6), "]\n")
cat("\n  Produtos exportados para:", normalizePath(pasta_saida), "\n")
cat("\n  Elaboração: SEPLAN/CPMA — Prefeitura de São Paulo.\n")
cat("===========================================================\n")

# Fim do script ---------------------------------------------------------------
