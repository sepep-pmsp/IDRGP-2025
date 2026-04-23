---
title: "IDRGP — Índice de Distribuição Regional do Gasto Público"
subtitle: "Análise do Ciclo PPA 2022–2025"
author: "SEPLAN/CPMA — Prefeitura de São Paulo"
date: "`r format(Sys.Date(), '%d/%m/%Y')`"
output:
  html_document:
    toc: true
    toc_float: true
    theme: flatly
    code_folding: show
    highlight: tango
    number_sections: true
---

# Introdução

O **IDRGP** (Índice de Distribuição Regional do Gasto Público) mede se a
distribuição territorial dos gastos públicos municipais está alinhada às
prioridades sociais e de infraestrutura de cada subprefeitura, expressas no
**Índice de Vulnerabilidade Urbana (IVU/SEPLAN/CPMA)**.

Este documento consolida a análise do ciclo PPA **2022–2025**, considerando
**todo o universo** de projetos/atividades e funções, sem seleção.

**Fontes de dados:**

- Dados regionalizados de despesas — Secretaria Municipal da Fazenda
- IDRGP Alvo (2024) — SGM/SEPLAN/CPMA (baseado no IVU)
- Dados cartográficos — GeoSampa (Subprefeituras de São Paulo)

**Referência:** <https://orcamento.sf.prefeitura.sp.gov.br/orcamento/execucao.php>

## Bibliotecas

```{r bibliotecas, message=FALSE, warning=FALSE}
library(tidyverse)   # manipulação e visualização de dados
library(readxl)      # leitura de arquivos Excel
library(janitor)     # padronização de nomes de colunas
library(scales)      # formatação de eixos em gráficos
library(writexl)     # exportação para Excel
library(sf)          # dados geoespaciais e mapas
library(ggplot2)     # gráficos
```

## Funções auxiliares

A função `norm_name()` cria uma **chave de integração robusta** entre as
diversas bases de dados. Remove acentos, converte para minúsculas e
elimina caracteres não alfabéticos, garantindo que nomes escritos de
formas ligeiramente diferentes sejam reconhecidos como iguais.

```{r funcao_norm_name}
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
```

## Tabela de referência das subprefeituras

Tabela oficial de correspondência **sigla → nome** das 32 subprefeituras.
Usada para padronizar nomes vindos de diferentes fontes (shapefile,
orçamento, IVU).

```{r df_siglas}
df_siglas <- tibble(
  sigla = c("AF","BT","CL","CS","CV","AD","CT","EM","FO","GU",
            "IP","IT","IQ","JA","JT","LA","MB","MO","PA","PE",
            "PR","PI","PJ","ST","SA","SM","MP","SB","SE","MG","VM","VP"),
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
```

## Caminhos dos arquivos

As bases orçamentárias são arquivos locais obtidos do portal da
Secretaria da Fazenda. Para atualizar, basta **substituir os arquivos**
na pasta `dados/` e ajustar os nomes abaixo.

```{r caminhos}
# ============================================================================
# CONFIGURAÇÃO DE ARQUIVOS — EDITE AQUI para atualizar as bases
# ============================================================================

# Bases orçamentárias (arquivos locais)
# Origem: https://orcamento.sf.prefeitura.sp.gov.br/orcamento/execucao.php
arquivo_2022 <- "dados/EXECUCAO_DA_2022_v2.xlsx"
arquivo_2023 <- "dados/basedadosDA_1223.xlsx"
arquivo_2024 <- "dados/basedadosDA_2024.xlsx"
arquivo_2025 <- "dados/basedadosDA_2025.xlsx"

# IDRGP Alvo (IVU 2024)
arquivo_ivu  <- "dados/ivu_subprefeituras_2025.xlsx"

# Shapefile das subprefeituras (fonte: GeoSampa)
arquivo_shp  <- "dados/subprefeitura_v2.shp"

# Pasta de saída
pasta_saida  <- "resultados"
if (!dir.exists(pasta_saida)) dir.create(pasta_saida)
```

# IDRGP Alvo

O **IDRGP Alvo** é o índice de referência que indica qual proporção dos
gastos públicos deveria ser destinada a cada subprefeitura. É calculado
com base no **IVU** (Índice de Vulnerabilidade Urbana), elaborado pela
SEPLAN/CPMA.

Neste relatório utiliza-se o **IDRGP Alvo calculado em 2024**.

## Importação do IVU

```{r idrgp_alvo}
df_idrgp <- read_excel(arquivo_ivu) |>
  clean_names() |>
  select(nome_subpref, ivu_geral) |>
  rename(subprefeitura = nome_subpref) |>
  mutate(
    ivu_geral     = as.numeric(ivu_geral),
    idrgp_alvo    = ivu_geral / sum(ivu_geral, na.rm = TRUE),
    subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
    subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    ""),
    # Corrige nomes divergentes
    subprefeitura = case_when(
      subprefeitura == "São Miguel"     ~ "São Miguel Paulista",
      subprefeitura == "Perus"          ~ "Perus/Anhanguera",
      subprefeitura == "Itaim"          ~ "Itaim Paulista",
      TRUE ~ subprefeitura
    )
  ) |>
  select(subprefeitura, idrgp_alvo) |>
  arrange(desc(idrgp_alvo))
```

## Controles de qualidade

```{r controle_idrgp}
# A soma do IDRGP Alvo deve ser igual a 1 (100%)
soma_idrgp <- sum(df_idrgp$idrgp_alvo, na.rm = TRUE)

if (abs(soma_idrgp - 1) > 0.001) {
  warning("ATENÇÃO: o IDRGP Alvo não soma 1 (soma = ", round(soma_idrgp, 6), ").")
} else {
  cat("✅ IDRGP Alvo verificado: soma =", round(soma_idrgp, 6), "\n")
}

cat("Subprefeituras no IDRGP:", nrow(df_idrgp), "(esperado: 32)\n")

# Adiciona join_key para integração
df_idrgp <- df_idrgp |>
  mutate(
    subprefeitura = str_squish(subprefeitura),
    join_key      = norm_name(subprefeitura)
  )

# Exibe a tabela
knitr::kable(
  df_idrgp |> select(subprefeitura, idrgp_alvo),
  col.names = c("Subprefeitura", "IDRGP Alvo"),
  digits = 4, caption = "IDRGP Alvo (baseado no IVU 2024)"
)
```

# Dados Orçamentários e Limpeza

Nesta seção, as bases orçamentárias dos **4 anos** do ciclo PPA são
importadas e padronizadas por uma **função de limpeza unificada**. Isso
garante consistência no tratamento e facilita a atualização futura.

## Função de limpeza unificada

```{r funcao_limpeza}
#' Importa e padroniza uma base orçamentária anual.
#'
#' @param arquivo Caminho do arquivo Excel.
#' @param ano     Ano de referência (inteiro).
#' @param sheet   Nome ou número da aba (default: 1).
#' @return Data frame limpo com colunas padronizadas.
limpar_orcamento <- function(arquivo, ano, sheet = 1) {

  # 1. Leitura e padronização de nomes
  df <- read_excel(arquivo, sheet = sheet) |> clean_names()

  # 2. Identifica coluna de valor (variações possíveis após clean_names)
  col_valor <- intersect(
    names(df),
    c("valor_detalhamento_acao", "valor_detalhamento_a_cao")
  )[1]
  if (is.na(col_valor)) stop("Coluna de valor não encontrada em ", arquivo)
  df <- df |> rename(valor_detalhamento_acao = !!sym(col_valor))

  # 3. Padronização de subprefeituras

  df <- df |>
    mutate(
      subprefeitura = as.character(subprefeitura),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura de ", ""),
      subprefeitura = str_replace_all(subprefeitura, "^Subprefeitura ",    ""),
      subprefeitura = case_when(
        subprefeitura %in% c("Supra Centro", "Supra Subprefeitura Centro") ~ "Sé",
        TRUE ~ subprefeitura
      )
    )

  # 4. Remove Supra Subprefeituras e NAs
  df <- df |>
    filter(
      !str_detect(replace_na(subprefeitura, ""), "Supra"),
      !is.na(subprefeitura)
    )

  # 5. Seleciona colunas essenciais e adiciona ano
  df <- df |>
    select(
      descricao_proj_ativ,
      subprefeitura,
      valor_detalhamento_acao
    ) |>
    mutate(ano = as.integer(ano))

  cat("✅ Ano", ano, ":", nrow(df), "linhas,",
      n_distinct(df$subprefeitura), "subprefeituras,",
      "R$", format(sum(df$valor_detalhamento_acao, na.rm = TRUE),
                    big.mark = ".", decimal.mark = ","), "\n")

  return(df)
}
```

## Processamento dos 4 anos

```{r processamento, message=FALSE, warning=FALSE}
# Detecta a aba correta para cada arquivo
detectar_sheet <- function(arquivo, ano) {
  sheets <- excel_sheets(arquivo)
  # Tenta encontrar aba com o ano no nome
  match <- str_detect(sheets, as.character(ano))
  if (any(match)) return(sheets[which(match)[1]])
  return(sheets[1])
}

# Processa cada ano
df_2022 <- limpar_orcamento(arquivo_2022, 2022, detectar_sheet(arquivo_2022, 2022))
df_2023 <- limpar_orcamento(arquivo_2023, 2023, detectar_sheet(arquivo_2023, 2023))
df_2024 <- limpar_orcamento(arquivo_2024, 2024, detectar_sheet(arquivo_2024, 2024))
df_2025 <- limpar_orcamento(arquivo_2025, 2025, detectar_sheet(arquivo_2025, 2025))

# Agrega por subprefeitura e ano
agregar_por_subpref <- function(df) {
  df |>
    group_by(subprefeitura) |>
    summarise(valor = sum(valor_detalhamento_acao, na.rm = TRUE), .groups = "drop")
}

df_agr_2022 <- agregar_por_subpref(df_2022)
df_agr_2023 <- agregar_por_subpref(df_2023)
df_agr_2024 <- agregar_por_subpref(df_2024)
df_agr_2025 <- agregar_por_subpref(df_2025)
```

## Verificação de qualidade

```{r verificacao_dados}
# Resumo por ano
resumo_anos <- tibble(
  ano = 2022:2025,
  n_linhas = c(nrow(df_2022), nrow(df_2023), nrow(df_2024), nrow(df_2025)),
  n_subpref = c(
    n_distinct(df_agr_2022$subprefeitura),
    n_distinct(df_agr_2023$subprefeitura),
    n_distinct(df_agr_2024$subprefeitura),
    n_distinct(df_agr_2025$subprefeitura)
  ),
  valor_total = c(
    sum(df_agr_2022$valor), sum(df_agr_2023$valor),
    sum(df_agr_2024$valor), sum(df_agr_2025$valor)
  )
)

knitr::kable(
  resumo_anos |>
    mutate(valor_total = scales::label_number(
      big.mark = ".", decimal.mark = ",", prefix = "R$ "
    )(valor_total)),
  col.names = c("Ano", "Linhas", "Subprefeituras", "Valor Total"),
  caption = "Resumo das bases orçamentárias processadas"
)

```

# Testes Estatísticos

**Metodologia:** O teste de **Kolmogorov-Smirnov** verifica a normalidade
das diferenças pareadas. Conforme o resultado, opta-se pelo **teste t pareado**
(paramétrico) ou **Wilcoxon** (não paramétrico). O intervalo de confiança
(95%) classifica cada subprefeitura.

## Integração dos dados (2025)

```{r integracao_2025}
# Dados cartográficos
subprefeitura_sf <- read_sf(arquivo_shp) |>
  clean_names() |> st_make_valid() |>
  rename(sigla = sg_subpref) |>
  left_join(df_siglas, by = "sigla") |>
  mutate(
    subprefeitura = coalesce(subprefeitura, nm_subpref),
    subprefeitura = str_squish(as.character(subprefeitura)),
    join_key      = norm_name(subprefeitura)
  ) |>
  select(sigla, subprefeitura, join_key, geometry)

# Prepara orçamento 2025
df_agr_2025_k <- df_agr_2025 |>
  mutate(subprefeitura = str_squish(subprefeitura),
         join_key = norm_name(subprefeitura))

# Integração
df_integrado <- subprefeitura_sf |>
  left_join(df_idrgp      |> select(join_key, idrgp_alvo), by = "join_key") |>
  left_join(df_agr_2025_k |> select(join_key, valor),      by = "join_key") |>
  mutate(valor = replace_na(valor, 0), idrgp_alvo = replace_na(idrgp_alvo, 0))

total_valor <- sum(df_integrado$valor, na.rm = TRUE)

df_integrado <- df_integrado |>
  mutate(
    idrgp_real      = if (total_valor > 0) valor / total_valor else NA_real_,
    idrgp_diferenca = idrgp_real - idrgp_alvo,
    idrgp_var_percentual = if_else(idrgp_alvo == 0, 0, idrgp_diferenca / idrgp_alvo),
    valor_previsto  = total_valor * idrgp_alvo,
    status_valor    = if_else(valor < valor_previsto,
                              "Valor previsto (R$) não atingido",
                              "Valor previsto (R$) atingido")
  )

cat("✅ Integração 2025: R$", format(total_valor, big.mark = ".", decimal.mark = ","), "\n")
```

## Teste KS e seleção do método

```{r teste_ks}
df_teste <- df_integrado |>
  st_drop_geometry() |>
  select(subprefeitura, valor, idrgp_alvo, idrgp_real,
         idrgp_diferenca, idrgp_var_percentual) |>
  filter(!is.na(idrgp_diferenca))

vetor_dif_z <- scale(df_teste$idrgp_diferenca)[, 1]
teste_ks    <- ks.test(vetor_dif_z, "pnorm")

cat("--- Teste KS (normalidade) ---\n")
cat("D =", round(teste_ks$statistic, 4), "| p =",
    formatC(teste_ks$p.value, format = "e", digits = 3), "\n\n")

if (teste_ks$p.value < 0.05) {
  metodo_estatistico <- "wilcoxon"
  teste_principal <- wilcox.test(
    df_teste$idrgp_real, df_teste$idrgp_alvo,
    paired = TRUE, conf.int = TRUE, conf.level = 0.95, exact = FALSE)
  medida_central <- median(df_teste$idrgp_diferenca, na.rm = TRUE)
  mensagem_metodo <- paste0("Distribuição não normal (KS p = ",
    formatC(teste_ks$p.value, format = "e", digits = 3),
    "). Método: Wilcoxon pareado.")
} else {
  metodo_estatistico <- "t_pareado"
  teste_principal <- t.test(
    df_teste$idrgp_real, df_teste$idrgp_alvo,
    paired = TRUE, conf.level = 0.95)
  medida_central <- unname(teste_principal$estimate)
  mensagem_metodo <- paste0("Distribuição normal (KS p = ",
    formatC(teste_ks$p.value, format = "f", digits = 3),
    "). Método: teste t pareado.")
}

ic_inf <- teste_principal$conf.int[1]
ic_sup <- teste_principal$conf.int[2]

cat(mensagem_metodo, "\n")
cat("IC 95%: [", round(ic_inf, 6), ";", round(ic_sup, 6), "]\n")
```

## Classificação das subprefeituras

```{r classificacao}
df_teste <- df_teste |>
  mutate(
    status_var_percentual = case_when(
      idrgp_var_percentual >  0.3  ~ "Acima do IDRGP Alvo",
      idrgp_var_percentual < -0.3  ~ "Abaixo do IDRGP Alvo",
      TRUE                         ~ "Dentro do IDRGP Alvo"),
    status_estatistico = case_when(
      idrgp_diferenca < ic_inf ~ "Abaixo do IDRGP Alvo",
      idrgp_diferenca > ic_sup ~ "Acima do IDRGP Alvo",
      TRUE                     ~ "Dentro do IDRGP Alvo")
  )

df_integrado <- df_integrado |>
  select(-any_of(c("status_var_percentual", "status_estatistico"))) |>
  left_join(df_teste |> select(subprefeitura, status_var_percentual, status_estatistico),
            by = "subprefeitura")

cat("Classificação por variação percentual:\n")
print(df_teste |> count(status_var_percentual))
cat("\nClassificação estatística:\n")
print(df_teste |> count(status_estatistico))
```

## Consolidação histórica (2022–2025)

```{r historico}
integrar_ano <- function(df_agr, ano_ref) {
  df_agr |>
    mutate(join_key = norm_name(subprefeitura)) |>
    left_join(df_idrgp |> select(join_key, idrgp_alvo), by = "join_key") |>
    mutate(
      valor = replace_na(valor, 0), idrgp_alvo = replace_na(idrgp_alvo, 0),
      total = sum(valor, na.rm = TRUE),
      idrgp_real = if_else(total > 0, valor / total, 0),
      idrgp_diferenca = idrgp_real - idrgp_alvo,
      idrgp_var_percentual = if_else(idrgp_alvo == 0, 0, idrgp_diferenca / idrgp_alvo),
      status = case_when(
        idrgp_var_percentual >  0.3  ~ "Acima do IDRGP Alvo",
        idrgp_var_percentual < -0.3  ~ "Abaixo do IDRGP Alvo",
        TRUE ~ "Dentro do IDRGP Alvo"),
      ano = ano_ref
    ) |> select(-total)
}

int_2022 <- integrar_ano(df_agr_2022, 2022)
int_2023 <- integrar_ano(df_agr_2023, 2023)
int_2024 <- integrar_ano(df_agr_2024, 2024)
int_2025 <- df_integrado |> st_drop_geometry() |>
  select(subprefeitura, join_key, valor, idrgp_alvo, idrgp_real,
         idrgp_diferenca, idrgp_var_percentual) |>
  mutate(status = coalesce(
    df_teste$status_estatistico[match(subprefeitura, df_teste$subprefeitura)],
    "Dentro do IDRGP Alvo"), ano = 2025)

df_longo <- bind_rows(int_2022, int_2023, int_2024, int_2025) |>
  mutate(ano = as.factor(ano))

df_ciclo <- df_longo |>
  group_by(subprefeitura) |>
  summarise(
    idrgp_alvo = first(idrgp_alvo),
    valor_2022 = sum(valor[ano == "2022"], na.rm = TRUE),
    valor_2023 = sum(valor[ano == "2023"], na.rm = TRUE),
    valor_2024 = sum(valor[ano == "2024"], na.rm = TRUE),
    valor_2025 = sum(valor[ano == "2025"], na.rm = TRUE),
    .groups = "drop") |>
  mutate(
    valor_2022_a_2025 = valor_2022 + valor_2023 + valor_2024 + valor_2025,
    idrgp_real_2022_a_2025 = valor_2022_a_2025 / sum(valor_2022_a_2025),
    idrgp_var_pct_ciclo = if_else(idrgp_alvo == 0, 0,
      (idrgp_real_2022_a_2025 - idrgp_alvo) / idrgp_alvo),
    status_ciclo = case_when(
      idrgp_var_pct_ciclo >  0.3  ~ "Acima do IDRGP Alvo",
      idrgp_var_pct_ciclo < -0.3  ~ "Abaixo do IDRGP Alvo",
      TRUE ~ "Dentro do IDRGP Alvo")
  )

df_ciclo_sf <- subprefeitura_sf |>
  left_join(df_ciclo |> mutate(join_key = norm_name(subprefeitura)), by = "join_key")

cat("✅ Consolidação histórica concluída.\n")
```

# Mapas

```{r config_mapas}
cores_status <- c(
  "Abaixo do IDRGP Alvo" = "#d97a0f",
  "Dentro do IDRGP Alvo" = "#0ea1cf",
  "Acima do IDRGP Alvo"  = "#1A1442")

tema_mapa <- theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    plot.caption  = element_text(size = 7, hjust = 0),
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.margin   = margin(10, 10, 10, 10),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    legend.text   = element_text(size = 8))

caption_base <- "Elaboração: SEPLAN/CPMA.\nFonte: Secretaria Municipal da Fazenda"

gerar_mapa <- function(sf_data, col_status, titulo, nota = "") {
  ggplot(sf_data) +
    geom_sf(aes(fill = .data[[col_status]]), color = "white", linewidth = 0.3) +
    geom_sf_text(aes(label = sigla), color = "white", size = 2.2, fontface = "bold") +
    scale_fill_manual(values = cores_status, name = "Status IDRGP Real", na.value = "grey80") +
    labs(title = titulo,
         subtitle = "Classificação de cada subprefeitura em relação ao IDRGP Alvo",
         caption = paste0(caption_base, "\n", nota)) +
    tema_mapa
}
```

## Mapas 2022–2024

```{r mapas_hist, fig.width=12, fig.height=10}
preparar_sf <- function(int_data) {
  subprefeitura_sf |>
    left_join(int_data |> select(join_key, status), by = "join_key")
}

mapa1 <- gerar_mapa(preparar_sf(int_2022), "status",
  "IDRGP 2022 — Status quanto ao IDRGP Alvo", "Variação percentual (±30%).")
mapa2 <- gerar_mapa(preparar_sf(int_2023), "status",
  "IDRGP 2023 — Status quanto ao IDRGP Alvo", "Variação percentual (±30%).")
mapa3 <- gerar_mapa(preparar_sf(int_2024), "status",
  "IDRGP 2024 — Status quanto ao IDRGP Alvo", "Variação percentual (±30%).")

print(mapa1); print(mapa2); print(mapa3)
```

## Mapa 2025

```{r mapa_2025, fig.width=12, fig.height=10}
mapa4 <- gerar_mapa(df_integrado, "status_estatistico",
  "IDRGP 2025 — Status quanto ao IDRGP Alvo",
  paste0("IC do teste ", toupper(metodo_estatistico),
         " (95%): [", round(ic_inf, 5), "; ", round(ic_sup, 5), "]."))
print(mapa4)
```

## Mapa Agregado 2022–2025

```{r mapa_ciclo, fig.width=12, fig.height=10}
mapa5 <- gerar_mapa(df_ciclo_sf, "status_ciclo",
  "IDRGP Agregado 2022–2025 — Balanço do Ciclo PPA",
  "Variação percentual (±30%) do IDRGP Real acumulado.")
print(mapa5)
cat("✅ 5 mapas gerados.\n")
```

# Gráficos

```{r tema_graficos}
tema_barras <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot", plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 7, hjust = 0),
    axis.text.y   = element_text(size = 8))
```

## G1 — IDRGP 2025: Alvo vs. Real (%)

```{r grafico1, fig.width=14, fig.height=12}
grafico1 <- ggplot(df_integrado, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(aes(x = idrgp_real, fill = status_estatistico), width = 0.65, alpha = 0.9) +
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), color = "#c0392b", size = 3) +
  scale_fill_manual(values = cores_status, name = "Status:") +
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(title = "IDRGP 2025 — Distribuição Percentual do Gasto por Subprefeitura",
       subtitle = "IDRGP Real (barras) e IDRGP Alvo (losango)",
       x = "IDRGP (%)", y = NULL,
       caption = paste0(caption_base, "\nTeste: ", toupper(metodo_estatistico),
                        " | p = ", formatC(teste_principal$p.value, format = "e", digits = 2))) +
  tema_barras

print(grafico1)
```

## G2 — IDRGP Agregado 2022–2025 (%)

```{r grafico2, fig.width=14, fig.height=12}
grafico2 <- ggplot(df_ciclo, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(aes(x = idrgp_real_2022_a_2025, fill = status_ciclo), width = 0.65, alpha = 0.9) +
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), color = "#c0392b", size = 3) +
  scale_fill_manual(values = cores_status, name = "Status:", limits = names(cores_status)) +
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(title = "IDRGP 2022–2025 — Balanço do Ciclo PPA (Distribuição Percentual)",
       subtitle = "IDRGP Real acumulado (barras) e IDRGP Alvo (losango)",
       x = "IDRGP Acumulado (%)", y = NULL,
       caption = paste0(caption_base, "\nVariação percentual (±30%).")) +
  tema_barras

print(grafico2)
```

## G3 — IDRGP 2025: Valores Absolutos (R$)

```{r grafico3, fig.width=14, fig.height=12}
grafico3 <- ggplot(df_integrado, aes(y = reorder(subprefeitura, idrgp_alvo))) +
  geom_col(aes(x = valor, fill = status_valor), width = 0.65, alpha = 0.9) +
  geom_point(aes(x = valor_previsto, shape = "Valor Previsto (R$)"),
             color = "#c0392b", size = 3) +
  scale_fill_manual(values = c(
    "Valor previsto (R$) atingido"     = "#1A1442",
    "Valor previsto (R$) não atingido" = "#d97a0f"), name = "Status:") +
  scale_shape_manual(values = c("Valor Previsto (R$)" = 18), name = "") +
  guides(fill = guide_legend(order = 1),
         shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)) +
  scale_x_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ",")) +
  labs(title = "IDRGP 2025 — Valor Previsto e Executado por Subprefeitura (R$)",
       x = "Valor executado (R$)", y = NULL, caption = caption_base) +
  tema_barras

print(grafico3)
```

## G4 — IDRGP 2022–2025: Série Histórica (R$)

```{r grafico4, fig.width=14, fig.height=12}
df_longo_plot <- df_longo |>
  mutate(
    subprefeitura = factor(subprefeitura,
      levels = df_ciclo |> arrange(idrgp_alvo) |> pull(subprefeitura)),
    ano = factor(ano, levels = c("2022","2023","2024","2025")))

valor_previsto_ciclo <- sum(df_ciclo$valor_2022_a_2025)

grafico4 <- ggplot(df_longo_plot, aes(y = subprefeitura, x = valor, fill = ano)) +
  geom_col(width = 0.65, alpha = 0.9, position = "stack") +
  geom_point(
    data = df_ciclo |>
      mutate(
        subprefeitura = factor(subprefeitura, levels = levels(df_longo_plot$subprefeitura)),
        valor_prev = idrgp_alvo * valor_previsto_ciclo),
    aes(y = subprefeitura, x = valor_prev, shape = "Valor Previsto Ciclo"),
    shape = 23, fill = "white", color = "#c0392b", stroke = 1.1, size = 3.2,
    inherit.aes = FALSE) +
  scale_fill_manual(values = c(
    "2022" = "#9BD7EA", "2023" = "#4CB7D8",
    "2024" = "#1F78B4", "2025" = "#0B3C6D"), name = "Ano") +
  scale_x_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ",")) +
  labs(title = "IDRGP 2022–2025 — Evolução do Gasto Real por Subprefeitura (R$)",
       subtitle = "Barras empilhadas por ano | Losango: valor previsto total do ciclo",
       x = "Valor executado acumulado (R$)", y = NULL, caption = caption_base) +
  tema_barras

print(grafico4)
cat("✅ 4 gráficos gerados.\n")
```

# Exportação e Anexos

## Tabela de anexos (2025)

Detalhamento do valor liquidado por **projeto/atividade** em cada
subprefeitura — apenas para o ano de **2025**.

```{r anexos}
df_anexo <- df_2025 |>
  group_by(subprefeitura, descricao_proj_ativ) |>
  summarise(valor_liquidado = sum(valor_detalhamento_acao, na.rm = TRUE), .groups = "drop") |>
  rename(projeto_atividade = descricao_proj_ativ) |>
  arrange(subprefeitura, projeto_atividade)

cat("Subprefeituras em df_anexo:", n_distinct(df_anexo$subprefeitura), "\n")
cat("Valor total:", format(sum(df_anexo$valor_liquidado), big.mark = ".", decimal.mark = ","), "\n")
```

## Exportação de mapas e gráficos

```{r exporta_png}
# Mapas (300 dpi)
ggsave(file.path(pasta_saida, "mapa1_idrgp_2022.png"), mapa1, width=12, height=10, dpi=300)
ggsave(file.path(pasta_saida, "mapa2_idrgp_2023.png"), mapa2, width=12, height=10, dpi=300)
ggsave(file.path(pasta_saida, "mapa3_idrgp_2024.png"), mapa3, width=12, height=10, dpi=300)
ggsave(file.path(pasta_saida, "mapa4_idrgp_2025.png"), mapa4, width=12, height=10, dpi=300)
ggsave(file.path(pasta_saida, "mapa5_idrgp_agregado_2022_2025.png"), mapa5, width=12, height=10, dpi=300)

# Gráficos (300 dpi)
ggsave(file.path(pasta_saida, "grafico1_idrgp_2025_pct.png"), grafico1, width=14, height=12, dpi=300)
ggsave(file.path(pasta_saida, "grafico2_idrgp_agregado_pct.png"), grafico2, width=14, height=12, dpi=300)
ggsave(file.path(pasta_saida, "grafico3_idrgp_2025_reais.png"), grafico3, width=14, height=12, dpi=300)
ggsave(file.path(pasta_saida, "grafico4_serie_2022_2025.png"), grafico4, width=14, height=12, dpi=300)

cat("✅ Mapas e gráficos exportados.\n")
```

## Exportação de dados em Excel

```{r exporta_xlsx}
write_xlsx(
  df_integrado |> st_drop_geometry() |> select(-any_of("join_key")),
  path = file.path(pasta_saida, "df_integrado_2025.xlsx"))

write_xlsx(df_ciclo, path = file.path(pasta_saida, "df_ciclo_2022_2025.xlsx"))

write_xlsx(
  df_longo |> select(-any_of("geometry")),
  path = file.path(pasta_saida, "df_longo_2022_2025.xlsx"))

write_xlsx(df_anexo, path = file.path(pasta_saida, "df_anexo_2025.xlsx"))

write_xlsx(
  tibble(
    metodo = metodo_estatistico,
    p_valor = teste_principal$p.value,
    estimativa_central = medida_central,
    ic_inferior_95pct = ic_inf,
    ic_superior_95pct = ic_sup,
    ks_D = unname(teste_ks$statistic),
    ks_p = teste_ks$p.value,
    n = nrow(df_teste)),
  path = file.path(pasta_saida, "tabela_teste_estatistico_2025.xlsx"))

cat("✅ Planilhas exportadas.\n")
```

## Exportação geoespacial

```{r exporta_geo}
# GeoPackage — IDRGP 2025
gpkg_2025 <- file.path(pasta_saida, "idrgp_2025.gpkg")
if (file.exists(gpkg_2025)) file.remove(gpkg_2025)
st_write(df_integrado |> select(-any_of("join_key")),
         dsn = gpkg_2025, layer = "idrgp_2025", delete_dsn = FALSE)

# GeoPackage — Ciclo 2022–2025
gpkg_ciclo <- file.path(pasta_saida, "idrgp_ciclo_2022_2025.gpkg")
if (file.exists(gpkg_ciclo)) file.remove(gpkg_ciclo)
st_write(df_ciclo_sf |> select(-any_of(c("join_key", "subprefeitura.y"))),
         dsn = gpkg_ciclo, layer = "idrgp_ciclo", delete_dsn = FALSE)

# Shapefile
df_shp <- df_integrado |>
  rename(sp_nome = subprefeitura, id_alvo = idrgp_alvo, id_real = idrgp_real,
         dif_idr = idrgp_diferenca, var_pct = idrgp_var_percentual,
         vlr_orc = valor, vlr_prev = valor_previsto,
         st_vlr = status_valor, st_var = status_var_percentual,
         st_est = status_estatistico) |>
  select(sp_nome, sigla, vlr_orc, id_alvo, id_real, dif_idr, var_pct,
         st_vlr, st_var, st_est, geometry)

st_write(df_shp, dsn = pasta_saida,
         layer = paste0("idrgp_2025_", format(Sys.Date(), "%Y%m%d")),
         driver = "ESRI Shapefile", delete_layer = TRUE, append = FALSE)

cat("✅ Arquivos geoespaciais exportados.\n")
```

## Conclusão

```{r conclusao}
cat("===========================================================\n")
cat("  IDRGP 2022–2025 — Relatório concluído com sucesso.\n")
cat("===========================================================\n\n")
cat("  Ciclo analisado:      2022 a 2025\n")
cat("  Valor 2025:           R$", format(total_valor, big.mark = ".", decimal.mark = ","), "\n")
cat("  Total ciclo:          R$", format(sum(df_ciclo$valor_2022_a_2025),
                                          big.mark = ".", decimal.mark = ","), "\n")
cat("  Subprefeituras:       32\n")
cat("  Método estatístico:  ", toupper(metodo_estatistico), "\n")
cat("  p-valor:             ", formatC(teste_principal$p.value, format = "e", digits = 2), "\n")
cat("  IC 95%:              [", round(ic_inf, 6), ";", round(ic_sup, 6), "]\n\n")
cat("  Produtos em:", pasta_saida, "\n")
cat("  Elaboração: SEPLAN/CPMA — Prefeitura de São Paulo.\n")
cat("===========================================================\n")
```

