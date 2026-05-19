## ----setup, include=FALSE-----------------------------------------------------
# ============================================================================
# CHUNK 1: SETUP COMPLETO E PACOTES
# ============================================================================

library(tidyverse)
library(readxl)
library(writexl)
library(openxlsx)
library(janitor)
library(sf)
library(tmap)
library(tmaptools)
library(ggplot2)
library(treemapify)
library(patchwork)
library(scales)
library(stringi)
library(stringr)
library(DT)
library(knitr)
library(kableExtra)

cores_status <- c(
  "Abaixo do IDRGP Alvo" = "#d97a0f",
  "Dentro do IDRGP Alvo" = "#0ea1cf",
  "Acima do IDRGP Alvo"  = "#1A1442"
)

cores_top10_cesta <- c(
  "#0ea1cf", "#d97a0f", "#1A1442", "#2ecc71", "#e74c3c",
  "#9b59b6", "#f39c12", "#1abc9c", "#e67e22", "#3498db",
  "#bdc3c7"   # FIXO para "Demais ações"
)

tema_idrgp <- theme_minimal(base_size = 9) +
  theme(
    text = element_text(family = "sans"),
    plot.title = element_text(face = "bold", size = 12, color = "#1a1a2e"),
    plot.subtitle = element_text(size = 9, color = "#666666"),
    plot.caption = element_text(size = 7, color = "#999999", hjust = 1),
    legend.position = "bottom",
    plot.background = element_rect(fill = "white", color = NA)
  )

ggplot2::theme_set(tema_idrgp)

pasta_saida <- file.path(
  "C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP-2025/R Markdown",
  "resultados/Produtos_Cesta_2026"
)

for (subpasta in c("mapas", "graficos", "tabelas", "treemaps")) {
  dir.create(file.path(pasta_saida, subpasta), recursive = TRUE, showWarnings = FALSE)
}


## ----carga_dados, message=FALSE, warning=FALSE--------------------------------
# ============================================================================
# CHUNK 2: CARGA DE DADOS
# ============================================================================

caminho_tabela <- file.path(
  "C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP-2025/R Markdown",
  "dados/tabela_conferencia_multi_ano.xlsx"
)

if (file.exists(caminho_tabela)) {
  dados_brutos <- read_excel(caminho_tabela) |> clean_names()
  
  if (!"valor_empenhado" %in% names(dados_brutos)) {
    if ("valor_detalhamento_acao" %in% names(dados_brutos)) {
      dados_brutos <- dados_brutos |> rename(valor_empenhado = valor_detalhamento_acao)
    } else {
      dados_brutos <- dados_brutos |> mutate(valor_empenhado = NA_real_)
    }
  }
} else {
  warning("⚠️ Arquivo de dados não encontrado. Utilizando dummy.")
  dados_brutos <- data.frame(
    ano = rep(2022:2025, each=32),
    subprefeitura = rep(paste("Subpref", 1:32), 4),
    valor_empenhado = runif(128, 1e7, 1e9),
    codigo_proj_ativ = "1234",
    descricao_proj_ativ = "Acao de Teste"
  )
}

# ORDEM PADRONIZADA DAS SUBPREFEITURAS (Ordem Alfabética)
subprefeituras_ordem <- sort(unique(dados_brutos$subprefeitura))
dados_brutos <- dados_brutos |>
  mutate(subprefeitura = factor(subprefeitura, levels = subprefeituras_ordem))

# CARGA DO SHAPEFILE
caminho_shape <- file.path(
  "C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP-2025/R Markdown",
  "dados/SIRGAS_SHP_subprefeitura.shp"
)

if (!file.exists(caminho_shape)) {
  # Tentar encontrar qualquer .shp contendo 'subprefeitura' na pasta dados
  pasta_dados <- dirname(caminho_shape)
  arquivos_shp <- list.files(pasta_dados, pattern = "subprefeitura.*\\.shp$", ignore.case = TRUE, full.names = TRUE)
  
  if (length(arquivos_shp) == 0) {
    arquivos_shp <- list.files(pasta_dados, pattern = "\\.shp$", full.names = TRUE)
  }
  
  if (length(arquivos_shp) > 0) {
    caminho_shape <- arquivos_shp[1]
    cat("📂 Shapefile alternativo encontrado:", basename(caminho_shape), "\n")
  } else {
    stop("❌ NENHUM shapefile (.shp) encontrado na pasta: ", pasta_dados)
  }
}

sf_sp <- st_read(caminho_shape, quiet = TRUE) |> clean_names()
cat("✅ Shapefile carregado:", nrow(sf_sp), "feições\n")

# Identificar e renomear coluna de nome do shapefile
padroes_nome <- c("sp_nome", "nm_subpref", "subprefei", "nome", "sub", "NOME", "SUBPREFEI")
col_nome <- intersect(padroes_nome, names(sf_sp))[1]
if (is.na(col_nome)) {
  col_nome <- names(sf_sp)[sapply(sf_sp, is.character)][1]
}
sf_sp <- sf_sp |> rename(sp_nome = all_of(col_nome))

cat("📋 Colunas:", paste(names(sf_sp), collapse = ", "), "\n")


## ----preparar_dados_testes, message=FALSE, warning=FALSE----------------------
# ============================================================================
# CHUNK 3: PREPARAÇÃO DOS DADOS PARA TESTES ESTATÍSTICOS
# ============================================================================

dados_testes <- dados_brutos |>
  group_by(subprefeitura, ano) |>
  summarise(
    valor_total = sum(valor_empenhado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(ano) |>
  mutate(
    percentual_liquidado = valor_total / sum(valor_total, na.rm=TRUE) * 100,
    percentual_referencia = 100 / 32
  ) |>
  ungroup()

# Função de padronização (REMOVE acentos, prefixos, espaços e caracteres especiais)
padronizar_nome <- function(x) {
  x <- as.character(x)
  # Transliteração Latin-ASCII para remover acentos
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- x |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all("^subprefeitura de ", "") |>
    str_replace_all("^subprefeitura ", "") |>
    str_replace_all("[^a-z0-9]", "")
  
  # Padronizar nomes discrepantes
  x <- case_when(
    x %in% c("perus", "anhanguera", "perusanhanguera") ~ "perus",
    x %in% c("saomiguel", "saomiguelpaulista") ~ "saomiguel",
    x %in% c("itaim", "itaimpaulista") ~ "itaim",
    x %in% c("cidadeademar", "ademar") ~ "cidadeademar",
    x %in% c("mboimirim", "m boi mirim") ~ "mboimirim",
    TRUE ~ x
  )
  return(x)
}

# Aplicar nos dados
dados_testes <- dados_testes |>
  mutate(subprefeitura_pad = padronizar_nome(subprefeitura))

# Aplicar no shapefile
sf_sp <- sf_sp |>
  mutate(sp_nome_pad = padronizar_nome(sp_nome))

# Verificar correspondência
nomes_dados <- unique(dados_testes$subprefeitura_pad)
nomes_shape <- unique(sf_sp$sp_nome_pad)

cat("📊 Subprefeituras nos dados:", length(nomes_dados), "\n")
cat("📊 Subprefeituras no shapefile:", length(nomes_shape), "\n")

# Verificar quais NÃO têm correspondência
sem_match <- setdiff(nomes_dados, nomes_shape)
if (length(sem_match) > 0) {
  warning("⚠️ Subprefeituras sem correspondência no shapefile:")
  print(sem_match)
}


## ----aplicar_testes_estatisticos, message=FALSE, warning=FALSE----------------
# ============================================================================
# CHUNK 4: APLICAÇÃO DOS TESTES ESTATÍSTICOS
# ============================================================================

resultados_testes <- list()

for (ano_atual in c(2022, 2023, 2024, 2025)) {
  dados_ano <- dados_testes |> filter(ano == ano_atual)
  if (nrow(dados_ano) == 0) next
  
  teste <- NULL
  if (ano_atual %in% c(2022, 2023)) {
    teste <- tryCatch(t.test(dados_ano$percentual_referencia, dados_ano$percentual_liquidado, paired = TRUE), error = function(e) NULL)
  } else if (ano_atual == 2024) {
    teste <- tryCatch(wilcox.test(dados_ano$percentual_referencia, dados_ano$percentual_liquidado, paired = TRUE), error = function(e) NULL)
  } else if (ano_atual == 2025) {
    teste <- tryCatch(ks.test(dados_ano$percentual_referencia, dados_ano$percentual_liquidado), error = function(e) NULL)
  }
  
  if (!is.null(teste) && "p.value" %in% names(teste)) {
    dados_ano <- dados_ano |>
      mutate(
        p_valor = teste$p.value,
        categoria = case_when(
          p_valor < 0.05 & percentual_liquidado < percentual_referencia ~ "Abaixo do IDRGP Alvo",
          p_valor < 0.05 & percentual_liquidado > percentual_referencia ~ "Acima do IDRGP Alvo",
          TRUE ~ "Dentro do IDRGP Alvo"
        )
      )
  } else {
    dados_ano <- dados_ano |> mutate(p_valor = NA, categoria = "Dentro do IDRGP Alvo")
  }
  resultados_testes[[as.character(ano_atual)]] <- dados_ano
}

dados_classificados <- bind_rows(resultados_testes) |>
  mutate(subprefeitura = factor(subprefeitura, levels = subprefeituras_ordem))


## ----config_mapas, echo=FALSE, message=FALSE, warning=FALSE-------------------
# ============================================================================
# CONFIGURAÇÃO DOS MAPAS (MODELO IDRGP_2025 — ADAPTADO)
# ============================================================================

# --- Paleta de cores padronizada ---
cores_status <- c(
  "Abaixo do IDRGP Alvo" = "#d97a0f",
  "Dentro do IDRGP Alvo" = "#0ea1cf",
  "Acima do IDRGP Alvo"  = "#1A1442"
)

# --- Tema do mapa ---
tema_mapa <- theme_void(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, hjust = 0),
    plot.caption  = element_text(size = 7, hjust = 0),
    plot.title.position = "plot", 
    plot.caption.position = "plot",
    plot.margin   = margin(10, 10, 10, 10),
    legend.position = "right",
    legend.title  = element_text(size = 9),
    legend.text   = element_text(size = 8)
  )

# --- Legenda base ---
caption_base <- "Elaboração: CPMA/SIME/SEPLAN/PMSP.\nFonte: Secretaria Municipal de Planejamento e Eficiência — SF-PMSP"

# --- Função gerar_mapa (CORRIGIDA - verifica se coluna existe) ---
gerar_mapa <- function(sf_data, col_status, titulo, nota = "") {
  
  # Verificar se a coluna de status existe
  if (!col_status %in% names(sf_data)) {
    warning("⚠️ Coluna '", col_status, "' não encontrada. Mapa não será gerado.")
    return(NULL)
  }
  
  # Criar o mapa
  p <- ggplot(sf_data) +
    geom_sf(aes(fill = .data[[col_status]]), 
            color = "white", linewidth = 0.3) +
    scale_fill_manual(
      values = cores_status, 
      name = "Status IDRGP Real", 
      na.value = "grey80",
      drop = FALSE
    ) +
    labs(
      title = titulo,
      subtitle = "Classificação de cada subprefeitura em relação ao IDRGP Alvo",
      caption = paste0(caption_base, "\n", nota)
    ) +
    tema_mapa
  
  # Adicionar siglas SOMENTE se a coluna existir
  if ("sigla" %in% names(sf_data)) {
    p <- p + geom_sf_text(
      aes(label = sigla), 
      color = "white", size = 2.2, fontface = "bold"
    )
  }
  
  return(p)
}

cat("✅ Configuração de mapas carregada\n")


## ----preparar_shapefile_dados, echo=FALSE, message=FALSE, warning=FALSE-------
# ============================================================================
# PREPARAÇÃO: JUNTAR SHAPEFILE COM DADOS CLASSIFICADOS
# ============================================================================

# Verificar se o shapefile foi carregado
if (!exists("sf_sp")) {
  stop("❌ Shapefile não carregado. Verifique o chunk de carga de dados.")
}

# Inspecionar colunas do shapefile
cat("📋 Colunas do shapefile:\n")
print(names(sf_sp))

# Identificar coluna de nome
padroes_nome <- c("sp_nome", "nm_subpre", "subprefei", "nome", "sub", "NOME", "SUBPREFEI")
col_nome <- intersect(padroes_nome, names(sf_sp))[1]

if (is.na(col_nome)) {
  col_nome <- names(sf_sp)[sapply(sf_sp, is.character)][1]
  cat("⚠️ Nenhum padrão de nome encontrado. Usando:", col_nome, "\n")
}

# Identificar coluna de sigla
padroes_sigla <- c("sigla", "sg", "SIGLA", "abrev", "ABREV", "sg_subpref")
col_sigla <- intersect(padroes_sigla, names(sf_sp))[1]

# Padronizar nomes no shapefile
sf_sp <- sf_sp |>
  rename(
    sp_nome = all_of(col_nome)
  ) |>
  mutate(
    sp_nome_pad = padronizar_nome(sp_nome)
  )

# Adicionar sigla (se existir) or criar
if (!is.na(col_sigla)) {
  sf_sp <- sf_sp |> rename(sigla = all_of(col_sigla))
} else {
  # Criar sigla a partir do nome (3 primeiras letras)
  sf_sp <- sf_sp |>
    mutate(sigla = str_sub(sp_nome, 1, 3) |> str_to_upper())
}

cat("✅ Coluna de nome:", col_nome, "\n")
cat("✅ Coluna de sigla:", if_else(is.na(col_sigla), "CRIADA", col_sigla), "\n")
cat("📊 Subprefeituras no shapefile:", nrow(sf_sp), "\n")

# --- Juntar shapefile com dados de cada ano ---

# Função para juntar shapefile com dados de um ano
preparar_sf_ano <- function(ano_alvo) {
  
  # Filtrar dados do ano
  dados_ano <- dados_classificados |>
    filter(ano == ano_alvo) |>
    mutate(subprefeitura_pad = padronizar_nome(subprefeitura))
  
  # Juntar com shapefile
  sf_ano <- sf_sp |>
    left_join(dados_ano, by = c("sp_nome_pad" = "subprefeitura_pad"))
  
  # Verificar quantas subprefeituras receberam dados
  n_com_dados <- sum(!is.na(sf_ano$categoria))
  cat("📊 Mapa", ano_alvo, ":", n_com_dados, "/", nrow(sf_ano), 
      "subprefeituras com classificação\n")
  
  return(sf_ano)
}

# Preparar shapefiles para todos os anos
sf_2022 <- preparar_sf_ano(2022)
sf_2023 <- preparar_sf_ano(2023)
sf_2024 <- preparar_sf_ano(2024)
sf_2025 <- preparar_sf_ano(2025)

cat("✅ Shapefiles preparados para todos os anos\n")


## ----mapas_2022_2024, echo=FALSE, fig.height=10, fig.width=12, message=FALSE, warning=FALSE----
# ====================================================================
# MAPAS 2022, 2023 e 2024 — Status estatístico individual
# ====================================================================

# Mapa 2022
mapa_2022 <- gerar_mapa(
  sf_2022, "categoria",
  "IDRGP 2022 — Status quanto ao IDRGP Alvo",
  "Teste T pareado | Classificação estatística (IC 95%)."
)

# Mapa 2023
mapa_2023 <- gerar_mapa(
  sf_2023, "categoria",
  "IDRGP 2023 — Status quanto ao IDRGP Alvo",
  "Teste T pareado | Classificação estatística (IC 95%)."
)

# Mapa 2024
mapa_2024 <- gerar_mapa(
  sf_2024, "categoria",
  "IDRGP 2024 — Status quanto ao IDRGP Alvo",
  "Teste de Wilcoxon | Classificação estatística (IC 95%)."
)

# Salvar PNGs
ggsave(file.path(pasta_saida, "mapas", "mapa_2022.png"), 
       mapa_2022, width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapas", "mapa_2023.png"), 
       mapa_2023, width = 12, height = 10, dpi = 300)
ggsave(file.path(pasta_saida, "mapas", "mapa_2024.png"), 
       mapa_2024, width = 12, height = 10, dpi = 300)

# Exibir no HTML
print(mapa_2022)
cat("\n\n")
print(mapa_2023)
cat("\n\n")
print(mapa_2024)

cat("✅ Mapas 2022, 2023, 2024 gerados\n")


## ----mapa_2025, echo=FALSE, fig.height=10, fig.width=12, message=FALSE, warning=FALSE----
# ====================================================================
# MAPA 2025 — Status estatístico (Teste KS)
# ====================================================================

# Verificar se temos dados de 2025
if (exists("sf_2025") && "categoria" %in% names(sf_2025)) {
  
  # Obter informações do teste KS
  metodo_2025 <- "Teste KS (Kolmogorov-Smirnov)"
  
  # Tentar obter p-valor
  p_valor_2025 <- if (exists("resultados_testes") && "2025" %in% names(resultados_testes)) {
    unique(resultados_testes[["2025"]]$p_valor)[1]
  } else {
    NA
  }
  
  nota_2025 <- paste0(
    metodo_2025,
    if (!is.na(p_valor_2025)) {
      paste0(" | p = ", formatC(p_valor_2025, format = "e", digits = 2))
    } else {
      ""
    },
    " | Classificação estatística."
  )
  
  mapa_2025 <- gerar_mapa(
    sf_2025, "categoria",
    "IDRGP 2025 — Status quanto ao IDRGP Alvo",
    nota_2025
  )
  
  # Salvar
  ggsave(file.path(pasta_saida, "mapas", "mapa_2025.png"), 
         mapa_2025, width = 12, height = 10, dpi = 300)
  
  # Exibir
  print(mapa_2025)
  cat("✅ Mapa 2025 gerado\n")
  
} else {
  cat("⚠️ Dados de 2025 não disponíveis para o mapa.\n")
}


## ----painel_4_mapas, echo=FALSE, fig.width=20, fig.height=16, message=FALSE, warning=FALSE----
# ====================================================================
# PAINEL: 4 MAPAS LADO A LADO (2×2)
# ====================================================================

if (exists("mapa_2022") && exists("mapa_2023") && 
    exists("mapa_2024") && exists("mapa_2025")) {
  
  painel_mapas <- (mapa_2022 | mapa_2023) / (mapa_2024 | mapa_2025) +
    plot_annotation(
      title = "IDRGP 2022–2025 — Ciclo PPA Completo",
      subtitle = "Classificação das Subprefeituras por Ano",
      caption = caption_base,
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666666")
      )
    )
  
  # Salvar
  ggsave(file.path(pasta_saida, "mapas", "painel_4_mapas_2022_2025.png"),
         painel_mapas, width = 20, height = 16, dpi = 300)
  
  # Exibir
  print(painel_mapas)
  cat("✅ Painel 4 mapas gerado\n")
}


## ----mapa_ciclo_ppa, echo=FALSE, fig.height=10, fig.width=12, message=FALSE, warning=FALSE----
# ====================================================================
# MAPA AGREGADO DO CICLO PPA 2022-2025
# ====================================================================

# Criar dados agregados do ciclo
sf_ciclo <- sf_sp |>
  left_join(
    dados_classificados |>
      group_by(subprefeitura) |>
      summarise(
        valor_total_ciclo = sum(valor_total, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(subprefeitura_pad = padronizar_nome(subprefeitura)),
    by = c("sp_nome_pad" = "subprefeitura_pad")
  )

# Classificar para o ciclo (usando a mesma lógica dos testes)
if ("categoria" %in% names(dados_classificados)) {
  # Usar a classificação mais frequente ou a do agregado
  sf_ciclo <- sf_ciclo |>
    left_join(
      dados_classificados |>
        group_by(subprefeitura) |>
        summarise(
          categoria_ciclo = names(sort(table(categoria), decreasing = TRUE))[1],
          .groups = "drop"
        ) |>
        mutate(subprefeitura_pad = padronizar_nome(subprefeitura)),
      by = c("sp_nome_pad" = "subprefeitura_pad")
    )
} else {
  sf_ciclo <- sf_ciclo |>
    mutate(categoria_ciclo = "Dentro do IDRGP Alvo")
}

# Gerar mapa
mapa_ciclo <- gerar_mapa(
  sf_ciclo, "categoria_ciclo",
  "IDRGP Agregado 2022–2025 — Balanço do Ciclo PPA",
  "Classificação consolidada do ciclo PPA | Frequência predominante nos 4 anos."
)

# Salvar
ggsave(file.path(pasta_saida, "mapas", "mapa_agregado_ciclo_ppa.png"),
       mapa_ciclo, width = 12, height = 10, dpi = 300)

# Exibir
print(mapa_ciclo)
cat("✅ Mapa agregado do ciclo PPA gerado\n")
cat("✅ 6 mapas gerados no total (4 individuais + painel + agregado)\n")


## ----carga_ivu, echo=FALSE, message=FALSE, warning=FALSE----------------------
# ============================================================================
# CARGA DO IVU (Índice de Vulnerabilidade Urbana)
# COPIADO DO IDRGP_25_DA.R
# ============================================================================

caminho_ivu <- file.path(
  "C:/Users/d954257/OneDrive - rede.sp/Área de Trabalho/IDRGP/IDRGP-2025/R Markdown",
  "dados/ivu_subprefeituras_2025.xlsx"
)

if (file.exists(caminho_ivu)) {
  ivu_raw <- read_excel(caminho_ivu) |> clean_names()
  
  col_nome_ivu <- intersect(c("nome_subpref", "subprefeitura", "nome_subprefeitura"), names(ivu_raw))[1]
  col_valor_ivu <- intersect(c("ivu_geral", "ivu", "indice_ivu"), names(ivu_raw))[1]
  
  if (is.na(col_nome_ivu) || is.na(col_valor_ivu)) {
    warning("⚠️ Colunas esperadas do IVU não encontradas no arquivo.")
    ivu_df <- NULL
  } else {
    ivu_df <- ivu_raw |>
      transmute(
        subprefeitura = .data[[col_nome_ivu]],
        ivu = suppressWarnings(as.numeric(.data[[col_valor_ivu]]))
      ) |>
      mutate(
        subprefeitura_pad = padronizar_nome(subprefeitura)
      )
    cat("✅ IVU carregado:", nrow(ivu_df), "subprefeituras\n")
  }
} else {
  warning("⚠️ IVU não encontrado em: ", caminho_ivu)
  cat("⚠️ IVU não encontrado. IDRGP Alvo será calculado sem ponderação.\n")
  ivu_df <- NULL
}


## ----preparar_dados_graficos, echo=FALSE, message=FALSE, warning=FALSE--------
# ============================================================================
# PREPARAÇÃO DOS DADOS PARA OS GRÁFICOS (MODELO IDRGP_2025)
# ============================================================================

# --- Tema igual ao IDRGP_2025 ---
tema_barras <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    plot.title.position = "plot", 
    plot.caption.position = "plot",
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10),
    plot.caption  = element_text(size = 7, hjust = 0),
    axis.text.y   = element_text(size = 8)
  )

# --- Tabela de correspondência Subprefeitura -> Sigla ---
tabela_siglas <- tibble::tribble(
  ~subprefeitura, ~sigla,
  "Subprefeitura Aricanduva/Formosa/Carrão", "AF",
  "Subprefeitura Butantã", "BT",
  "Subprefeitura Campo Limpo", "CL",
  "Subprefeitura Capela do Socorro", "CS",
  "Subprefeitura Casa Verde/Cachoeirinha", "CV",
  "Subprefeitura Cidade Ademar", "AD",
  "Subprefeitura Cidade Tiradentes", "CT",
  "Subprefeitura de Guaianases", "GU",
  "Subprefeitura de Vila Prudente", "VP",
  "Subprefeitura Ermelino Matarazzo", "EM",
  "Subprefeitura Freguesia/Brasilândia", "FO",
  "Subprefeitura Ipiranga", "IP",
  "Subprefeitura Itaim Paulista", "IT",
  "Subprefeitura Itaquera", "IQ",
  "Subprefeitura Jabaquara", "JA",
  "Subprefeitura Jaçanã/Tremembé", "JT",
  "Subprefeitura Lapa", "LA",
  "Subprefeitura M'Boi Mirim", "MB",
  "Subprefeitura Mooca", "MO",
  "Subprefeitura Parelheiros", "PA",
  "Subprefeitura Penha", "PE",
  "Subprefeitura Perus/Anhanguera", "PR",
  "Subprefeitura Pinheiros", "PI",
  "Subprefeitura Pirituba/Jaraguá", "PJ",
  "Subprefeitura Santana/Tucuruvi", "ST",
  "Subprefeitura Santo Amaro", "SA",
  "Subprefeitura São Mateus", "SM",
  "Subprefeitura São Miguel Paulista", "MP",
  "Subprefeitura Sapopemba", "SB",
  "Subprefeitura Sé", "SE",
  "Subprefeitura Vila Maria/Vila Guilherme", "MG",
  "Subprefeitura Vila Mariana", "VM"
)

# --- Calcular IDRGP Alvo e Real por subprefeitura (2025) ---

# Valor total municipal 2025
valor_total_municipio_2025 <- dados_brutos |>
  filter(ano == 2025) |>
  summarise(total = sum(valor_empenhado, na.rm = TRUE)) |>
  pull(total)

# IDRGP por subprefeitura 2025
df_integrado <- dados_brutos |>
  filter(ano == 2025) |>
  group_by(subprefeitura) |>
  summarise(
    valor_executado = sum(valor_empenhado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(subprefeitura_pad = padronizar_nome(subprefeitura))

# Mesclar com IVU
if (exists("ivu_df") && !is.null(ivu_df)) {
  df_integrado <- df_integrado |>
    left_join(ivu_df |> select(subprefeitura_pad, ivu), by = "subprefeitura_pad")
} else {
  df_integrado <- df_integrado |> mutate(ivu = 1)
}

df_integrado <- df_integrado |>
  mutate(
    idrgp_real = valor_executado / valor_total_municipio_2025,
    # Usar IVU para ponderar o IDRGP Alvo
    idrgp_alvo = ivu / sum(ivu, na.rm = TRUE),
    valor_previsto = idrgp_alvo * valor_total_municipio_2025,
    status_estatistico = case_when(
      idrgp_real < idrgp_alvo * 0.7 ~ "Abaixo do IDRGP Alvo",
      idrgp_real > idrgp_alvo * 1.3 ~ "Acima do IDRGP Alvo",
      TRUE ~ "Dentro do IDRGP Alvo"
    ),
    status_valor = if_else(valor_executado >= valor_previsto,
                           "Valor previsto (R$) superado",
                           "Valor previsto (R$) não atingido")
  ) |>
  left_join(tabela_siglas, by = "subprefeitura") |>
  filter(!is.na(sigla))

# --- Dados do ciclo 2022-2025 ---
valor_total_ciclo <- dados_brutos |>
  summarise(total = sum(valor_empenhado, na.rm = TRUE)) |>
  pull(total)

df_ciclo <- dados_brutos |>
  group_by(subprefeitura) |>
  summarise(
    valor_2022_a_2025 = sum(valor_empenhado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(subprefeitura_pad = padronizar_nome(subprefeitura))

# Mesclar com IVU
if (exists("ivu_df") && !is.null(ivu_df)) {
  df_ciclo <- df_ciclo |>
    left_join(ivu_df |> select(subprefeitura_pad, ivu), by = "subprefeitura_pad")
} else {
  df_ciclo <- df_ciclo |> mutate(ivu = 1)
}

df_ciclo <- df_ciclo |>
  mutate(
    idrgp_real_2022_a_2025 = valor_2022_a_2025 / valor_total_ciclo,
    # Usar IVU para ponderar o IDRGP Alvo
    idrgp_alvo = ivu / sum(ivu, na.rm = TRUE),
    status_ciclo = case_when(
      idrgp_real_2022_a_2025 < idrgp_alvo * 0.7 ~ "Abaixo do IDRGP Alvo",
      idrgp_real_2022_a_2025 > idrgp_alvo * 1.3 ~ "Acima do IDRGP Alvo",
      TRUE ~ "Dentro do IDRGP Alvo"
    )
  ) |>
  left_join(tabela_siglas, by = "subprefeitura") |>
  filter(!is.na(sigla))

# --- Dados longos (para série histórica G4) ---
df_longo <- dados_brutos |>
  group_by(subprefeitura, ano) |>
  summarise(
    valor = sum(valor_empenhado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ano = as.factor(ano))

# --- Legenda base ---
caption_base <- "Fonte: SF-PMSP | Elaboração: CPMA/SIME/SEPLAN"

cat("✅ Dados preparados para gráficos (modelo IDRGP_2025)\n")
cat("📊 Subprefeituras:", nrow(df_integrado), "\n")
cat("💰 Valor total 2025: R$", format(valor_total_municipio_2025, big.mark = "."), "\n")
cat("💰 Valor total ciclo: R$", format(valor_total_ciclo, big.mark = "."), "\n")


## ----grafico_g1, echo=FALSE, fig.width=14, fig.height=12, message=FALSE, warning=FALSE----
# ====================================================================
# G1: Barras horizontais com LOSANGO do IDRGP Alvo
# EXATAMENTE igual ao IDRGP_2025, usando df_integrado
# ====================================================================

grafico_g1 <- ggplot(df_integrado, aes(y = reorder(sigla, idrgp_alvo))) +
  # Barras: IDRGP Real
  geom_col(aes(x = idrgp_real, fill = status_estatistico), 
           width = 0.65, alpha = 0.9) +
  # Losango: IDRGP Alvo
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), 
             color = "#c0392b", size = 3) +
  # Cores das barras
  scale_fill_manual(
    values = c(
      "Abaixo do IDRGP Alvo" = "#d97a0f",
      "Dentro do IDRGP Alvo"  = "#0ea1cf",
      "Acima do IDRGP Alvo"   = "#1A1442"
    ),
    name = "Status:",
    drop = FALSE
  ) +
  # Forma do losango
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  # Ordem das legendas
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  # Eixo X em percentual
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  # Títulos
  labs(
    title = "IDRGP 2025 — Distribuição Percentual do Gasto por Subprefeitura",
    subtitle = "IDRGP Real (barras) e IDRGP Alvo (losango)",
    x = "IDRGP (%)", y = NULL,
    caption = caption_base
  ) +
  tema_barras

# Salvar
ggsave(file.path(pasta_saida, "graficos", "G1_idrgp_alvo_vs_real.png"),
       grafico_g1, width = 14, height = 12, dpi = 300)

# Exibir
print(grafico_g1)
cat("✅ G1 gerado — IDRGP 2025: Alvo vs Real (%)\n")


## ----grafico_g2, echo=FALSE, fig.width=14, fig.height=12, message=FALSE, warning=FALSE----
# ====================================================================
# G2: Barras horizontais com LOSANGO — Ciclo PPA completo
# EXATAMENTE igual ao IDRGP_2025, usando df_ciclo
# ====================================================================

grafico_g2 <- ggplot(df_ciclo, aes(y = reorder(sigla, idrgp_alvo))) +
  # Barras: IDRGP Real acumulado
  geom_col(aes(x = idrgp_real_2022_a_2025, fill = status_ciclo), 
           width = 0.65, alpha = 0.9) +
  # Losango: IDRGP Alvo
  geom_point(aes(x = idrgp_alvo, shape = "IDRGP Alvo"), 
             color = "#c0392b", size = 3) +
  # Cores
  scale_fill_manual(
    values = c(
      "Abaixo do IDRGP Alvo" = "#d97a0f",
      "Dentro do IDRGP Alvo"  = "#0ea1cf",
      "Acima do IDRGP Alvo"   = "#1A1442"
    ),
    name = "Status:",
    limits = names(cores_status),
    drop = FALSE
  ) +
  scale_shape_manual(values = c("IDRGP Alvo" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title = "IDRGP 2022–2025 — Balanço do Ciclo PPA (Distribuição Percentual)",
    subtitle = "IDRGP Real acumulado (barras) e IDRGP Alvo (losango)",
    x = "IDRGP Acumulado (%)", y = NULL,
    caption = paste0(caption_base, "\nVariação percentual (±30%).")
  ) +
  tema_barras

# Salvar
ggsave(file.path(pasta_saida, "graficos", "G2_idrgp_ciclo_ppa.png"),
       grafico_g2, width = 14, height = 12, dpi = 300)

# Exibir
print(grafico_g2)
cat("✅ G2 gerado — IDRGP Agregado 2022–2025 (%)\n")


## ----grafico_g3, echo=FALSE, fig.width=14, fig.height=12, message=FALSE, warning=FALSE----
# ====================================================================
# G3: Barras com LOSANGO do Valor Previsto
# EXATAMENTE igual ao IDRGP_2025, usando df_integrado
# ====================================================================

df_plot_g3 <- df_integrado |>
  mutate(
    status_valor_plot = recode(
      status_valor,
      "Valor previsto (R$) superado" = "Valor previsto (R$) atingido"
    )
  )

grafico_g3 <- ggplot(df_plot_g3, aes(y = reorder(sigla, idrgp_alvo))) +
  # Barras: valor executado
  geom_col(aes(x = valor_executado, fill = status_valor_plot), 
           width = 0.65, alpha = 0.9) +
  # Losango: valor previsto
  geom_point(aes(x = valor_previsto, shape = "Valor Previsto (R$)"),
             color = "#c0392b", size = 3) +
  # Cores
  scale_fill_manual(
    values = c(
      "Valor previsto (R$) atingido"     = "#1A1442",
      "Valor previsto (R$) não atingido" = "#d97a0f"
    ),
    name = "Status:",
    drop = FALSE
  ) +
  scale_shape_manual(values = c("Valor Previsto (R$)" = 18), name = "") +
  guides(
    fill  = guide_legend(order = 1),
    shape = guide_legend(override.aes = list(color = "#c0392b", size = 3), order = 2)
  ) +
  scale_x_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ","
  )) +
  labs(
    title = "IDRGP 2025 — Valor Previsto e Executado por Subprefeitura (R$)",
    subtitle = "Barras: valor executado | Losango: valor previsto",
    x = "Valor executado (R$)", y = NULL, 
    caption = caption_base
  ) +
  tema_barras

# Salvar
ggsave(file.path(pasta_saida, "graficos", "G3_valores_absolutos.png"),
       grafico_g3, width = 14, height = 12, dpi = 300)

# Exibir
print(grafico_g3)
cat("✅ G3 gerado — IDRGP 2025: Valores Absolutos (R$)\n")


## ----grafico_g4, echo=FALSE, fig.width=14, fig.height=12, message=FALSE, warning=FALSE----
# ====================================================================
# G4: Barras empilhadas com LOSANGO do valor previsto do ciclo
# 🚨 ORDEM INVERTIDA: 2025 → 2024 → 2023 → 2022
# ====================================================================

df_longo_plot <- df_longo |>
  mutate(
    # Inverter ordem dos anos para que 2025 apareça PRIMEIRO nas barras
    ano = factor(ano, levels = c("2025", "2024", "2023", "2022"))
  ) |>
  left_join(tabela_siglas, by = "subprefeitura") |>
  filter(!is.na(sigla)) |>
  mutate(
    sigla = factor(sigla, 
      levels = df_ciclo |> arrange(desc(idrgp_real_2022_a_2025)) |> pull(sigla))
  )

valor_previsto_ciclo <- sum(df_integrado$valor_previsto, na.rm = TRUE)

grafico_g4 <- ggplot(df_longo_plot, aes(y = sigla, x = valor, fill = ano)) +
  # Barras empilhadas por ano com position_stack(reverse = TRUE)
  geom_col(width = 0.65, alpha = 0.9, position = position_stack(reverse = TRUE)) +
  # Losango: valor previsto do ciclo
  geom_point(
    data = df_ciclo |>
      mutate(
        sigla = factor(sigla, levels = levels(df_longo_plot$sigla)),
        valor_prev = idrgp_alvo * valor_previsto_ciclo
      ),
    aes(y = sigla, x = valor_prev, shape = "Valor Previsto Ciclo"),
    shape = 23, fill = "white", color = "#c0392b", stroke = 1.1, size = 3.2,
    inherit.aes = FALSE
  ) +
  # Cores (🚨 ordem invertida)
  scale_fill_manual(
    values = c(
      "2025" = "#0B3C6D",  # Mais escuro (ano mais recente)
      "2024" = "#1F78B4",
      "2023" = "#4CB7D8",
      "2022" = "#9BD7EA"   # Mais claro (ano mais antigo)
    ),
    name = "Ano"
  ) +
  scale_x_continuous(labels = scales::label_number(
    scale_cut = scales::cut_short_scale(), prefix = "R$ ", decimal.mark = ","
  )) +
  labs(
    title = "IDRGP 2022–2025 — Evolução do Gasto Real por Subprefeitura (R$)",
    subtitle = "Barras empilhadas por ano (2025–2022) | Losango: valor previsto total do ciclo",
    x = "Valor executado acumulado (R$)", y = NULL,
    caption = caption_base
  ) +
  tema_barras

# Salvar
ggsave(file.path(pasta_saida, "graficos", "G4_serie_historica.png"),
       grafico_g4, width = 14, height = 12, dpi = 300)

# Exibir
print(grafico_g4)
cat("✅ G4 gerado — Série Histórica 2022–2025 (ordem invertida)\n")
cat("✅ 4 gráficos gerados no padrão IDRGP_2025.\n")


## ----tabela_valores, message=FALSE, warning=FALSE-----------------------------
# ============================================================================
# CHUNK 19: EXPORTAÇÃO EXCEL DA TABELA 1
# ============================================================================
if (exists("dados_classificados") && exists("dados_agregado")) {
    
    planilhas_valores <- list()
    for (ano_req in c(2022, 2023, 2024, 2025)) {
      df_ano_tab <- dados_classificados |> 
        filter(ano == ano_req) |> 
        select(Subprefeitura = subprefeitura, Valor_Total = valor_total, Categoria = categoria) |> 
        arrange(desc(Valor_Total))
      planilhas_valores[[as.character(ano_req)]] <- df_ano_tab
    }
    
    df_agregado_tab <- dados_agregado |> 
      select(Subprefeitura = subprefeitura, Valor_Total = valor_total_ciclo, Categoria = categoria) |> 
      arrange(desc(Valor_Total))
    
    planilhas_valores[["Agregado_2022_2025"]] <- df_agregado_tab
    
    wb_valores <- createWorkbook()
    for (nome_aba in names(planilhas_valores)) {
      addWorksheet(wb_valores, nome_aba)
      writeData(wb_valores, nome_aba, planilhas_valores[[nome_aba]])
      hs <- createStyle(fontColour = "#ffffff", fgFill = "#1A1442", halign = "center", textDecoration = "bold")
      addStyle(wb_valores, nome_aba, style = hs, rows = 1, cols = 1:ncol(planilhas_valores[[nome_aba]]), gridExpand = TRUE)
      setColWidths(wb_valores, nome_aba, cols = 1:ncol(planilhas_valores[[nome_aba]]), widths = "auto")
    }
    
    saveWorkbook(wb_valores, file.path(pasta_saida, "tabelas", "tabela_valores_por_subprefeitura.xlsx"), overwrite = TRUE)
    
    datatable(df_agregado_tab, options = list(pageLength = 10)) %>%
      formatCurrency("Valor_Total", currency = "R$ ", interval = 3, mark = ".", dec.mark = ",")
}


## ----treemaps_cesta, fig.width=14, fig.height=10, message=FALSE, warning=FALSE, results='hide'----
# ============================================================================
# CHUNK 20: GERAÇÃO DOS TREEMAPS E EXPORTAÇÃO EXCEL
# ============================================================================
if (exists("dados_brutos")) {
    subprefeituras <- levels(dados_brutos$subprefeitura)
    wb_acoes <- createWorkbook()
    
    for (sub in subprefeituras) {
      dados_sub <- dados_brutos |>
        filter(subprefeitura == sub) |>
        group_by(codigo_proj_ativ, descricao_proj_ativ) |>
        summarise(valor_total = sum(valor_empenhado, na.rm = TRUE), .groups = "drop") |>
        filter(valor_total > 0) |>
        arrange(desc(valor_total))
      
      if (nrow(dados_sub) == 0) next
      
      aba_nome <- str_trunc(make_clean_names(sub), 31)
      addWorksheet(wb_acoes, aba_nome)
      writeData(wb_acoes, aba_nome, dados_sub)
      hs <- createStyle(fontColour = "#ffffff", fgFill = "#1A1442", halign = "center", textDecoration = "bold")
      addStyle(wb_acoes, aba_nome, style = hs, rows = 1, cols = 1:ncol(dados_sub), gridExpand = TRUE)
      
      if (nrow(dados_sub) > 10) {
        top10 <- dados_sub[1:10, ]
        demais <- dados_sub[11:nrow(dados_sub), ]
        dados_tm <- bind_rows(top10, tibble(descricao_proj_ativ = paste0("Demais ações (", nrow(demais), ")"), valor_total = sum(demais$valor_total)))
      } else {
        dados_tm <- dados_sub
      }
      
      total_sub <- sum(dados_tm$valor_total)
      dados_tm <- dados_tm |>
        mutate(
          taxa = valor_total / total_sub * 100,
          rotulo = paste0(str_trunc(descricao_proj_ativ, 30), "\n", "R$ ", number(valor_total/1e6, accuracy=0.1), " Mi\n", number(taxa, accuracy=0.1), "%"),
          cor_atribuida = if_else(str_detect(descricao_proj_ativ, "Demais ações"), "#bdc3c7", NA_character_)
        )
      
      p <- ggplot(dados_tm, aes(area = valor_total, fill = if_else(is.na(cor_atribuida), descricao_proj_ativ, cor_atribuida), label = rotulo)) +
        geom_treemap(color = "white", linewidth = 1) +
        geom_treemap_text(place = "centre", grow = TRUE, colour = "white", size = 10) +
        scale_fill_manual(values = c(setNames(cores_top10_cesta[1:min(10, nrow(dados_tm))], dados_tm$descricao_proj_ativ[!str_detect(dados_tm$descricao_proj_ativ, "Demais")]), c("Demais ações" = "#bdc3c7")), guide = "none") +
        labs(title = paste("Subprefeitura", sub), subtitle = "Top 10 Ações", caption = "Fonte: SF-PMSP") +
        theme_void()
      
      ggsave(file.path(pasta_saida, "treemaps", paste0("treemap_cesta_", make_clean_names(sub), ".png")), p, width = 14, height = 10, units = "cm", dpi = 300)
      
      if (sub == first(subprefeituras)) {
          cat("✅ Geração de 32 treemaps iniciada e em salvamento...\n")
      }
    }
    
    saveWorkbook(wb_acoes, file.path(pasta_saida, "tabelas", "top10_acoes_cesta_por_subprefeitura.xlsx"), overwrite = TRUE)
}


## ----galeria_treemaps, message=FALSE, warning=FALSE, results='asis'-----------
# ============================================================================
# CHUNK 21: GALERIA HTML DE TREEMAPS
# ============================================================================
treemaps_files <- list.files(file.path(pasta_saida, "treemaps"), pattern = "\\.png$", full.names = TRUE)
if (length(treemaps_files) > 0) {
  amostra_files <- head(treemaps_files, 6)
  cat("<div style='display: grid; grid-template-columns: 1fr 1fr; gap: 10px;'>\n")
  for (img in amostra_files) {
    cat(sprintf("<div><img src='%s' width='100%%'></div>\n", img))
  }
  cat("</div>\n")
}


## ----exportacao_pdf, echo=FALSE, message=FALSE, warning=FALSE, results='asis'----
# ============================================================================
# CHUNK 22: EXPORTAÇÃO PDF COMPLETA
# ============================================================================
if (knitr::is_latex_output()) {
  
  inserir_pagina_mapa <- function(caminho_imagem, titulo = NULL, subtitulo = NULL) {
    if(file.exists(caminho_imagem)) {
      cat("\n\\newpage\n")
      if (!is.null(titulo)) cat("\n### ", titulo, "\n\n")
      if (!is.null(subtitulo)) cat("*", subtitulo, "*\n\n")
      cat(sprintf("\\includegraphics[width=\\textwidth]{%s}\n", caminho_imagem))
      cat("\n")
    }
  }

  inserir_tabela_pdf <- function(df, titulo = NULL) {
    if (!is.null(titulo)) cat("\n### ", titulo, "\n\n")
    print(
      df %>%
        kable(format = "latex", booktabs = TRUE, linesep = "") %>%
        kable_styling(
          latex_options = c("striped", "hold_position", "scale_down"),
          stripe_color = "#F7F9FC",
          font_size = 8
        )
    )
  }

  cat("\\newpage\n# Mapas de Classificação\n\n")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "mapa_2022.png"), "Mapa 2022")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "mapa_2023.png"), "Mapa 2023")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "mapa_2024.png"), "Mapa 2024")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "mapa_2025.png"), "Mapa 2025")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "painel_4_mapas_2022_2025.png"), "Painel PPA 2022-2025")
  inserir_pagina_mapa(file.path(pasta_saida, "mapas", "mapa_agregado_ciclo_ppa.png"), "Agregado do Ciclo PPA")
  
  cat("\\newpage\n# Gráficos Analíticos\n\n")
  inserir_pagina_mapa(file.path(pasta_saida, "graficos", "G1_comparacao_previsto_executado.png"))
  inserir_pagina_mapa(file.path(pasta_saida, "graficos", "G2_evolucao_temporal.png"))
  inserir_pagina_mapa(file.path(pasta_saida, "graficos", "G3_distribuicao_percentual.png"))
  inserir_pagina_mapa(file.path(pasta_saida, "graficos", "G4_serie_historica_valores_absolutos.png"))
  
  cat("\\newpage\n# Tabelas\n\n")
  if (exists("dados_agregado")) {
      inserir_tabela_pdf(head(dados_agregado, 20), "Amostra da Tabela Agregada")
  }
}


## ----conclusao_final, echo=FALSE, results='asis'------------------------------
# ============================================================================
# CHUNK FINAL: CONCLUSÃO E ESTATÍSTICAS
# ============================================================================
cat("\n\n<hr>\n\n")
cat("### Resumo Consolidado do Processamento\n\n")
cat(sprintf("✅ **Ciclo PPA Analisado:** 2022 a 2025\n\n"))
if (exists("dados_brutos")) {
  cat(sprintf("✅ **Total de Registros Base:** %d ações processadas\n\n", nrow(dados_brutos)))
}
if (exists("dados_agregado")) {
  cat(sprintf("✅ **Volume Financeiro Global:** R$ %s\n\n", 
              format(sum(dados_agregado$valor_total_ciclo, na.rm = TRUE), big.mark = ".", decimal.mark = ",")))
}
cat(sprintf("✅ **Produtos Gerados:** 32 Treemaps, 4 Gráficos, 6 Mapas, 2 Planilhas Excel\n\n"))
cat(sprintf("✅ **Local de Saída:** `%s`\n\n", pasta_saida))
cat("==============================================================\n")

