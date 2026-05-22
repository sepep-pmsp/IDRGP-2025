library(dplyr)
library(readxl)
library(janitor)
library(stringi)
library(stringr)

caminho_tabela <- "dados/base_resumida_ppa_idrgp.xlsx"
caminho_cesta <- "dados/Despesas_IDRGP_2024.xlsx"

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
  "Subprefeitura de Vila Prudente", "VP",
  "Subprefeitura Vila Maria/Vila Guilherme", "MG",
  "Subprefeitura Vila Mariana", "VM"
)

norm_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- stringr::str_to_lower(x)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, "^subprefeitura\\s+(de\\s+)?", "")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", "")
  dplyr::case_when(
    x %in% c("saomiguel", "saomiguelpaulista") ~ "saomiguelpaulista",
    x %in% c("itaim", "itaimpaulista") ~ "itaimpaulista",
    x %in% c("perus", "anhanguera", "perusanhanguera") ~ "perusanhanguera",
    TRUE ~ x
  )
}

dados_brutos <- read_excel(caminho_tabela) |>
  clean_names() |>
  transmute(
    ano_empenho = as.integer(ano_empenho),
    codigo_proj_ativ = as.character(codigo_proj_ativ),
    subprefeitura = as.character(subprefeitura),
    valor = as.numeric(valor),
    join_key = norm_name(subprefeitura)
  ) |>
  inner_join(
    tabela_siglas |>
      mutate(join_key = norm_name(subprefeitura)) |>
      select(join_key, subprefeitura_oficial = subprefeitura),
    by = "join_key"
  ) |>
  transmute(
    ano_empenho,
    codigo_proj_ativ,
    subprefeitura = subprefeitura_oficial,
    valor
  )

tabela_cesta <- read_excel(caminho_cesta) |>
  clean_names() |>
  mutate(codigo_proj_ativ = as.character(codigo_proj_ativ))

dados_cesta <- dados_brutos |>
  filter(codigo_proj_ativ %in% tabela_cesta$codigo_proj_ativ)

base_sintese <- dados_brutos |>
  group_by(Ano = as.character(ano_empenho)) |>
  summarise(valor_total = sum(valor, na.rm = TRUE), .groups = "drop")

cesta_sintese <- dados_cesta |>
  group_by(Ano = as.character(ano_empenho)) |>
  summarise(valor_cesta = sum(valor, na.rm = TRUE), .groups = "drop")

df_sintese <- base_sintese |>
  left_join(cesta_sintese, by = "Ano")

df_total <- df_sintese |>
  summarise(
    Ano = "Total",
    valor_total = sum(valor_total, na.rm = TRUE),
    valor_cesta = sum(valor_cesta, na.rm = TRUE)
  )

out <- bind_rows(df_sintese, df_total)
print(out)
