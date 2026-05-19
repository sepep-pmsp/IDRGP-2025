library(sf)
library(dplyr)
library(readxl)
library(janitor)
library(stringr)

# Caminhos corretos
pasta_dados <- "dados"
caminho_dados <- file.path(pasta_dados, "tabela_conferencia_multi_ano.xlsx")
caminho_shapefile <- file.path(pasta_dados, "subprefeitura_v2.shp")

# --- Ler shapefile ---
sf_sp <- st_read(caminho_shapefile, quiet = TRUE) |> clean_names()
padroes_nome <- c("sp_nome", "nm_subpref", "subprefei", "nome", "sub", "NOME", "SUBPREFEI")
col_nome <- intersect(padroes_nome, names(sf_sp))[1]
if (is.na(col_nome)) {
  col_nome <- names(sf_sp)[sapply(sf_sp, is.character)][1]
}
sf_sp <- sf_sp |> rename(sp_nome = all_of(col_nome))

# --- Padronização ---
padronizar_nome <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- x |>
    str_to_lower() |>
    str_squish() |>
    str_replace_all("^subprefeitura de ", "") |>
    str_replace_all("^subprefeitura ", "") |>
    str_replace_all("[^a-z0-9]", "")
  
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

sf_sp <- sf_sp |>
  mutate(join_key = padronizar_nome(sp_nome))

# --- Ler e processar dados brutos ---
dados_brutos <- read_excel(caminho_dados) |> clean_names()

# Verificar nomes e siglas dos dados brutos
print("=== Subprefeituras nos dados brutos ===")
subpref_dados <- unique(dados_brutos$subprefeitura)
print(length(subpref_dados))

# Verificar correspondência de nomes
print("=== Join check ===")
unmatched_shp <- setdiff(sf_sp$join_key, padronizar_nome(subpref_dados))
print(paste("Nomes no shapefile não encontrados nos dados:", paste(unmatched_shp, collapse = ", ")))

unmatched_dados <- setdiff(padronizar_nome(subpref_dados), sf_sp$join_key)
# Filtrar fora supra-regionais para ver se todas as 32 batem
supra_regionais <- c("supraregional", "administracaodireta", "smads", "sms", "sme", "siurb", "smsub")
unmatched_subprefs <- unmatched_dados[!unmatched_dados %in% supra_regionais]
print(paste("Subprefeituras nos dados não encontradas no shapefile (excluindo supraregionais):", paste(unmatched_subprefs, collapse = ", ")))

print(paste("Total matches:", sum(sf_sp$join_key %in% padronizar_nome(subpref_dados))))
