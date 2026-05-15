import re

file_path = "IDRGP_25_DA.Rmd"
with open(file_path, "r", encoding="utf-8") as f:
    content = f.read()

# 1. Fix testar_idrgp (remove filter(is.finite(idrgp_diferenca)))
target_1 = """testar_idrgp <- function(df_base) {
  df_teste <- df_base |>
    st_drop_geometry() |>
    select(subprefeitura, valor, idrgp_alvo, idrgp_real, idrgp_diferenca, idrgp_var_percentual) |>
    filter(is.finite(idrgp_diferenca))"""

rep_1 = """testar_idrgp <- function(df_base) {
  df_teste <- df_base |>
    st_drop_geometry() |>
    select(subprefeitura, valor, idrgp_alvo, idrgp_real, idrgp_diferenca, idrgp_var_percentual)"""
content = content.replace(target_1, rep_1)

# 2. Fix shapiro test
target_2 = """    if (n_obs <= 5000) {
      teste_norm <- safe_try(
        stats::shapiro.test(df_teste$idrgp_diferenca),
        on_error = teste_norm
      )"""
rep_2 = """    if (n_obs <= 5000) {
      teste_norm <- safe_try(
        stats::shapiro.test(df_teste$idrgp_diferenca[!is.na(df_teste$idrgp_diferenca)]),
        on_error = teste_norm
      )"""
content = content.replace(target_2, rep_2)

# 3. Fix status_estatistico NA bug
target_3 = """  df_teste <- df_teste |>
    mutate(
      status_var_percentual = case_when(
        idrgp_var_percentual >  0.3  ~ "Acima do IDRGP Alvo",
        idrgp_var_percentual < -0.3  ~ "Abaixo do IDRGP Alvo",
        TRUE                         ~ "Dentro do IDRGP Alvo"
      ),
      status_estatistico = case_when(
        idrgp_diferenca < ic_inf ~ "Abaixo do IDRGP Alvo",
        idrgp_diferenca > ic_sup ~ "Acima do IDRGP Alvo",
        TRUE                     ~ "Dentro do IDRGP Alvo"
      )
    )"""

rep_3 = """  df_teste <- df_teste |>
    mutate(
      status_var_percentual = case_when(
        is.na(idrgp_var_percentual)  ~ "Dentro do IDRGP Alvo",
        idrgp_var_percentual >  0.3  ~ "Acima do IDRGP Alvo",
        idrgp_var_percentual < -0.3  ~ "Abaixo do IDRGP Alvo",
        TRUE                         ~ "Dentro do IDRGP Alvo"
      ),
      status_estatistico = case_when(
        is.na(idrgp_diferenca)   ~ "Dentro do IDRGP Alvo",
        idrgp_diferenca < ic_inf ~ "Abaixo do IDRGP Alvo",
        idrgp_diferenca > ic_sup ~ "Acima do IDRGP Alvo",
        TRUE                     ~ "Dentro do IDRGP Alvo"
      )
    )"""
content = content.replace(target_3, rep_3)

# 4. Remove 70% regionalization chunks
import sys

target_4_start = "```{r acoes_regionalizacao_maior_70_2025_25_DA, echo=FALSE, message=FALSE, warning=FALSE}"
target_4_end = "arq_graf2_25_DA, \"`\\n\", sep = \"\")\n```\n"
idx_s = content.find(target_4_start)
idx_e = content.find(target_4_end) + len(target_4_end)

if idx_s != -1 and idx_e != -1 and idx_e > idx_s:
    content = content[:idx_s] + content[idx_e:]
else:
    print("WARNING: Could not find 70% chunk")

# 5. Remove grafico3
target_5_start = "## G3 \u2014 IDRGP 2025: Valores Absolutos (R$)\n\n```{r grafico3, fig.width=14, fig.height=12}\n"
target_5_end = "print(grafico3)\n```\n"
idx_s5 = content.find(target_5_start)
idx_e5 = content.find(target_5_end) + len(target_5_end)
if idx_s5 != -1 and idx_e5 != -1 and idx_e5 > idx_s5:
    content = content[:idx_s5] + content[idx_e5:]
else:
    print("WARNING: Could not find grafico3 chunk")

# 6. Replace DT::datatable with knitr::kable
target_6 = """if (requireNamespace("DT", quietly = TRUE) && requireNamespace("htmltools", quietly = TRUE)) {
  DT::datatable(
    tbl_out,
    rownames = FALSE,
    caption = htmltools::tags$caption(
      style = "caption-side: top; text-align: left; font-weight: bold;",
      titulo_tabela
    ),
    options = list(pageLength = 15, scrollX = TRUE)
  ) |>
    DT::formatCurrency(
      columns = c("2025"),
      currency = "R$",
      mark = ".",
      dec.mark = ","
    )
} else {
  knitr::kable(tbl_out |> dplyr::slice_head(n = 25), caption = titulo_tabela)
}"""

rep_6 = """knitr::kable(
  tbl_out |>
    dplyr::mutate(
      `2025` = scales::label_number(big.mark = ".", decimal.mark = ",", prefix = "R$ ")(`2025`)
    ),
  caption = titulo_tabela
)"""
content = content.replace(target_6, rep_6)

# 7. Replace gerar_mapa_subpref with original
with open("scratch/mapa_subpref_target.txt", "r", encoding="utf-8") as f:
    target_7 = f.read().strip()
    
with open("scratch/mapa_subpref_original.txt", "r", encoding="utf-8") as f:
    rep_7 = f.read().strip()

# Target 7 has to match exactly, but if there's trailing spaces, standard python replace might fail.
# Let's find using start and end lines.
idx_s7 = content.find("gerar_mapa_subpref <- function(nome_sp) {\n  # 4.1) Filtros internos")
if idx_s7 != -1:
    idx_e7 = content.find("\n}\n\n# 5) Loop", idx_s7) + 2
    if idx_e7 > idx_s7:
        content = content[:idx_s7] + rep_7 + content[idx_e7:]
    else:
        print("WARNING: Could not find end of gerar_mapa_subpref")
else:
    print("WARNING: Could not find start of gerar_mapa_subpref")

with open(file_path, "w", encoding="utf-8") as f:
    f.write(content)

print("Python replace done!")
