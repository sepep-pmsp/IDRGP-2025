IDRGP 2025 — Relatório, mapas e página (GitHub Pages)
Este repositório organiza o processamento, análises e produtos (mapas, gráficos e planilhas) do IDRGP 2025, para divulgação e comunicação interna (SEPLAN/CPMA).

O que você encontra aqui:
Relatório em R Markdown (gera tabelas, mapas, gráficos e exportações):
IDRGP_2025.Rmd
(opcionais/legado) RELATORIO_IDRGP_PDF.Rmd, scripts *.R
Produtos gerados (imagens, planilhas e arquivos geoespaciais): pasta definida no Rmd como pasta_saida (por padrão, resultados/).
Bases de dados (arquivos locais): definidas no chunk de caminhos do IDRGP_2025.Rmd.
Como rodar (do zero)
Abra o projeto no RStudio.
Confirme os caminhos no chunk caminhos do IDRGP_2025.Rmd (variável pasta_dados e os arquivo_2022…arquivo_2025 etc.).
Clique em Knit no IDRGP_2025.Rmd.
Se o Knit parar com erro de “Duplicate chunk label”, significa que existem dois chunks com o mesmo nome. Renomeie um deles (ex.: mapas_hist → mapas_hist_2022_2024) ou remova o label ( ```{r, ...}).

Produtos gerados (exportação)
O relatório exporta, normalmente para pasta_saida:

PNG (300 dpi): mapas e gráficos (mapa*.png, grafico*.png)
Excel: bases consolidadas e tabelas de apoio (*.xlsx)
Geoespacial:
GeoPackage: idrgp_2025.gpkg, idrgp_ciclo_2022_2025.gpkg
Shapefile: idrgp_2025_YYYYMMDD.*
Publicar no GitHub Pages (opção simples)
A publicação usa uma pasta com página estática (HTML/CSS/JS). Se a sua pasta tiver outro nome, só ajuste nos passos abaixo.

Coloque a pasta GIT_HUB_2025/ dentro deste repositório (ou confirme que ela já existe).
Suba o repositório para o GitHub.
No GitHub: Settings → Pages
Em Build and deployment:
Source: Deploy from a branch
Branch: main (ou master)
Folder: /GIT_HUB_2025
Aguarde o GitHub gerar o link da página.
Onde editar a página
Conteúdo principal: GIT_HUB_2025/index.html
Estilos (paleta/identidade): GIT_HUB_2025/assets/css/styles.css
Modal de zoom nas imagens: GIT_HUB_2025/assets/js/main.js
Imagens e PDFs:
GIT_HUB_2025/assets/img/maps/
GIT_HUB_2025/assets/img/graphs/
GIT_HUB_2025/assets/docs/
Convenções do relatório (importante)
Labels de chunks no .Rmd precisam ser únicos no arquivo inteiro.
Quando juntar bases de anos diferentes, prefira selecionar só as colunas necessárias (evita erro de tipos diferentes entre anos).
Quando uma coluna “some” por variação de nome (acento/maiúsculas), padronize antes (ex.: sigla_orgao, subprefeitura).
Suporte rápido (erros comuns)
Duplicate chunk label: renomeie o chunk duplicado (ou remova o label).
objeto 'total_valor' não encontrado: use idrgp_por_ano[["2025"]]$total_valor (ou defina total_valor <- res_2025$total_valor antes de usar).
Coluna não encontrada (ex.: sigla_orgao): a base do ano pode vir com nome diferente; padronize antes de bind_rows().
