# IDRGP 2025 — Índice de Distribuição Regional do Gasto Público no Município de São Paulo

Repositório dedicado à produção, análise e publicação dos produtos técnicos do **IDRGP 2025** (Índice de Distribuição Regional do Gasto Público), desenvolvido a partir do ciclo do Plano Plurianual (PPA 2022–2025) do Município de São Paulo.

O projeto organiza rotinas de processamento de dados orçamentários territorializados, construção de indicadores, análises espaciais e geração automatizada de produtos gráficos, tabelas e mapas voltados ao monitoramento territorial do gasto público municipal.

---

# Sobre o Projeto

O IDRGP é um instrumento previsto no Plano Plurianual do Município de São Paulo, com o objetivo de apoiar análises sobre desigualdades territoriais na distribuição regionalizada do orçamento público.

A metodologia articula:

* Indicadores territoriais de vulnerabilidade urbana;
* Execução orçamentária regionalizada;
* Agregações históricas do ciclo do PPA;
* Análises espaciais por subprefeitura;
* Produtos voltados à transparência pública e monitoramento territorial.

O cálculo central do indicador considera:

```text
IDRGP = AOE − AOR
```

Onde:

* **AOE** = Alocação Orçamentária Esperada;
* **AOR** = Alocação Orçamentária Realizada.

---

# Principais Produtos Gerados

O pipeline do projeto gera automaticamente:

* Relatórios técnicos em RMarkdown;
* Gráficos analíticos e séries históricas;
* Mapas temáticos e cartografia territorial;
* Planilhas consolidadas em `.xlsx`;
* Tabelas para transparência e dados abertos;
* Arquivos geoespaciais;
* Produtos exportados em alta resolução para publicação institucional.

---

# Estrutura do Repositório

| Pasta / Arquivo         | Descrição                              |
| ----------------------- | -------------------------------------- |
| `IDRGP_2025.Rmd`        | Relatório principal do projeto         |
| `dados/`                | Bases utilizadas na análise            |
| `resultados/`           | Produtos exportados automaticamente    |
| `graficos/`             | Gráficos gerados pelo pipeline         |
| `mapas/`                | Mapas temáticos consolidados           |
| `mapas_subprefeituras/` | Mapas individuais por subprefeitura    |
| `tabelas/`              | Tabelas analíticas e planilhas finais  |
| `GIT_HUB_2025/`         | Estrutura da página pública do projeto |
| `*.R`                   | Scripts auxiliares                     |

---

# Metodologia

O projeto foi estruturado com foco em:

* Reprodutibilidade;
* Transparência metodológica;
* Padronização territorial;
* Integração entre dados estatísticos e espaciais;
* Automatização da geração de produtos.

As análises utilizam:

* Dados orçamentários regionalizados;
* Indicadores territoriais de vulnerabilidade;
* Processamento espacial com `sf`;
* Visualizações com `ggplot2`;
* Estrutura modular em `RMarkdown`.

---

# Execução do Projeto

## Requisitos

* R ≥ 4.2
* RStudio
* Pacotes listados no script principal

## Como executar

1. Clone este repositório:

```bash
git clone https://github.com/SEU-USUARIO/SEU-REPOSITORIO.git
```

2. Abra o projeto no RStudio;

3. Verifique se as bases estão organizadas na pasta `dados/`;

4. Execute o arquivo:

```r
IDRGP_2025.Rmd
```

5. Utilize o botão **Knit** para gerar os produtos finais.

---

# Produtos Exportados

Ao final da execução, o pipeline gera automaticamente:

* Gráficos `.png`;
* Mapas territoriais;
* Planilhas `.xlsx`;
* Tabelas consolidadas;
* Produtos para publicação web;
* Estruturas compatíveis com GitHub Pages.

---

# Publicação Web

A pasta:

```bash
GIT_HUB_2025/
```

pode ser utilizada para publicação estática via **GitHub Pages**.

---

# Transparência e Reprodutibilidade

O projeto foi desenvolvido para operar com bases locais e caminhos relativos, reduzindo dependências externas e facilitando auditoria metodológica, replicação e atualização periódica dos produtos.

---

# Licença

Este repositório é disponibilizado para fins de transparência pública, análise territorial e produção técnica.

Verifique os termos de uso das bases de dados utilizadas antes de redistribuição ou reutilização externa.

---

# Créditos

Projeto desenvolvido no âmbito de análises territoriais e monitoramento orçamentário do Município de São Paulo.

Elaboração técnica:
**CPMA / SIME / SEPLAN**
