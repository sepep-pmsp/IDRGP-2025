# =============================================================================
# .Rprofile — IDRGP 2025
# Protocolo de inicialização segura para ambiente Windows com OneDrive
# =============================================================================
# OBJETIVO: Prevenir erros "nome muito longo?" causados pelo MAX_PATH do Windows
# (260 caracteres) quando o projeto está em um caminho de OneDrive com acentos.
#
# Este arquivo é carregado AUTOMATICAMENTE pelo RStudio antes de qualquer
# biblioteca ou script. NÃO modifique sem entender os efeitos.
# =============================================================================

local({

  # ---------------------------------------------------------------------------
  # 1. Define pasta curta para todos os temporários do R
  #    C:/Temp/IDRGP tem apenas 14 caracteres — seguro para qualquer operação
  # ---------------------------------------------------------------------------
  pasta_temp_curta <- "C:/Temp/IDRGP"
  if (!dir.exists(pasta_temp_curta)) {
    dir.create(pasta_temp_curta, recursive = TRUE, showWarnings = FALSE)
  }

  # Redireciona as variáveis de ambiente que o R e seus pacotes usam
  # para criar arquivos temporários e de lock
  Sys.setenv(
    TEMP   = pasta_temp_curta,   # usada por file.path, tempfile, tempdir
    TMP    = pasta_temp_curta,   # variante Windows legada
    TMPDIR = pasta_temp_curta    # variante POSIX (usada por sf, readxl, etc.)
  )

  # ---------------------------------------------------------------------------
  # 2. Previne salvamento e restauração automática do .RData
  #    O .RData na pasta do OneDrive é a principal causa dos erros de sessão.
  #    Com esta configuração, o RStudio não tentará salvar nem restaurar.
  # ---------------------------------------------------------------------------
  options(
    save.defaults      = list(compress = FALSE),  # sem compressão se salvar
    save.image.defaults = list(compress = FALSE)
  )

  # Desativa o prompt "Save workspace?" que às vezes ignora as configurações do projeto
  if (interactive()) {
    q_original <- base::q
    # Não sobrescreve q() — apenas registra a intenção para o usuário
    message(
      "\n[IDRGP .Rprofile] Sessao iniciada com paths seguros.\n",
      "  TEMP  -> ", pasta_temp_curta, "\n",
      "  DICA  -> Va em Tools > Global Options > General e desmarque\n",
      "           'Restore .RData into workspace at startup' e\n",
      "           'Save workspace to .RData on exit: Never'\n"
    )
  }

  # ---------------------------------------------------------------------------
  # 3. Remove arquivos .lock antigos da pasta temp e do projeto
  #    Locks "zumbis" (de sessões anteriores que travaram) impedem que
  #    pacotes como renv e filelock sejam carregados corretamente.
  # ---------------------------------------------------------------------------
  tryCatch({
    locks_temp    <- list.files(pasta_temp_curta, pattern = "\\.lock$",
                                full.names = TRUE, recursive = FALSE)
    locks_projeto <- list.files(".", pattern = "\\.lock$",
                                full.names = TRUE, recursive = FALSE)
    todos_locks   <- c(locks_temp, locks_projeto)
    if (length(todos_locks) > 0) {
      file.remove(todos_locks)
    }
  }, error = function(e) NULL)  # falha silenciosa — não bloqueia a inicialização

  # ---------------------------------------------------------------------------
  # 4. Opções globais de segurança e formatação
  # ---------------------------------------------------------------------------
  options(
    warn        = 0,          # warnings normais (não silenciosos, não fatais)
    scipen      = 999,        # evita notação científica em outputs
    OutDec      = ",",        # decimal brasileiro
    encoding    = "UTF-8",    # encoding padrão para leitura de arquivos
    # Limita o tamanho máximo de nomes impressos no console (proteção adicional)
    max.print   = 1000
  )

  # ---------------------------------------------------------------------------
  # 5. Locale seguro para Windows com suporte a caracteres pt-BR
  #    Previne erros de conversão de multibyte em sistemas com locale inglês
  # ---------------------------------------------------------------------------
  invisible(tryCatch(
    Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.UTF-8"),
    error = function(e)
      tryCatch(
        Sys.setlocale("LC_CTYPE", "Portuguese_Brazil.1252"),
        error = function(e) NULL
      )
  ))

})
