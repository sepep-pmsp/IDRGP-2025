library(readxl)
library(janitor)

for (ano in 2022:2025) {
  file_name <- sprintf("dados/base_%d_SEPLAN_IDRGP_2025.xlsx", ano)
  if (file.exists(file_name)) {
    df <- read_excel(file_name, n_max = 1) |> clean_names()
    cat("Ano:", ano, "\n")
    cat("Cols:", paste(names(df), collapse=", "), "\n\n")
  }
}
