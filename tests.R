library(arrow)

db_yearly <- open_dataset(sources = "data/banco_anual_tempmed.parquet")
db_daily <- open_dataset(sources = "data/banco_diario_tempmed.parquet")

db_yearly |>
    head() |>
    collect()

db_yearly |>
    mutate(CODMUNRES = substr(as.character(CODMUNRES), 0, 6)) |>
    filter(CODMUNRES == 330455) |>
    collect() |>
    View()

arrow::open_dataset(sources = "data/banco_anual_tempmed.parquet") |>
    dplyr::filter(CODMUNRES == 3304557) |>
    dplyr::arrange(CODMUNRES, ano) |>
    collect()
