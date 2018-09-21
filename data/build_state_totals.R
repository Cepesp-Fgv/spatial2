rm(list = ls())

suppressWarnings(dir.create("data/state_totals"))

# 1. Download -------------------------------------------------------------

anos <- seq(1998,2014, by = 4)
cargos <- c(1,3,5,6,7)
args <- expand.grid(year = anos, position = cargos)

vars_state <-  list("UF", "QTDE_VOTOS", "ANO_ELEICAO")

state_ls <- purrr::pmap(args, cepespR::get_votes,
                        regional_aggregation = 2,
                        columns_list = vars_state)

state_temp <- purrr::map(state_ls, dplyr::select, UF, QTDE_VOTOS)

state_temp <- purrr::map(state_temp, dplyr::rename, Tot_State = QTDE_VOTOS)

# 2. Save -----------------------------------------------------------------

for(i in seq_along(state_temp)){
  readr::write_rds(state_temp[[i]], paste0("data/state_totals/",args$year[[i]], "_", args$position[[i]], ".rds"))
}
