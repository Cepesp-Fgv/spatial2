library(RPostgreSQL)
library(DBI)

pgdrv <- dbDriver(drvName = "PostgreSQL")


db_get_elections <- function (year, position, candidate_number, state, turn, name = NULL) {
    conn <- DBI::dbConnect(pgdrv,
                           dbname=Sys.getenv("DB_DATABASE"),
                           host=Sys.getenv("DB_HOST"), port=5432,
                           user = Sys.getenv("DB_USERNAME"),
                           password = Sys.getenv("DB_PASSWORD"))
    
    result <- dbGetQuery(conn, 
        statement = "
          SELECT 
              v.NUM_TURNO, v.UF, c.NUMERO_PARTIDO, v.ANO_ELEICAO, v.COD_MUN_IBGE, v.NUMERO_CANDIDATO, c.SIGLA_PARTIDO, c.NOME_URNA_CANDIDATO, c.DESC_SIT_TOT_TURNO, SUM(v.QTDE_VOTOS) AS QTDE_VOTOS
          FROM
              (SELECT * FROM votos_mun WHERE numero_candidato = $1 AND uf = $2 AND codigo_cargo = $3 AND ano_eleicao = $4 AND num_turno = $5) AS v
          JOIN candidatos c ON c.id_candidato = v.id_candidato AND c.ano_eleicao = cast(v.ano_eleicao as integer)
          WHERE c.NOME_URNA_CANDIDATO = $6
          GROUP BY 1, 2, 3, 4, 5, 6, 7, 8, 9",
        param = list(candidate_number, state, position, year, turn, name)
    )
    
    DBI::dbDisconnect(conn)

    setnames(result, toupper(names(result)))
    
    return(result)
    
}
