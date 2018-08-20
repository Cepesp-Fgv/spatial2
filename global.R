url <- "http://cepesp.io/api/consulta/tse"

d_uniq <- suppressWarnings(suppressMessages(readr::read_csv("data/d_uniq_all_new_aug.csv", locale = readr::locale(encoding = "ISO-8859-1"))))

mun <- readRDS("data/mun_simple3.rds")

d_uniq$winner <- "Loser"
d_uniq[d_uniq[,"DESC_SIT_TOT_TURNO"] %in% c("ELEITO","ELEITO POR MÉDIA","ELEITO POR QP"),"winner"] <- "Winner"
colnames(d_uniq)[colnames(d_uniq)=="ANO_ELEICAO"] <- "anoEleicao"
colnames(d_uniq)[colnames(d_uniq)=="UF"] <- "sigla_UF"
colnames(d_uniq)[colnames(d_uniq)=="V2"] <- "Number_Votes"
colnames(d_uniq)[colnames(d_uniq)=="winner"] <- "Result"

party_template <- readr::read_rds("data/party_template.rds")

sigla_partidos <- c("PRB"=10,"PP"=11,"PDT"=12,"PT"=13,"PTB"=14,"PMDB"=15,"PSTU"=16,"PSL"=17,"REDE"=18,"PTN"=19,"PSC"=20,"PCB"=21,"PR"=22,"PPS"=23,"DEM"=25,"PSDC"=27,"PRTB"=28,"PCO"=29,"NOVO"=30,"PHS"=31,"PMN"=33,"PMB"=35,"PTC"=36,"PSB"=40,"PV"=43,"PRP"=44,"PSDB"=45,"PSOL"=50,"PEN"=51,"PPL"=54,"PSD"=55,"PCdoB"=65,"PTdoB"=70,"SD"=77,"PROS"=90)

