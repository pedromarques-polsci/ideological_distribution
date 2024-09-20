# Pacotes -----------------------------------------------------------------
if(require(deflateBR) == F) install.packages('deflateBR'); require(deflateBR)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(electionsBR) == F) install.packages('electionsBR'); require(electionsBR)
# if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(ipeadatar) == F) install.packages('ipeadatar'); require(ipeadatar)
if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(magrittr) == F) install.packages('magrittr'); require(magrittr)
if(require(purrr) == F) install.packages('purrr'); require(purrr)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
# if(require(sf) == F) install.packages('sf'); require(sf)
if(require(sidrar) == F) install.packages('sidrar'); require(sidrar)
if(require(stringr) == F) install.packages('stringr'); require(stringr)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)
if(require(zoo) == F) install.packages('zoo'); require(zoo)

# 1. DADOS ELEITORAIS --------------------------------------------------------
## 1.1 Resultados eleitorais CEPESP ------------------------------------------
party_seats_2020 <- read_xlsx("raw_data/party_seats_2020.xlsx") %>% 
  rename(elec_year = 1, city_tse = 4, city_ibge = 5, city_name = 6, party = 8, 
         disp_seats = 10, party_seat_share = 12) %>%
  mutate(party = case_when(party == "PC DO B" ~ "PCDOB",
                           party == "CIDADANIA" ~ "CID",
                           party == "SOLIDARIEDADE" ~ "SD",
                           party == "REPUBLICANOS" ~ "REP",
                           party != "PC DO B" ~ party)) %>% 
  mutate(city_ibge = as.numeric(city_ibge),
         city_tse = str_pad(city_tse, width = 5, pad = "0")) %>% 
  select(-2, -7, -9, -11)

tse_ibge <- party_seats_2020 %>% 
  distinct(city_tse, city_ibge) %>% 
  select(city_tse, city_ibge)

party_seats_2016 <- read.csv2("raw_data/party_seats_2016.csv", sep = ",") %>% 
  rename(elec_year = 1, city_tse = 4, city_ibge = 5, city_name = 6, party = 8, 
         disp_seats = 10, party_seat_share = 12) %>%
  mutate(party = case_when(party == "PC DO B" ~ "PCDOB",
                           party == "CIDADANIA" ~ "CID",
                           party == "SOLIDARIEDADE" ~ "SD",
                           party == "REPUBLICANOS" ~ "REP",
                           party != "PC DO B" ~ party)) %>% 
  mutate(city_ibge = as.numeric(city_ibge),
         city_tse = str_pad(city_tse, width = 5, pad = "0")) %>% 
  select(-2, -7, -9, -11)

party_seats <- rbind(party_seats_2016, party_seats_2020)

# Exporting data
saveRDS(tse_ibge, "processed_data/tse_ibge.RDS")
saveRDS(party_seats, "processed_data/party_seats.RDS")

## 1.2 Eleitores -------------------------------------------------------------
voter <- rbind(elections_tse(year = 2016, type = "voter_profile") %>% 
                 group_by(CD_MUNICIPIO) %>% 
                 reframe(voter_sum = sum(QT_ELEITORES_PERFIL),
                         NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% ungroup() %>% 
                 mutate(CD_MUNICIPIO = as.character(CD_MUNICIPIO),
                        CD_MUNICIPIO = str_pad(CD_MUNICIPIO, width = 5, pad = "0"),
                        elec_year = 2016),
               elections_tse(year = 2020, type = "voter_profile") %>% 
                 group_by(CD_MUNICIPIO) %>% 
                 reframe(voter_sum = sum(QT_ELEITORES_PERFIL),
                         NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% ungroup() %>% 
                 mutate(CD_MUNICIPIO = as.character(CD_MUNICIPIO),
                        CD_MUNICIPIO = str_pad(CD_MUNICIPIO, width = 5, pad = "0"),
                        elec_year = 2020))

# Exporting Data
saveRDS(voter, "processed_data/voter.RDS")

## 1.3 Resultados eleitorais -----------------------------------------------
party_zone_2020 <- elections_tse(year = 2020, type = "party_mun_zone", uf = "all") %>% 
  mutate(SG_PARTIDO = case_when(SG_PARTIDO == "PC do B" ~ "PCDOB",
                                SG_PARTIDO == "CIDADANIA" ~ "CID",
                                SG_PARTIDO == "SOLIDARIEDADE" ~ "SD",
                                SG_PARTIDO == "REPUBLICANOS" ~ "REP",
                                SG_PARTIDO != "PC do B" ~ SG_PARTIDO)) %>% 
  select(-23:-26, -31, -32, -33, -35, -36)

party_zone_2016 <- elections_tse(year = 2016, type = "party_mun_zone", uf = "all") %>% 
  mutate(SG_PARTIDO = case_when(SG_PARTIDO == "PC do B" ~ "PCDOB",
                                SG_PARTIDO == "CIDADANIA" ~ "CID",
                                SG_PARTIDO == "SOLIDARIEDADE" ~ "SD",
                                SG_PARTIDO == "REPUBLICANOS" ~ "REP",
                                SG_PARTIDO != "PC do B" ~ SG_PARTIDO)) %>% 
  select(-28) %>% rename(QT_VOTOS_NOMINAIS_VALIDOS = 27)

party_zone <- rbind(party_zone_2020, party_zone_2016)

# Exporting Data
saveRDS(party_zone, "processed_data/party_zone.RDS")

# Maioria em primeiro turno entre cidades onde ha segundo turno
majority_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO, elec_year), 
            join_by(CD_MUNICIPIO, ANO_ELEICAO == elec_year)) %>% 
  filter(voter_sum > 200000) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  filter(NR_TURNO == 1, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO),
          ANO_ELEICAO = unique(ANO_ELEICAO)) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO, ANO_ELEICAO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  filter(any(vote_share > 0.5),
         QT_VOTOS_NOMINAIS_VALIDOS %in% 
           sort(QT_VOTOS_NOMINAIS_VALIDOS, decreasing = T)[1:2]) %>% 
  ungroup()

# Cidades sem segundo turno
unique_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO, elec_year), 
            join_by(CD_MUNICIPIO, ANO_ELEICAO == elec_year)) %>% 
  filter(voter_sum < 200000) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  filter(NR_TURNO == 1, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO, ANO_ELEICAO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO),
          ANO_ELEICAO = unique(ANO_ELEICAO)) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO, ANO_ELEICAO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  filter(QT_VOTOS_NOMINAIS_VALIDOS %in% 
           sort(QT_VOTOS_NOMINAIS_VALIDOS, decreasing = T)[1:2]) %>% 
  ungroup()

# Vitoria no segundo turno
second_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO, elec_year), 
            join_by(CD_MUNICIPIO, ANO_ELEICAO == elec_year)) %>% 
  filter(voter_sum > 200000) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  filter(NR_TURNO == 2, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO, ANO_ELEICAO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO),
          ANO_ELEICAO = unique(ANO_ELEICAO)) %>% 
  group_by(CD_MUNICIPIO, ANO_ELEICAO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO, ANO_ELEICAO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  ungroup()

# Os casos de 100% surpreendem a primeira vista, mas e porque estou usando
# apenas votos validos, e algumas candidaturas em segundo lugar sao
# irregulares

# Exporting Data
saveRDS(majority_round, "processed_data/majority_round.RDS")
saveRDS(unique_round, "processed_data/unique_round.RDS")
saveRDS(second_round, "processed_data/second_round.RDS")

## Loading data ------------------------------------------------------------
tse_ibge <- readRDS("processed_data/tse_ibge.RDS")
party_seats <- readRDS("processed_data/party_seats.RDS")
voter <- readRDS("processed_data/voter.RDS")
party_zone <- readRDS("processed_data/party_zone.RDS")
majority_round <- read_rds("processed_data/majority_round.RDS")
unique_round <- read_rds("processed_data/unique_round.RDS")
second_round <- read_rds("processed_data/second_round.RDS")

# 2. IDEOLOGIA (BOLOGNESI, RIBEIRO, CODATO, 2022) ---------------------

# Alguns partidos foram renomeados entre a aplicacao do survey e as eleicoes
# de 2020
# PRB -> REPUBLICANOS (REP)
# PPS -> CIDADANIA (CID)
# PR -> PL

# Alguns partidos mudaram de nome antes da aplicação do survey
# PMDB -> PMB
# PTN -> Podemos
# PSDC -> DC
# PEN -> PATRI / Patriota (ha contreversias)
# PT do B -> Avante

# UNIAO BRASIL = fusao entre DEM e PSL em 2021

# Gerando os dados de ideologia partidaria (p. 7-8, Tabela 1)
bolognesi.table <- data.frame(
  party = c("PSTU", "PCO", "PCB", "PSOL", "PCDOB", "PT", "PDT",
            "PSB", "REDE", "CID", "PPS", "PV", "PTB", 
            "AVANTE", "PT do B", "PT DO B",
            "SD", "PMN", "PMB", "PHS", "MDB", "PMDB", "PSD", "PSDB",
            "PODE", "PTN", "PPL", "PRTB", "PROS", "PRP", "REP", "PRB", "PL", "PR",
            "PTC", "DC", "PSDC", "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "PEN", 
            "PATRI", "DEM", "UNIÃO"),
  
  # Media dos posicionamentos
  ideo.bmean = c(0.51, 0.61, 0.91, 1.28, 1.92, 2.97, 3.92,
                 4.05, 4.77, 4.92, 4.92, 5.29, 6.1, 
                 6.32, 6.32, 6.32,
                 6.5, 6.88, 6.9, 6.96, 7.01, 7.01, 7.09, 7.11,
                 7.24, 7.24, 7.27, 7.45, 7.47, 7.59, 7.78, 7.78, 7.78, 7.78,
                 7.86, 8.11, 8.11, 8.11, 8.13, 8.20, 8.33, 8.55, 8.55, 
                 8.55, 8.57, (8.57+8.11)/2)) %>% 
  # Reescalamento
  mutate(ideo.b = case_when(
    ideo.bmean <= 1.5 ~ -3,
    ideo.bmean >= 1.51 & ideo.bmean <= 3 ~ -2,
    ideo.bmean >= 3.01 & ideo.bmean <= 4.49 ~ -1,
    ideo.bmean >= 4.5 & ideo.bmean <= 5.5 ~ 0,
    ideo.bmean > 5.5 & ideo.bmean < 7.01 ~ 1,
    ideo.bmean > 7 & ideo.bmean <= 8.5 ~ 2,
    ideo.bmean >= 8.49 ~ 3))

## 2.1 Camaras municipais ----------------------------------------------------
# Unindo com a base de CEPESP
party_seats.b <- party_seats %>% 
  left_join(bolognesi.table, join_by(party == party),
            copy = T)

# Visualizacao de missings
View(party_seats.b %>% filter(is.na(ideo.b)))

# Gerando medias ideologicas para cada camara municipal
party_seats.b_mun <- party_seats.b %>% #filter(!is.na(leg_ideo.b)) %>% 
  group_by(city_ibge, elec_year) %>% 
  mutate(notna_seats = sum(party_seat_share),
         leg_party = paste(party, collapse = ", ")) %>% 
  reframe(leg_ideo.bmean = sum(party_seat_share * ideo.bmean / notna_seats),
          leg_ideo.b = sum(party_seat_share * ideo.b / notna_seats),
          UF = unique(UF),
          city_name = unique(city_name),
          leg_party = unique(leg_party),
          elec_year = unique(elec_year), city_ibge = unique(city_ibge),
          city_tse = unique(city_tse)) %>% 
  relocate(elec_year, UF, city_ibge, city_tse, city_name, leg_party) %>% 
  ungroup()

party_seats.b_mun %>% 
  group_by(elec_year) %>% 
  count()

# Estatisticas descritivas
max(party_seats.b_mun$leg_ideo.bmean, na.rm = T)
min(party_seats.b_mun$leg_ideo.bmean, na.rm = T)
max(party_seats.b_mun$leg_ideo.b, na.rm = T)
min(party_seats.b_mun$leg_ideo.b, na.rm = T)

## 2.2 Prefeituras ----------------------------------------------------------
# Unindo com a base de ideologia
mayor_top.b <- rbind(
  second_round %>% 
    mutate(may_vote_type = "second round majority") %>% 
    left_join(bolognesi.table, join_by(SG_PARTIDO == party),
              copy = T), 
  unique_round %>% 
    mutate(may_vote_type = "first round plurality") %>%
    left_join(bolognesi.table, join_by(SG_PARTIDO == party),
              copy = T), 
  majority_round %>% 
    mutate(may_vote_type = "first round majority") %>%
    left_join(bolognesi.table, join_by(SG_PARTIDO == party),
              copy = T)) %>%
  rename(may_ideo.bmean = ideo.bmean, may_ideo.b = ideo.b,
         mayor_party = SG_PARTIDO, city_name = NM_MUNICIPIO, 
         city_tse = CD_MUNICIPIO, may_vote_share = vote_share) %>% 
  left_join(tse_ibge, join_by(city_tse))

# Desempatando
mayor_top.b <- subset(mayor_top.b, !(city_ibge == 2303303 & ANO_ELEICAO == 2016
                                     & mayor_party == "PMB"))

mayor_top.b <- subset(mayor_top.b, !(city_ibge == 2504074 & ANO_ELEICAO == 2020
                                     & mayor_party == "MDB"))

mayor_top.b <- subset(mayor_top.b, !(city_ibge == 4113106 & ANO_ELEICAO == 2020
                                     & mayor_party == "PSD"))

mayor_top.b <- subset(mayor_top.b, !(city_ibge == 4208955 & ANO_ELEICAO == 2020
                                     & mayor_party == "PT"))

# Selecionando o vencedor e cruzando com dados legislativos
all_elect.b <- mayor_top.b %>% 
  group_by(city_tse, ANO_ELEICAO) %>% 
  filter(may_vote_share == max(may_vote_share)) %>% 
  ungroup() %>% 
  left_join(party_seats.b_mun %>% select(city_tse, leg_party, elec_year,
                                         leg_ideo.bmean, leg_ideo.b, UF),
            join_by(city_tse, ANO_ELEICAO == elec_year)) %>% 
  mutate(dist_ideo.bmean = abs(may_ideo.bmean - leg_ideo.bmean),
         dist_ideo.b = abs(may_ideo.b - leg_ideo.b)) %>% 
  select(-QT_VOTOS_NOMINAIS_VALIDOS, -NR_TURNO, -tot_vote) %>% 
  relocate(ANO_ELEICAO, UF, city_ibge, city_tse, city_name, may_vote_type, mayor_party,
           leg_party)

all_elect.b %>%
  count(ANO_ELEICAO, city_ibge) %>%
  filter(n > 1)

# Exporting data
saveRDS(all_elect.b, "processed_data/all_elect_b.RDS")
saveRDS(mayor_top.b, "processed_data/mayor_top_b.RDS")

## Loading data ------------------------------------------------------------
all_elect.b <- readRDS("processed_data/all_elect_b.RDS")
mayor_top.b <- readRDS("processed_data/mayor_top_b.RDS")

# 3. IDEOLOGIA (ZUCCO, POWER, 2023) ----------------------------------------
## Loading data ------------------------------------------------------------
tse_ibge <- readRDS("processed_data/tse_ibge.RDS")
party_seats <- readRDS("processed_data/party_seats.RDS")
voter <- readRDS("processed_data/voter.RDS")
party_zone <- readRDS("processed_data/party_zone.RDS")
majority_round <- read_rds("processed_data/majority_round.RDS")
unique_round <- read_rds("processed_data/unique_round.RDS")
second_round <- read_rds("processed_data/second_round.RDS")

load("raw_data/bls9_estimates_partiespresidents_long.RData")

# Abrindo os dados com a mensuracao mais recente de cada partido
long.table <- long.table %>% group_by(party.or.pres) %>% 
  filter(year %in% max(year)) %>% 
  ungroup()

## 3.1 Camaras municipais -----------------------------------------------------
# Unindo com a base do CEPESP
party_seats.zuc <- party_seats %>% 
  left_join(long.table %>% filter(year >= 2017), join_by(party == party.or.pres),
            copy = T) %>% 
  select(-c(year, ideo.se, ideo.raw))

# Visualizando observacoes para as quais nao ha dados ideologicos
View(party_seats.zuc %>% filter(is.na(ideo)))
View(party_seats.zuc %>% filter(is.na(ideo), elec_year == 2020))

# Partidos na base do CEPESPE que nao estao na base de Zucco e Power
party_seats.zuc %>% 
  filter(is.na(ideo)) %>% 
  distinct(party)

party_seats.zuc %>% 
  filter(is.na(ideo),
         elec_year == 2020) %>% 
  distinct(party)

# Proporcao de cadeiras ocupadas por partidos sem dados ideologicos
party_seats.zuc %>% 
  filter(is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  summarise(soma = sum(party_seat_share),
            proporcao = sum(party_seat_share/disp_seats)) %>% 
  View()

# Tirando a media ideologica de cada camara municipal
party_seats.zuc_mun <- party_seats.zuc %>% 
  mutate(leg_party = paste(party, collapse = ", ")) %>% 
  filter(!is.na(ideo)) %>% 
  group_by(city_ibge, elec_year) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(leg_ideo = sum(party_seat_share * ideo / notna_seats),
          UF = unique(UF),
          city_name = unique(city_name),
          leg_party = unique(leg_party),
          elec_year = unique(elec_year), city_ibge = unique(city_ibge),
          city_tse = unique(city_tse)) %>% 
  relocate(elec_year, UF, city_ibge, city_tse, city_name, leg_party) %>% 
  ungroup()

party_seats.zuc_mun %>% 
  count(elec_year, city_ibge) %>% 
  filter(n > 1)

# Estatisticas descritivas
max(party_seats.zuc_mun$leg_ideo)
min(party_seats.zuc_mun$leg_ideo)

## 3.2 Prefeituras ----------------------------------------------------------
# Unindo com a base de ideologia
mayor_top.zuc <- rbind(
  second_round %>% 
    mutate(may_vote_type = "second round majority") %>% 
    left_join(long.table, join_by(SG_PARTIDO == party.or.pres),
              copy = T), 
  unique_round %>% 
    mutate(may_vote_type = "first round plurality") %>%
    left_join(long.table, join_by(SG_PARTIDO == party.or.pres),
              copy = T), 
  majority_round %>% 
    mutate(may_vote_type = "first round majority") %>%
    left_join(long.table, join_by(SG_PARTIDO == party.or.pres),
              copy = T)) %>%
  rename(may_ideo = ideo,
         mayor_party = SG_PARTIDO, city_name = NM_MUNICIPIO, 
         city_tse = CD_MUNICIPIO, may_vote_share = vote_share) %>% 
  left_join(tse_ibge, join_by(city_tse)) %>% 
  select(-c(ideo.se, ideo.raw))

# Desempatando
mayor_top.zuc <- subset(mayor_top.zuc, !(city_ibge == 2303303 & ANO_ELEICAO == 2016
                                     & mayor_party == "PMB"))

mayor_top.zuc <- subset(mayor_top.zuc, !(city_ibge == 2504074 & ANO_ELEICAO == 2020
                                     & mayor_party == "MDB"))

mayor_top.zuc <- subset(mayor_top.zuc, !(city_ibge == 4113106 & ANO_ELEICAO == 2020
                                     & mayor_party == "PSD"))

mayor_top.zuc <- subset(mayor_top.zuc, !(city_ibge == 4208955 & ANO_ELEICAO == 2020
                                     & mayor_party == "PT"))

# Selecionando o vencedor e cruzando com dados legislativos
all_elect.zuc <- mayor_top.zuc %>% 
  group_by(city_tse, ANO_ELEICAO) %>% 
  filter(may_vote_share == max(may_vote_share)) %>% 
  ungroup() %>% 
  left_join(party_seats.zuc_mun %>% select(city_tse, elec_year,
                                         leg_ideo, UF, leg_party),
            join_by(city_tse, ANO_ELEICAO == elec_year)) %>% 
  mutate(dist_ideo = abs(may_ideo - leg_ideo)) %>% 
  select(-QT_VOTOS_NOMINAIS_VALIDOS, -NR_TURNO, -tot_vote) %>% 
  relocate(ANO_ELEICAO, UF, city_ibge, city_tse, city_name, may_vote_type, 
           mayor_party, leg_party)

# Exporting data
saveRDS(all_elect.zuc, "processed_data/all_elect_zuc.RDS")
saveRDS(mayor_top.zuc, "processed_data/mayor_top_zuc.RDS")

## Loading data ------------------------------------------------------------
all_elect.zuc <- readRDS("processed_data/all_elect_zuc.RDS")
all_elect.b <- readRDS("processed_data/all_elect_b.RDS")

# 4. VARIAVEIS MUNICIPAIS --------------------------------------------------
ipeadatar::search_series(language = "br") %>% filter(theme == "Regional")

mun_exp_code <- ipeadatar::search_series(language = "br") %>% 
  filter(grepl("Despesa por função", name),
         str_ends(code, "M"))

mun_var_code <- c("PIB_IBGE_5938_37", "TACIDT", "THOMIC", "TSUICID", 
                  "POPTOT", "RECORRM")

codebook <- ipeadatar::search_series(language = "br") %>% 
  filter(code %in% mun_var_code) %>% 
  rbind(mun_exp_code)

mun_var <- purrr::map(codebook$code,
                   ~ipeadatar::ipeadata(code = .x, language = "br") %>% 
                     dplyr::filter(uname == "Municípios") %>% 
  dplyr::select(-uname)) %>% 
  purrr::list_rbind()

periodo <- as.character(seq(2000, 2024))

pop <- purrr::map(periodo,
                  ~get_sidra(6579, geo = "City",
                             period = .x)) %>%
  list_rbind() %>%
  clean_names() %>%
  rename(estpop = valor, tcode = municipio_codigo) %>%
  mutate(tcode = as.double(tcode),
         ano = as.double(ano))

# Exporting raw data
saveRDS(mun_var, "raw_data/mun_var.rds")
saveRDS(codebook, "raw_data/mun_var_codebook.rds")
saveRDS(pop, "sidra_estpop.RDS")

# Loading data
mun_var <- readRDS("raw_data/mun_var.rds")
estpop <- readRDS("processed_data/sidra_estpop.rds") %>% 
  filter(nivel_territorial_codigo == 6) %>% 
  mutate(date = ymd(paste0(ano, "-01-01")))

str_var <- mun_var %>%
  tidyr::pivot_wider(
    names_from = code,
    values_from = value
  ) %>%
  janitor::clean_names() %>%
  rename(pib = pib_ibge_5938_37) %>% 
  left_join(estpop %>% select(estpop, tcode, date), 
            join_by(tcode, date))

str_var <- str_var %>% 
  group_by(tcode) %>% 
  mutate(estpop = na.approx(estpop, na.rm = F)) %>% 
  ungroup() %>% 
  mutate(real_recorrm = deflateBR::deflate(nominal_values = str_var$recorrm,
                                       nominal_dates = str_var$date,
                                       index = "ipca",
                                       real_date = '01/2010'),
         real_dfagrm = deflateBR::deflate(nominal_values = str_var$dfagrm,
                                          nominal_dates = str_var$date,
                                          index = "ipca",
                                          real_date = '01/2010'),
         real_dfasprm = deflateBR::deflate(nominal_values = str_var$dfasprm,
                                          nominal_dates = str_var$date,
                                          index = "ipca",
                                          real_date = '01/2010'),
         real_dfcetm = deflateBR::deflate(nominal_values = str_var$dfcetm,
                                           nominal_dates = str_var$date,
                                           index = "ipca",
                                           real_date = '01/2010'),
         real_dfdefsm = deflateBR::deflate(nominal_values = str_var$dfdefsm,
                                          nominal_dates = str_var$date,
                                          index = "ipca",
                                          real_date = '01/2010'),
         real_dfeducm = deflateBR::deflate(nominal_values = str_var$dfeducm,
                                           nominal_dates = str_var$date,
                                           index = "ipca",
                                           real_date = '01/2010'),
         real_dfhabm = deflateBR::deflate(nominal_values = str_var$dfhabm,
                                      nominal_dates = str_var$date,
                                      index = "ipca",
                                      real_date = '01/2010'),
         real_dfsausm = deflateBR::deflate(nominal_values = str_var$dfsausm,
                                          nominal_dates = str_var$date,
                                          index = "ipca",
                                          real_date = '01/2010'),
         real_dftrabm = deflateBR::deflate(nominal_values = str_var$dftrabm,
                                           nominal_dates = str_var$date,
                                           index = "ipca",
                                           real_date = '01/2010'),
         recorrm_pcp = real_recorrm/estpop, 
         pib_pcp = pib/estpop,
         dfagrm_pcp = real_dfagrm/estpop,
         dfasprm_pcp = real_dfasprm/estpop,
         dfcetm_pcp = real_dfcetm/estpop,
         dfdefsm_pcp = real_dfdefsm/ estpop,
         dfeducm_pcp = real_dfeducm/ estpop,
         dfhabm_pcp = real_dfhabm/estpop,
         dfsausm_pcp = real_dfsausm/estpop,
         dftrabm_pcp = real_dftrabm/estpop) %>% 
  select(-poptot)

str_var %>% 
  group_by(date) %>% 
  summarise(recorrm_pcp = sum(is.na(recorrm_pcp)),
            recorrm = sum(is.na(recorrm)),
            real_recorrm = sum(is.na(real_recorrm)),
            estpop = sum(is.na(estpop)))

# Exporting raw data
saveRDS(str_var, "processed_data/structured_mun_var.rds")