# Pacotes -----------------------------------------------------------------
if(require(electionsBR) == F) install.packages('electionsBR'); require(electionsBR)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(stringr) == F) install.packages('stringr'); require(stringr)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# Resultados eleitorais CEPESP -----------------------------------------------
party_seats <- read_xlsx("raw_data/party_seats_2020.xlsx") %>% 
  rename(elec_year = 1, city_tse = 4, city_ibge = 5, city_name = 6, party = 8, 
         disp_seats = 10, party_seat_share = 12) %>%
  mutate(party = case_when(party == "PC DO B" ~ "PCDOB",
                           party == "CIDADANIA" ~ "CID",
                           party == "SOLIDARIEDADE" ~ "SD",
                           party == "REPUBLICANOS" ~ "REP",
                           party != "PC DO B" ~ party)) %>% 
  mutate(city_ibge = as.numeric(city_ibge))

# Resultados eleitorais TSE ---------------------------------------------------
voter <- elections_tse(year = 2020, type = "voter_profile") %>% 
  group_by(CD_MUNICIPIO) %>% 
  reframe(voter_sum = sum(QT_ELEITORES_PERFIL),
          NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% ungroup() %>% 
  mutate(CD_MUNICIPIO = as.character(CD_MUNICIPIO),
         CD_MUNICIPIO = str_pad(CD_MUNICIPIO, width = 5, pad = "0"))

party_zone <- elections_tse(year = 2020, type = "party_mun_zone", uf = "all") %>% 
  mutate(SG_PARTIDO = case_when(SG_PARTIDO == "PC do B" ~ "PCDOB",
                                SG_PARTIDO == "CIDADANIA" ~ "CID",
                                SG_PARTIDO == "SOLIDARIEDADE" ~ "SD",
                                SG_PARTIDO == "REPUBLICANOS" ~ "REP",
                                SG_PARTIDO != "PC do B" ~ SG_PARTIDO))

# Resultados eleitorais em municipios onde ha segundo turno, porem o vencedor
# obteve maioria
majority_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO), join_by(CD_MUNICIPIO)) %>% 
  filter(voter_sum > 200000) %>% 
  group_by(CD_MUNICIPIO) %>% 
  filter(NR_TURNO == 1, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% 
  group_by(CD_MUNICIPIO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  group_by(CD_MUNICIPIO) %>% 
  filter(any(vote_share > 0.5),
         QT_VOTOS_NOMINAIS_VALIDOS %in% 
           sort(QT_VOTOS_NOMINAIS_VALIDOS, decreasing = T)[1:2]) %>% 
  ungroup()

# Resultados eleitorais em municipios onde nao existe segundo turno
unique_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO), join_by(CD_MUNICIPIO)) %>% 
  filter(voter_sum < 200000) %>% 
  group_by(CD_MUNICIPIO) %>% 
  filter(NR_TURNO == 1, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% 
  group_by(CD_MUNICIPIO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  group_by(CD_MUNICIPIO) %>% 
  filter(QT_VOTOS_NOMINAIS_VALIDOS %in% 
  sort(QT_VOTOS_NOMINAIS_VALIDOS, decreasing = T)[1:2]) %>% 
  ungroup()

# Resultados eleitorais onde houve segundo turno
second_round <- party_zone %>% 
  left_join(voter %>% select(voter_sum, CD_MUNICIPIO), join_by(CD_MUNICIPIO)) %>% 
  filter(voter_sum > 200000) %>% 
  group_by(CD_MUNICIPIO) %>% 
  filter(NR_TURNO == 2, CD_CARGO == 11) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  reframe(QT_VOTOS_NOMINAIS_VALIDOS = sum(QT_VOTOS_NOMINAIS_VALIDOS),
          NR_TURNO = unique(NR_TURNO), NM_MUNICIPIO = unique(NM_MUNICIPIO)) %>% 
  group_by(CD_MUNICIPIO) %>% 
  mutate(tot_vote = sum(QT_VOTOS_NOMINAIS_VALIDOS)) %>% 
  group_by(CD_MUNICIPIO, SG_PARTIDO) %>% 
  mutate(vote_share = QT_VOTOS_NOMINAIS_VALIDOS/tot_vote) %>% 
  ungroup()

# Os casos de 100% surpreendem a primeira vista, mas e porque estou usando
# apenas votos validos, e algumas candidaturas em segundo lugar sao
# irregulares

# Exporting Data
saveRDS(majority_round, "processed_data/majority_round.RDS")
saveRDS(unique_round, "processed_data/unique_round.RDS")
saveRDS(second_round, "processed_data/second_round.RDS")

# Reading data
majority_round <- read_rds("processed_data/majority_round.RDS")
unique_round <- read_rds("processed_data/unique_round.RDS")
second_round <- read_rds("processed_data/second_round.RDS")

# 1. Dados ideologicos (ZUCCO, POWER, 2023) ----------------------------------
load("raw_data/bls9_estimates_partiespresidents_long.RData")

# Abrindo os dados com a mensuracao mais recente de cada partido
long.table <- long.table %>% group_by(party.or.pres) %>% filter(year %in% max(year)) %>% 
  ungroup()

## 1.1 Camaras municipais -----------------------------------------------------
# Unindo com a base do CEPESP
party_seats <- party_seats %>% 
  left_join(long.table %>% filter(year >= 2017), join_by(party == party.or.pres),
            copy = T) %>% 
  select(-year)

# Visualizando observacoes para as quais nao ha dados ideologicos
View(party_seats %>% filter(is.na(ideo)))

# Partidos na base do CEPESPE que nao estao na base de Zucco e Power
party_seats %>% 
  filter(is.na(ideo)) %>% 
  distinct(party)

# Proporcao de cadeiras ocupadas por partidos sem dados ideologicos
party_seats %>% 
  filter(is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  summarise(soma = sum(party_seat_share),
            proporcao = sum(party_seat_share/disp_seats)) %>% 
  View()

# Tirando a media ideologica de cada camara municipal
party_seats_mun <- party_seats %>% filter(!is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(ideo_mean = sum(party_seat_share * ideo / notna_seats))

# Estatisticas descritivas
max(party_seats_mun$ideo_mean)
min(party_seats_mun$ideo_mean)

### 1.1.2 Unindo a base de coordenadas geograficas ---------------------------

### Brasil --------------------------------------------------------------------
ideo_br <- read_municipality(year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_br, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

### Ceara ---------------------------------------------------------------------
ideo_ce <- read_municipality(code_muni = 23, year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_ce, aes(fill=ideo_mean), color= NA, size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

### Paraiba -------------------------------------------------------------------
ideo_pb <- read_municipality(code_muni = 25, year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_pb, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

# 2. Dados ideologicos (BOLOGNESI, RIBEIRO, CODATO, 2022) ---------------------

# Alguns partidos foram renomeados entre a aplicacao do survey e as eleicoes
# de 2020
# PRB -> REPUBLICANOS (REP)
# PPS -> CIDADANIA (CID)
# PR -> PL

# Gerando os dados de ideologia partidaria (p. 7-8, Tabela 1)
bolognesi.table <- data.frame(party = c("PSTU", "PCO", "PCB", "PSOL", "PCDOB", "PT", "PDT",
                     "PSB", "REDE", "CID", "PV", "PTB", "AVANTE",
                     "SD", "PMN", "PMB", "PHS", "MDB", "PSD", "PSDB",
                     "PODE", "PPL", "PRTB", "PROS", "PRP", "REP", "PL",
                     "PTC", "DC", "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "DEM",
                     "UNIÃO"),
           
                     # Media dos posicionamentos
                     ideo.bmean = c(0.51, 0.61, 0.91, 1.28, 1.92, 2.97, 3.92,
                    4.05, 4.77, 4.92, 5.29, 6.1, 6.32,
                    6.5, 6.88, 6.9, 6.96, 7.01, 7.09, 7.11,
                    7.24, 7.27, 7.45, 7.47, 7.59, 7.78, 7.78,
                    7.86, 8.11, 8.11, 8.13, 8.20, 8.33, 8.55, 8.57,
                    (8.57+8.11)/2)) %>% 
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
            copy = T) %>% 
  rename(leg_ideo.bmean = ideo.bmean, leg_ideo.b = ideo.b)

# Visualizacao de missings
# View(party_seats.b %>% filter(is.na(leg_ideo.b)))

# Gerando medias ideologicas para cada camara municipal
party_seats.b_mun <- party_seats.b %>% #filter(!is.na(leg_ideo.b)) %>% 
  group_by(city_ibge) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(leg_ideo.bmean = sum(party_seat_share * leg_ideo.bmean / notna_seats),
          leg_ideo.b = sum(party_seat_share * leg_ideo.b / notna_seats),
          UF = unique(UF),
          city_name = unique(city_name),
          leg_party = paste(party, collapse = ", ")) %>% 
  ungroup() %>% 
  left_join(party_seats %>% select(city_tse, city_ibge) %>% 
              distinct(city_ibge, city_tse), join_by(city_ibge)) %>% 
  relocate(UF, city_ibge, city_tse, city_name, leg_party)

# Estatisticas descritivas
max(party_seats.b_mun$leg_ideo.bmean, na.rm = T)
min(party_seats.b_mun$leg_ideo.bmean, na.rm = T)
max(party_seats.b_mun$leg_ideo.b, na.rm = T)
min(party_seats.b_mun$leg_ideo.b, na.rm = T)

# Gerando medias ideologicas para cada estado
party_seats.b_state <- party_seats.b %>% filter(!is.na(leg_ideo.b)) %>% 
  group_by(UF) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(leg_ideo.bmean = sum(party_seat_share * leg_ideo.bmean / notna_seats),
          leg_ideo.b = sum(party_seat_share * leg_ideo.b / notna_seats)) %>% 
  ungroup()

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
  left_join(party_seats.b_mun %>% select(city_ibge, city_tse),
            join_by(city_tse))

# Selecionando o vencedor e cruzando com dados legislativos
all_elect.b <- mayor_top.b %>% 
  group_by(city_tse) %>% 
  filter(may_vote_share == max(may_vote_share)) %>% 
  ungroup() %>% 
  left_join(party_seats.b_mun %>% select(city_tse, leg_party,
                                         leg_ideo.bmean, leg_ideo.b, UF),
            join_by(city_tse)) %>% 
  mutate(dist_ideo.bmean = abs(may_ideo.bmean - leg_ideo.bmean),
         dist_ideo.b = abs(may_ideo.b - leg_ideo.b)) %>% 
  select(-QT_VOTOS_NOMINAIS_VALIDOS, -NR_TURNO, -tot_vote) %>% 
  relocate(UF, city_ibge, city_tse, city_name, may_vote_type, mayor_party,
           leg_party)

# Gerando medias ideologicas para cada estado
all_elect_uf.b <- all_elect.b %>% 
  group_by(UF) %>% 
  select(-leg_ideo.bmean, -leg_ideo.b) %>% 
  reframe(dist_ideo.bmean = mean(dist_ideo.bmean, na.rm = T),
          dist_ideo.b = mean(dist_ideo.b, na.rm = T),
          may_ideo.b = mean(may_ideo.b, na.rm = T),
          may_ideo.bmean = mean(may_ideo.bmean, na.rm = T)) %>% 
  ungroup() %>% 
    left_join(party_seats.b_state, join_by(UF))

## 2.3 Geolocalizacao --------------------------------------------------------
### 2.3.1 Por municipio ------------------------------------------------------

# Paraiba
ggplot() +
  geom_sf(data = 
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b, join_by(code_muni==city_ibge)), 
          aes(fill=leg_ideo.bmean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

ggplot() +
  labs(title = "Ideologia partidária dos prefeitos",
       subtitle = "Paraíba") +
  geom_sf(data = 
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b, join_by(code_muni==city_ibge)), 
          aes(fill=may_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(3, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

ggplot() +
  labs(title = "Distância ideológica partidária absoluta entre Executivo e 
       Legislativo",
       subtitle = "Paraíba") +
  geom_sf(data =
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b, join_by(code_muni==city_ibge)), 
          aes(fill=dist_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Distância Ideológica",
                       colors = c(low = "white", mid = "lightgreen",
                                  high = "brown4"),
                       breaks = c(0, 3.38),
                       labels = c("Convergência", "Divergência"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

### 2.3.2 Por estado -------------------------------------------------------
ggplot() +
  geom_sf(data = 
            read_state(code_state = "all", year=2020) %>% 
            left_join(all_elect_uf.b, join_by(abbrev_state==UF))
            , aes(fill=may_ideo.bmean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))


# 4. RDD --------------------------------------------------------------------

## 4.1 Amostra quase-experimental --------------------------------------------

# Vitoria da esquerda sobre a direita/centro
left_wins_right <- mayor_top.b %>% 
  group_by(city_tse) %>% 
  filter(max(may_vote_share) & may_ideo.b[which.max(may_vote_share)] < 0,
         min(may_vote_share) & may_ideo.b[which.min(may_vote_share)] >= 0) %>% 
  ungroup()

# Vitoria da direita/centro sobre a esquerda
right_wins_left <- mayor_top.b %>% 
  group_by(city_tse) %>% 
  filter(max(may_vote_share) & may_ideo.b[which.max(may_vote_share)] >= 0,
         min(may_vote_share) & may_ideo.b[which.min(may_vote_share)] < 0) %>% 
  ungroup()

# Eleicoes competitivas: o segundo colocado obteve muitos votos

# Tratamento: esquerda vence uma direita competitiva
left_wins_right <- left_wins_right %>%
  group_by(city_tse) %>%
  filter(may_vote_share[which.max(may_vote_share)] >= 0.50,
         may_vote_share[which.min(may_vote_share)] >= 0.40,
         may_vote_share == max(may_vote_share)) %>%
  ungroup()

# Controle: direita vence uma esquerda competitiva
right_wins_left <- right_wins_left %>%
  group_by(city_tse) %>%
  filter(may_vote_share[which.max(may_vote_share)] >= 0.50,
         may_vote_share[which.min(may_vote_share)] >= 0.40,
         may_vote_share == max(may_vote_share)) %>%
  ungroup() %>% 
  mutate(may_vote_share = 1 - may_vote_share)

competitive_election <- rbind(left_wins_right,
                        right_wins_left)

## 4.2 Variavel dependente -------------------------------------------------
ideb_2023.in <- read_excel("raw_data/divulgacao_anos_iniciais_municipios_2023.xlsx", 
                           range = cell_rows(10:14507), na = "-")

teste <- competitive_election %>% 
  left_join(ideb_2023.in %>% filter(REDE == "Municipal") %>% 
              select(CO_MUNICIPIO, VL_OBSERVADO_2021, VL_OBSERVADO_2023),
            join_by(city_ibge == CO_MUNICIPIO))

if(require(rddtools) == F) install.packages('rddtools'); require(rddtools)
if(require(magrittr) == F) install.packages('magrittr'); require(magrittr)

teste %>% filter(may_vote_share >= 0.49, may_vote_share <= 0.51) %>% 
  ggplot(aes(x = may_vote_share, y = VL_OBSERVADO_2023)) + 
  geom_point() +
  geom_vline(xintercept = 0.5, color = "red", size = 1, linetype = "dashed")

teste %>% filter(may_vote_share >= 0.49, may_vote_share <= 0.51) %>% 
  select(may_vote_share, VL_OBSERVADO_2023) %>%
  mutate(threshold = as.factor(ifelse(may_vote_share >= 0.5, 1, 0))) %>%
  ggplot(aes(x = may_vote_share, y = VL_OBSERVADO_2023)) +
  geom_point(aes(color = threshold)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 0.5, color = "red",
             size = 1, linetype = "dashed")

teste %>% filter(may_vote_share >= 0.490, may_vote_share <= 0.510) %>% 
  select(may_vote_share, VL_OBSERVADO_2023) %>%
  mutate(threshold = as.factor(ifelse(may_vote_share >= 0.5, 1, 0))) %>%
  ggplot(aes(x = may_vote_share, y = VL_OBSERVADO_2023, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 0.5, color = "red",
             size = 1, linetype = "dashed")

teste %>% filter(may_vote_share >= 0.49, may_vote_share <= 0.51) %>% 
  select(may_vote_share, VL_OBSERVADO_2023) %>%
  mutate(threshold = as.factor(ifelse(may_vote_share >= 0.5, 1, 0))) %>%
  ggplot(aes(x = may_vote_share, y = VL_OBSERVADO_2023, color = threshold)) +
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x ^ 3),
              se = FALSE) +
  scale_color_brewer(palette = "Accent") +
  guides(color = FALSE) +
  geom_vline(xintercept = 0.5, color = "red",
             size = 1, linetype = "dashed")

rdd <- teste %>% filter(may_vote_share >= 0.49, may_vote_share <= 0.51)

rdd_data(y = rdd$VL_OBSERVADO_2023, 
         x = rdd$may_vote_share, 
         cutpoint = 0.5) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

rdd_data(y = rdd$VL_OBSERVADO_2023, 
         x = rdd$may_vote_share, 
         cutpoint = 0.5) %>% 
  rdd_reg_lm(slope = "separate", order = 3) %>% 
  summary()

# 5. Correlacao entre os indices de ideologia ------------------------------

## 5.1 A nivel de camaras municipais ---------------------------------------
ideo.all_br <- ideo_br %>% 
  left_join(party_seats.b_mun, join_by(code_muni == city_ibge))

ggplot(ideo.all_br, aes(x=ideo_mean, y=ideob.mean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação entre classificações distintas de ideologia partidária 
  quando aplicadas às câmaras municipais brasileiras",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=22))

ggplot(ideo.all_br, aes(x=ideo_mean, y=ideo.b)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação linear entre índices de ideologia partidária",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)")
  theme_minimal()

## 5.2 A nivel de partidos --------------------------------------------------
all.table <- bolognesi.table %>%
  left_join(long.table %>% select(party.or.pres,ideo), join_by(party == party.or.pres))

ggplot(all.table, aes(x=ideo, y=ideob.mean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação linear entre índices de ideologia partidária",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)")
  theme_minimal()