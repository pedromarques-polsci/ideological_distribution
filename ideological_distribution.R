# Pacotes -----------------------------------------------------------------
if(require(electionsBR) == F) install.packages('electionsBR'); require(electionsBR)
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

# Resultados eleitorais CEPESP -----------------------------------------------
party_seats <- read_xlsx("raw_data/party_seats_2020.xlsx") %>% 
  rename(elec_year = 1, city_ibge = 5, city_name = 6, party = 8, 
         disp_seats = 10, party_seat_share = 12) %>%
  mutate(party = case_when(party == "PC DO B" ~ "PCDOB",
                           party == "CIDADANIA" ~ "CID",
                           party == "SOLIDARIEDADE" ~ "SD",
                           party == "REPUBLICANOS" ~ "REP",
                           party != "PC DO B" ~ party)) %>% 
  mutate(city_ibge = as.numeric(city_ibge))

# Resultados eleitorais TSE ---------------------------------------------------
mayor <- elections_tse(year = 2020, type = "candidate")

# 1.1 Dados ideologicos (ZUCCO, POWER, 2023) ----------------------------------
load("raw_data/bls9_estimates_partiespresidents_long.RData")

# Abrindo os dados com a mensuracao mais recente de cada partido
long.table <- long.table %>% group_by(party.or.pres) %>% filter(year %in% max(year)) %>% 
  ungroup()

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

# 1.2 Unindo a base de coordenadas geograficas --------------------------------

## Brasil --------------------------------------------------------------------
ideo_br <- read_municipality(year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_br, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

## Ceara ---------------------------------------------------------------------
ideo_ce <- read_municipality(code_muni = 23, year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_ce, aes(fill=ideo_mean), color= NA, size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

## Paraiba -------------------------------------------------------------------
ideo_pb <- read_municipality(code_muni = 25, year=2020) %>% 
  left_join(party_seats_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_pb, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

# 2.1 Dados ideologicos (BOLOGNESI, RIBEIRO, CODATO, 2022) --------------------

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
                     "PTC", "DC", "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "DEM"),
           
                     # Media dos posicionamentos
                     ideob.mean = c(0.51, 0.61, 0.91, 1.28, 1.92, 2.97, 3.92,
                    4.05, 4.77, 4.92, 5.29, 6.1, 6.32,
                    6.5, 6.88, 6.9, 6.96, 7.01, 7.09, 7.11,
                    7.24, 7.27, 7.45, 7.47, 7.59, 7.78, 7.78,
                    7.86, 8.11, 8.11, 8.13, 8.20, 8.33, 8.55, 8.57)) %>% 
  # Reescalamento
  mutate(ideo.b = case_when(
    ideob.mean <= 1.5 ~ -3,
    ideob.mean >= 1.51 & ideob.mean <= 3 ~ -2,
    ideob.mean >= 3.01 & ideob.mean <= 4.49 ~ -1,
    ideob.mean >= 4.5 & ideob.mean <= 5.5 ~ 0,
    ideob.mean > 5.5 & ideob.mean < 7.01 ~ 1,
    ideob.mean > 7 & ideob.mean <= 8.5 ~ 2,
    ideob.mean >= 8.49 ~ 3))

# Unindo com a base de CEPESP
party_seats.b <- party_seats %>% 
  left_join(bolognesi.table, join_by(party == party),
            copy = T)

# Visualizacao de missings
View(party_seats.b %>% filter(is.na(ideo.b)))

# Gerando medias ideologicas para cada camara municipal
party_seats.b_mun <- party_seats.b %>% filter(!is.na(ideo.b)) %>% 
  group_by(city_ibge) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(ideob.mean = sum(party_seat_share * ideob.mean / notna_seats),
          ideo.b = sum(party_seat_share * ideo.b / notna_seats),
          UF = UF) %>% 
  ungroup()

# Estatisticas descritivas
max(party_seats.b_mun$ideob.mean)
min(party_seats.b_mun$ideob.mean)
max(party_seats.b_mun$ideo.b)
min(party_seats.b_mun$ideo.b)

# Gerando medias ideologicas para cada estado
party_seats.b_state <- party_seats.b %>% filter(!is.na(ideo.b)) %>% 
  group_by(UF) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(ideob.mean = sum(party_seat_share * ideob.mean / notna_seats),
          ideo.b = sum(party_seat_share * ideo.b / notna_seats))

# 2.2 Unindo a base de coordenadas geograficas --------------------------------

## Brasil ------------------------------------------------------------------
ideo.b_br <- read_municipality(year=2020) %>% 
  left_join(party_seats.b_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo.b_br, aes(fill=ideob.mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

## A nivel de estado
ideo.b_uf <- read_state(year=2020) %>% 
  left_join(party_seats.b_state, join_by(abbrev_state==UF))

ggplot() +
  geom_sf(data=ideo.b_uf, aes(fill=ideob.mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

ggplot() +
  geom_sf(data=ideo.b_uf, aes(fill=ideo.b), size=.15) +
  scale_fill_gradientn(limits = c(-3, 3),
                       colours=c("red", "white", "blue"))
  #scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

## Ceara -------------------------------------------------------------------
ideo.b_ce <- read_municipality(code_muni = 23, year=2020) %>% 
  left_join(party_seats.b_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo.b_ce, aes(fill=ideob.mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))


## Paraiba -------------------------------------------------------------------
ideo.b_pb <- read_municipality(code_muni = 25, year=2020) %>% 
  left_join(party_seats.b_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo.b_pb, aes(fill=ideob.mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

## Pernambuco -----------------------------------------------------------------
ideo.b_pe <- read_municipality(code_muni = 26, year=2020) %>% 
  left_join(party_seats.b_mun, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo.b_pe, aes(fill=ideob.mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

# 3. Correlacao entre os indices de ideologia ------------------------------

## 3.1 A nivel de camaras municipais ---------------------------------------
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

## 3.2 A nivel de partidos --------------------------------------------------
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