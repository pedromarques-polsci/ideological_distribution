# PACOTES -----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(lubridate) == F) install.packages('lubridate'); require(lubridate)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)

# Loading data ------------------------------------------------------------
# Dados municipais
all_elect.zuc <- readRDS("processed_data/all_elect_zuc.RDS")
all_elect.b <- readRDS("processed_data/all_elect_b.RDS")
str_mun_var <- readRDS("processed_data/structured_mun_var.rds")

# Ideologia
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

load("raw_data/bls9_estimates_partiespresidents_long.RData")

# Abrindo os dados com a mensuracao mais recente de cada partido
long.table <- long.table %>% group_by(party.or.pres) %>% 
  filter(year %in% max(year)) %>% 
  ungroup()

# 1. CORRELACOES ----------------------------------------------------------
enr_data <- str_mun_var %>%
  left_join(all_elect.b %>% mutate(date = ymd(paste0(ANO_ELEICAO, "-01-01"))), 
            join_by(tcode == city_ibge, date == date)) %>% 
  group_by(tcode) %>% 
  arrange(date) %>% 
  fill(ANO_ELEICAO:dist_ideo.b, .direction = "down") %>% 
  ungroup() %>% 
  filter(ANO_ELEICAO >= 2016)

# 5. VALIDACAO DOS DADOS ---------------------------------------------------
## 5.1 A nivel de camaras e prefeituras ------------------------------------
all_elect.zuc %>%
  count(ANO_ELEICAO, city_ibge) %>%
  filter(n > 1)

all_elect.b %>%
  count(ANO_ELEICAO, city_ibge) %>%
  filter(n > 1)

ideo_br_2020 <- read_municipality(year=2020) %>% 
  left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
            join_by(code_muni == city_ibge)) %>% 
  left_join(all_elect.zuc %>% filter(ANO_ELEICAO == 2020) %>% 
              select(-c(UF, ANO_ELEICAO, city_tse, city_name, may_vote_type,
                        leg_party, may_vote_share, ANO_ELEICAO, mayor_party)), 
            join_by(code_muni==city_ibge))

ggplot(ideo_br_2020, aes(x=leg_ideo, y=leg_ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação entre classificações distintas de ideologia partidária 
  quando aplicadas às câmaras municipais brasileiras",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=22))

ggplot(ideo_br_2020, aes(x=may_ideo, y=may_ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação entre classificações distintas de ideologia partidária 
  quando aplicadas às câmaras municipais brasileiras",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20))

## 5.2 A nivel de partidos --------------------------------------------------
all.table <- bolognesi.table %>%
  left_join(long.table %>% select(party.or.pres,ideo), join_by(party == party.or.pres))

ggplot(all.table, aes(x=ideo, y=ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação linear entre índices de ideologia partidária",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
theme_minimal()
