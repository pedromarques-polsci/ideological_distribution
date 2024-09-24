# PACOTES -----------------------------------------------------------------
if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot) == F) install.packages('geobr'); require(ggplot)
if(require(ggpubr) == F) install.packages('ggpubr'); require(ggpubr)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)
if(require(lubridate) == F) install.packages('lubridate'); require(lubridate)
if(require(plm) == F) install.packages('plm'); require(plm)
if(require(rdrobust) == F) install.packages('rdrobust'); require(rdrobust)
if(require(rddtools) == F) install.packages('rddtools'); require(rddtools)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(tidyr) == F) install.packages('tidyr'); require(tidyr)

# LOADING DATA ------------------------------------------------------------
# Dados municipais
all_elect.zuc <- readRDS("processed_data/all_elect_zuc.RDS")
all_elect.b <- readRDS("processed_data/all_elect_b.RDS")
str_mun_var <- readRDS("processed_data/structured_mun_var.rds")
codebook <- readRDS("raw_data/mun_var_codebook.rds")

# 1. ANALISE DESCRITIVA ---------------------------------------------------

## 1.1 GEOLOCALIZACAO ------------------------------------------------------
### 1.1.1 PARAIBA -----------------------------------------------------------
all_elect.b %>% filter(ANO_ELEICAO == 2020) %>% 
  summary(na.rm = T)

all_elect.b %>% filter(ANO_ELEICAO == 2020) %>% 
  summarise(sd(may_ideo.bmean, na.rm = T)/mean(may_ideo.bmean, na.rm = T))

all_elect.b %>% filter(ANO_ELEICAO == 2020) %>% 
  summarise(sd(dist_ideo.bmean, na.rm = T)/mean(dist_ideo.bmean, na.rm = T))

all_elect.b %>% filter(ANO_ELEICAO == 2020) %>% 
  summarise(sd(leg_ideo.bmean, na.rm = T)/mean(leg_ideo.bmean, na.rm = T))

all_elect.b %>% filter(ANO_ELEICAO == 2020) %>% View()

uniqv <- unique(all_elect.b$may_ideo.bmean)
uniqv[which.max(tabulate(match(all_elect.b$may_ideo.bmean, uniqv)))]

uniqv <- unique(all_elect.b$leg_ideo.bmean)
uniqv[which.max(tabulate(match(all_elect.b$leg_ideo.bmean, uniqv)))]

uniqv <- unique(all_elect.b$dist_ideo.bmean)
uniqv[which.max(tabulate(match(all_elect.b$dist_ideo.bmean, uniqv)))]

all_elect.b %>% 
  filter(ANO_ELEICAO == 2020) %>% 
  count(leg_party) %>% 
  mutate(freq = n / sum(n)) %>% arrange(desc(freq))

all_elect.b %>% 
  filter(ANO_ELEICAO == 2020) %>% 
  count(leg_party) %>% 
  mutate(freq = n / sum(n)) %>% arrange(freq)

all_elect.b %>% 
  filter(ANO_ELEICAO == 2020) %>% 
  count(mayor_party) %>% 
  mutate(freq = n / sum(n)) %>% arrange(desc(freq))

all_elect.b %>% 
  filter(ANO_ELEICAO == 2020) %>% 
  count(mayor_party) %>% 
  mutate(freq = n / sum(n)) %>% arrange(freq)

all_elect.b %>% 
  filter(ANO_ELEICAO == 2020, UF == "PE") %>% 
  View()

#### PREFEITURAS
may_pb <- ggplot() +
  labs(title = "Ideologia partidária dos prefeitos",
       subtitle = "Paraíba (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=may_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(3, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

may_pb

ggsave('plot/may_pb.png', dpi = 300, height = 5, width = 10, unit = 'in', may_pb)

#### LEGISLATURAS
leg_pb <- ggplot() +
  labs(title = "Ideologia partidária das câmaras municipais",
       subtitle = "Paraíba (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=leg_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(4.0, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

leg_pb

ggsave('plot/leg_pb.png', dpi = 300, height = 5, width = 10, unit = 'in', leg_pb)

#### DISTANCIA IDEOLOGICA
dist_pb <- ggplot() +
  labs(title = "Distância ideológica entre prefeito e legislatura",
       subtitle = "Paraíba (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 25, year=2020) %>% 
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=dist_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Distância",
                       colors = c(low = "white", mid = "lightgreen", 
                                  high = "brown4"),
                       breaks = c(0.08, 3.37),
                       labels = c("Convergência", "Divergência"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

dist_pb

ggsave('plot/dist_pb.png', dpi = 300, height = 5, width = 10, unit = 'in', dist_pb)

### 1.1.2 PERNAMBUCO --------------------------------------------------------------
#### PREFEITURAS
may_pe <- ggplot() +
  labs(title = "Ideologia partidária dos prefeitos",
       subtitle = "Pernambuco (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 26, year=2020) %>% 
            filter(code_muni != 2605459) %>% 
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=may_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(2, 8.5),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

may_pe

ggsave('plot/may_pe.png', dpi = 300, height = 5, width = 10, unit = 'in', may_pe)

#### LEGISLATURAS
leg_pe <- ggplot() +
  labs(title = "Ideologia partidária das câmaras municipais",
       subtitle = "Pernambuco (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 26, year=2020)  %>% 
            filter(code_muni != 2605459) %>%  
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=leg_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Ideologia Partidária",
                       colors = c(low = "red", mid = "white", high = "blue"),
                       breaks = c(3.1, 8.3),
                       labels = c("Esquerda", "Direita"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "white")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

leg_pe

ggsave('plot/leg_pe.png', dpi = 300, height = 5, width = 10, unit = 'in', leg_pe)

#### DISTANCIA IDEOLOGICA
dist_pe <- ggplot() +
  labs(title = "Distância ideológica entre prefeito e legislatura",
       subtitle = "Pernambuco (2020)") +
  geom_sf(data = 
            read_municipality(code_muni = 26, year=2020) %>% 
            filter(code_muni != 2605459) %>%  
            left_join(all_elect.b %>% filter(ANO_ELEICAO == 2020), 
                      join_by(code_muni==city_ibge)), 
          aes(fill=dist_ideo.bmean), size=.15) +
  scale_fill_gradientn(name = "Distância",
                       colors = c(low = "white", mid = "lightgreen", 
                                  high = "brown4"),
                       breaks = c(0.1, 4.3),
                       labels = c("Convergência", "Divergência"),
                       guide = guide_colorbar(frame.colour = "black", 
                                              ticks.colour = "darkorange")) +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

dist_pe

ggsave('plot/dist_pe.png', dpi = 300, height = 5, width = 10, unit = 'in', 
       dist_pe)

# 2. ANALISE DE REGRESSAO (NAIVE) -----------------------------------------
## Tratamento final  ------------------------------------------------------
dataset <- str_mun_var %>%
  left_join(all_elect.b %>% 
              mutate(date = ymd(paste0(ANO_ELEICAO + 1, "-01-01"))), 
            join_by(tcode == city_ibge, date == date)) %>% 
  group_by(tcode) %>% 
  arrange(date) %>% 
  fill(ANO_ELEICAO:dist_ideo.b, .direction = "down") %>% 
  mutate(across(recorrm_pcp:dftrabm_pcp, ~ifelse(.x == 0, NA, .x))) %>% 
  mutate(d_dfagrm_pcp = dplyr::lag(dfagrm_pcp, n = 1),
         d_dfasprm_pcp = dplyr::lag(dfasprm_pcp, n = 1),
         d_dfcetm_pcp = dplyr::lag(dfcetm_pcp, n = 1),
         d_dfdefsm_pcp = dplyr::lag(dfdefsm_pcp, n = 1),
         d_dfhabm_pcp = dplyr::lag(dfhabm_pcp, n = 1),
         d_dfeducm_pcp = dplyr::lag(dfeducm_pcp, n = 1),
         d_dfsausm_pcp = dplyr::lag(dfsausm_pcp, n = 1),
         d_dftrabm_pcp = dplyr::lag(dftrabm_pcp, n = 1),
         d_pib_pcp = dplyr::lag(pib_pcp, n = 1),
         d_recorrm_pcp = dplyr::lag(recorrm_pcp, n = 1)) %>% 
  ungroup() %>% 
  mutate(year = lubridate::year(date),
         log_dfagrm_pcp = log(dfagrm_pcp),
         log_dfasprm_pcp = log(dfasprm_pcp),
         log_dfcetm_pcp = log(dfcetm_pcp),
         log_dfdefsm_pcp = log(dfdefsm_pcp),
         log_dfeducm_pcp = log(dfeducm_pcp),
         log_dfhabm_pcp = log(dfhabm_pcp),
         log_dfsausm_pcp = log(dfsausm_pcp),
         log_dftrabm_pcp = log(dftrabm_pcp),
         log_pib_pcp = log(pib_pcp),
         log_recorrm_pcp = log(recorrm_pcp))

enr_data <- dataset %>% filter(date >= "2021-01-01")

# Saude e saneamento
log_enr_data <- enr_data %>% filter(is.finite(log_pib_pcp),
                                    is.finite(log_dfsausm_pcp))

log_enr_data %>% plm(formula = log_dfsausm_pcp ~ may_ideo.bmean + 
                       log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_recorrm_pcp),
                                    is.finite(log_dfsausm_pcp))

log_enr_data %>% plm(formula = log_dfsausm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Trabalho (DFTRABM)
enr_data %>% plm(formula = dftrabm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dftrabm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dftrabm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Habitacao (DFHABM)
enr_data %>% plm(formula = dfhabm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

enr_data %>% plm(formula = dfhabm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfhabm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfhabm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Educacao (DFEDUCM)
log_enr_data <- enr_data %>% filter(is.finite(log_dfeducm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfeducm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfeducm_pcp),
                                    is.finite(log_pib_pcp))

log_enr_data %>% plm(formula = log_dfeducm_pcp ~ may_ideo.bmean + 
                       log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfeducm_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfeducm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Gestao ambiental
enr_data %>% plm(formula = dfagrm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfagrm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfagrm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Protecao social
enr_data %>% plm(formula = dfasprm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfasprm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfasprm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Ciencia e tecnologia
enr_data %>% plm(formula = dfcetm_pcp ~ may_ideo.bmean + recorrm_pcp +
                   pib_pcp + dist_ideo.bmean + leg_ideo.bmean,
                 data = .,
                 index = c("tcode", "year"),
                 model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfcetm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfcetm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# Seguranca publica
log_enr_data <- enr_data %>% filter(is.finite(log_dfdefsm_pcp),
                                    is.finite(log_pib_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfdefsm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean + thomic,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfdefsm_pcp),
                                    is.finite(log_pib_pcp))

log_enr_data %>% plm(formula = log_dfdefsm_pcp ~ may_ideo.bmean + 
                       log_pib_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean + thomic,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

log_enr_data <- enr_data %>% filter(is.finite(log_dfdefsm_pcp),
                                    is.finite(log_recorrm_pcp))

log_enr_data %>% plm(formula = log_dfdefsm_pcp ~ may_ideo.bmean + 
                       log_recorrm_pcp + dist_ideo.bmean + 
                       leg_ideo.bmean + thomic,
                     data = .,
                     index = c("tcode", "year"),
                     model = "pooling") %>%
  coeftest(., vcov = vcovHC(., type="HC0"))

# 3. RDD -------------------------------------------------------------------
## 3.1 Tratamento dos dados ------------------------------------------------
mayor_top.b <- readRDS("processed_data/mayor_top_b.RDS")

# Vitoria da esquerda sobre a direita/centro
left_wins_right <- mayor_top.b %>% 
  group_by(city_tse, ANO_ELEICAO) %>% 
  filter(max(may_vote_share) & may_ideo.b[which.max(may_vote_share)] < 0,
         min(may_vote_share) & may_ideo.b[which.min(may_vote_share)] >= 0) %>% 
  mutate(left_vote_share = 
           max(may_vote_share)/(max(may_vote_share)+min(may_vote_share)),
         right_vote_share = 
           min(may_vote_share)/(max(may_vote_share)+min(may_vote_share))) %>% 
  ungroup() %>% filter(ANO_ELEICAO == 2020)

# Vitoria da direita/centro sobre a esquerda
right_wins_left <- mayor_top.b %>% 
  group_by(city_tse, ANO_ELEICAO) %>% 
  filter(max(may_vote_share) & may_ideo.b[which.max(may_vote_share)] >= 0,
         min(may_vote_share) & may_ideo.b[which.min(may_vote_share)] < 0) %>% 
  mutate(left_vote_share = 
           min(may_vote_share)/(max(may_vote_share)+min(may_vote_share)),
         right_vote_share = 
           max(may_vote_share)/(max(may_vote_share)+min(may_vote_share))) %>% 
  ungroup() %>% filter(ANO_ELEICAO == 2020)

# Base de dados unica: todos os resultados abaixo do cutoff sao derrotas da
# esquerda e vice-verse
competitive_election <- rbind(left_wins_right %>% 
                                group_by(city_tse) %>% 
                                filter(may_vote_share == max(may_vote_share)), 
                              right_wins_left %>% 
                                group_by(city_tse) %>% 
                                filter(may_vote_share == max(may_vote_share)))

# Exporting data
saveRDS(competitive_election, "processed_data/competitive_election.RDS")

## 3.2 Enriquecimento -----------------------------------------------------
competitive_election <- readRDS("processed_data/competitive_election.RDS")

dataset_rdd <- dataset %>% select(date, year, tcode, real_recorrm:dftrabm_pcp,
                                  d_dfagrm_pcp:d_recorrm_pcp, dist_ideo.bmean,
                   log_dfagrm_pcp:log_recorrm_pcp) %>% 
  left_join(competitive_election %>% 
              select(ANO_ELEICAO, city_ibge, left_vote_share) %>% 
              mutate(date = ymd(paste0(ANO_ELEICAO + 1, "-01-01"))),
            join_by(tcode == city_ibge, date == date)) %>% 
  group_by(tcode) %>% 
  arrange(date) %>% 
  fill(ANO_ELEICAO:left_vote_share, .direction = "downup") %>% 
  ungroup() %>% 
  filter(year >= 2021)

## 3.3 Estimation ----------------------------------------------------------
dataset_rdd_2022 <- dataset_rdd %>% filter(year == 2022)
dataset_rdd_2021 <- dataset_rdd %>% filter(year == 2021)

rdrobust(y = dataset_rdd_2022$log_dfdefsm_pcp, 
         x = dataset_rdd_2022$left_vote_share, c = 0.5,
         p = 1) %>% summary()

rdrobust(y = dataset_rdd_2022$log_dfdefsm_pcp, 
         x = dataset_rdd_2022$left_vote_share, c = 0.5,
         p = 3) %>% summary()

rdplot(y = dataset_rdd_2022$log_dfdefsm_pcp, 
       x = dataset_rdd_2022$left_vote_share, c = 0.5, 
       p = 1, h = 0.061, x.lim = c(0.5-0.061,0.5+0.061),
       x.label = "Performance eleitoral da esquerda",
       y.label = "Gasto com Segurança Pública (log) 2022", 
       title = "Efeito Médio de Tratamento Local")

rdplot(y = dataset_rdd_2022$log_dfdefsm_pcp, 
       x = dataset_rdd_2022$left_vote_share, c = 0.5, 
       p = 3, h = 0.122, x.lim = c(0.5-0.122,0.5+0.122),
       x.label = "Performance eleitoral da esquerda",
       y.label = "Gasto com Segurança Pública (log) 2022", 
       title = "Efeito Médio de Tratamento Local")

# 4. VALIDACAO DOS DADOS ---------------------------------------------------
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

## 4.1 A nivel de camaras e prefeituras ------------------------------------
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

corr1 <- ggplot(ideo_br_2020, aes(x=leg_ideo, y=leg_ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação entre classificações distintas de ideologia partidária 
  quando aplicadas às câmaras municipais brasileiras",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=22))

corr1

ggsave('plot/may_corr.png', dpi = 300, height = 5, width = 10, unit = 'in',
       corr1)

corr2 <- ggplot(ideo_br_2020, aes(x=may_ideo, y=may_ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação entre classificações distintas de ideologia partidária 
  quando aplicadas às câmaras municipais brasileiras",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size=20))

corr2

ggsave('plot/leg_corr.png', dpi = 300, height = 5, width = 10, unit = 'in',
       corr2)

## 4.2 A nivel de partidos --------------------------------------------------
all.table <- bolognesi.table %>%
  left_join(long.table %>% select(party.or.pres,ideo), join_by(party == party.or.pres))

corr3 <- ggplot(all.table, aes(x=ideo, y=ideo.bmean)) + 
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  stat_cor(method = "pearson") +
  labs(title = "Correlação linear entre índices de ideologia partidária",
       x = "Zucco & Power (2023)",
       y = "Bolognesi, Ribeiro e Codato (2022)") +
theme_minimal()

corr3

ggsave('plot/party_corr.png', dpi = 300, height = 5, width = 10, unit = 'in',
       corr3)