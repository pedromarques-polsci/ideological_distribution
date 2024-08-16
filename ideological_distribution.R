if(require(dplyr) == F) install.packages('dplyr'); require(dplyr)
if(require(geobr) == F) install.packages('geobr'); require(geobr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(haven) == F) install.packages('haven'); require(haven)
if(require(readxl) == F) install.packages('readxl'); require(readxl)
if(require(sf) == F) install.packages('sf'); require(sf)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)

party_seats <- read_xlsx("raw_data/party_seats_2020.xlsx") %>% 
  rename(elec_year = 1, city_ibge = 5, city_name = 6, party = 8, 
         disp_seats = 10, party_seat_share = 12) %>%
  mutate(party = case_when(party == "PC DO B" ~ "PCDOB",
                           party == "CIDADANIA" ~ "CID",
                           party == "SOLIDARIEDADE" ~ "SD",
                           party == "REPUBLICANOS" ~ "REP",
                           party != "PC DO B" ~ party)) %>% 
  mutate(city_ibge = as.numeric(city_ibge))

load("raw_data/bls9_estimates_partiespresidents_long.RData")

ideo <- long.table %>% group_by(party.or.pres) %>% filter(year %in% max(year)) %>% 
  ungroup()

party_seats <- party_seats %>% 
  left_join(ideo %>% filter(year >= 2017), join_by(party == party.or.pres),
            copy = T) %>% 
  select(-year)

View(party_seats %>% filter(is.na(ideo)))

party_seats %>% 
  filter(is.na(ideo)) %>% 
  distinct(party)

party_seats %>% 
  filter(is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  summarise(soma = sum(party_seat_share),
            proporcao = sum(party_seat_share/disp_seats)) %>% 
  View()

ave_ideo <- party_seats %>% filter(!is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(ideo_mean = sum(party_seat_share * ideo / notna_seats))

max(ave_ideo$ideo_mean)
min(ave_ideo$ideo_mean)

ideo_br <- read_municipality(year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_br, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

ideo_ce <- read_municipality(code_muni = 23, year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_ce, aes(fill=ideo_mean), color= NA, size=.15)

ideo_pb <- read_municipality(code_muni = 25, year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_pb, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

# Ao pegar dados de 2021, não se pegam dados de
# 2017: REDE, PV
# 1993: PTC (PRN)
# 1990: PDC/DC

# Em todos os casos, não há dados para PRTB, PMN, PMB, PATRIOTA, AVANTE

## Usando dados de Bolognesi et al. (2021) -------------------------------------

# Um ano após aplicação do survey, PRB foi renomeado pra REPUBLICANOS (REP)
ideo_b <- data.frame(party = c("PSTU", "PCO", "PCB", "PSOL", "PCDOB", "PT", "PDT",
                     "PSB", "REDE", "PPS", "PV", "PTB", "AVANTE",
                     "SD", "PMN", "PMB", "PHS", "MDB", "PSD", "PSDB",
                     "PODE", "PPL", "PRTB", "PROS", "PRP", "REP", "PR",
                     "PTC", "DC", "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "DEM"),
           ideob.mean = c(0.51, 0.61, 0.91, 1.28, 1.92, 2.97, 3.92,
                    4.05, 4.77, 4.92, 5.29, 6.1, 6.32,
                    6.5, 6.88, 6.9, 6.96, 7.01, 7.09, 7.11,
                    7.24, 7.27, 7.45, 7.47, 7.59, 7.78, 7.78,
                    7.86, 8.11, 8.11, 8.13, 8.20, 8.33, 8.55, 8.57)) %>% 
  mutate(ideo.b = case_when(
    ideob.mean <= 1.5 ~ -3,
    ideob.mean >= 1.51 & ideob.mean <= 3 ~ -2,
    ideob.mean >= 3.01 & ideob.mean <= 4.49 ~ -1,
    ideob.mean >= 4.5 & ideob.mean <= 5.5 ~ 0,
    ideob.mean > 5.5 & ideob.mean < 7.01 ~ 1,
    ideob.mean > 7 & ideob.mean <= 8.5 ~ 2,
    ideob.mean >= 8.49 ~ 3))

party_seats.b <- party_seats %>% 
  left_join(ideo_b, join_by(party == party),
            copy = T)

View(party_seats %>% filter(is.na(ideo)))

party_seats.b %>% 
  filter(is.na(ideo.b)) %>% 
  distinct(party)

party_seats %>% 
  filter(is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  summarise(soma = sum(party_seat_share),
            proporcao = sum(party_seat_share/disp_seats)) %>% 
  View()

ave_ideo <- party_seats %>% filter(!is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  mutate(notna_seats = sum(party_seat_share)) %>% 
  reframe(ideo_mean = sum(party_seat_share * ideo / notna_seats))

max(ave_ideo$ideo_mean)
min(ave_ideo$ideo_mean)

ideo_br <- read_municipality(year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_br, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))

ideo_ce <- read_municipality(code_muni = 23, year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_ce, aes(fill=ideo_mean), color= NA, size=.15)

ideo_pb <- read_municipality(code_muni = 25, year=2020) %>% 
  left_join(ave_ideo, join_by(code_muni==city_ibge))

ggplot() +
  geom_sf(data=ideo_pb, aes(fill=ideo_mean), size=.15) +
  scale_fill_gradientn(colors = c(low = "red", mid = "white", high = "blue"))