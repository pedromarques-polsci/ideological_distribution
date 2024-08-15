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

party_seats %>% 
  filter(is.na(ideo)) %>% 
  distinct(party)

ave_ideo <- party_seats %>% filter(!is.na(ideo)) %>% 
  group_by(city_ibge) %>% 
  reframe(ideo_mean = sum(party_seat_share * ideo / disp_seats))

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