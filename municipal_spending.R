# Packages ----------------------------------------------------------------
if(require(ipeadatar) == F) install.packages('ipeadatar'); require(ipeadatar)
if(require(janitor) == F) install.packages('janitor'); require(janitor)
if(require(purrr) == F) install.packages('purrr'); require(purrr)
#if(require(sidrar) == F) install.packages('sidrar'); require(sidrar)
if(require(stringi) == F) install.packages('stringi'); require(stringi)
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)


# Municipal Expenditure by Function ---------------------------------------
ipeadatar::search_series(language = "br") %>% View()

expidc <- ipeadatar::search_series(language = "br") %>% 
  filter(grepl("Despesa por função", name),
         str_ends(code, "M"))

mexp <- purrr::map(expidc$code,
                    ~ipeadatar::ipeadata(code = .x, language = "br") %>% 
                     dplyr::filter(uname == "Municípios") %>%
                     dplyr::select(-uname)) %>% 
  purrr::list_rbind()

mexp <- mexp %>% 
  pivot_wider(names_from = code,
              values_from = value)%>% 
  clean_names()

write_rds(expidc, "processed_data/ipeacodes.rds")
write_rds(mexp, "raw_data/mun_expenditure.rds")