
library(readstata13)
library(dplyr)
library(tidyr)


setwd("~/Dropbox/officeb2b")

dakope = read.dta13("Raw Data/KOPE2neu.dta")
colnames(dakope) <- tolower(colnames(dakope))
dakope = dakope %>%
  dplyr::select(kundennr, kopenr, abteilungs_kz) %>%
  mutate(kundennr = as.character(kundennr))
dakope = distinct(dakope)
dakope = dakope %>%
  group_by(kundennr, kopenr) %>%
  summarise(abteilungs_kz = sample(abteilungs_kz, 1))



dan = read.csv("Raw Data/181001 Data.csv", numerals = "no.loss")

# set variable names to lower case.
colnames(dan) <- tolower(colnames(dan))


da = dan %>%
  select(artikelnr,
         artikelgruppe,
         kundennr,
         menge,
         rohertrag_basis,
         betriebsgroessennr,
         auftrags_dt,  
         uebermittlungsnr,
         kontakt_key,
         vhproduktgruppe,
         kopenr) %>%
  mutate(date =  as.Date(as.character(auftrags_dt), format="%Y%m%d"),
         kundennr = as.character(kundennr),
         rev = as.numeric(as.character(rohertrag_basis)),
         menge = as.numeric(as.character(menge)),
         month_year= as.Date(cut(date, "month"))) %>%
  filter(rev >= 0) %>%
  filter(date > as.Date("2005-01-01")) %>%       # filter a few early dates
  filter(date < as.Date("2012-12-01")) %>%       # filter a few late dates
  rename(cust_type = betriebsgroessennr) %>%
  filter(cust_type %in% c("A", "B", "C", "D", "E", "F", "G")) %>%
  mutate(digital = if_else(uebermittlungsnr > 20, 1, 0)) 


da = da %>%
  group_by(artikelnr) %>%
  mutate(av_price = mean(rev/menge, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(price_ind = abs((rev/menge)/av_price)) %>%
  group_by(artikelnr) %>%
  mutate(price_ind_995 = quantile(price_ind, .99, na.rm = TRUE)) %>%
  mutate(price_ind = pmin(price_ind, price_ind_995)) %>%
  ungroup() %>%
  mutate(month_year = as.Date(cut(date, "month")),
         price_ind  = replace_na(price_ind, 1))

da = da %>%
  left_join(dakope)


