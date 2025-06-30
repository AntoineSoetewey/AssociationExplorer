library(dplyr)
library(naniar)
library(forcats)

# Filter for Belgium and select relevant columns
dat_BE <- read.csv("data/ESS11_csv/ESS11.csv", stringsAsFactors = TRUE) |>
  filter(cntry == "BE")

dat <- dat_BE |>
  select(nwspol:pplhlp, polintr:vote, happy:rlgblg, rlgdgr:dscrgrp, ctzcntr:brncntr, livecnta, feethngr, facntr, mocntr, ccrdprs, wrclmch, ctrlife:alcwknd, alcbnge:weighta, medtrun,
         stflife, gndr, agea, rshpsts, ipstrgva, vacc19)

# Replace specific values with NA
dat <- dat %>%
  naniar::replace_with_na(replace = list(
    # media and social trust
    nwspol = c(7777, 8888, 9999),
    netusoft = c(7, 8, 9),
    netustm = c(6666, 7777, 8888, 9999),
    ppltrst = c(77, 88, 99),
    pplfair = c(77, 88, 99),
    pplhlp = c(77, 88, 99),
    # politics
    polintr = c(7, 8, 9),
    psppsgva = c(7, 8, 9),
    actrolga = c(7, 8, 9),
    psppipla = c(7, 8, 9),
    cptppola = c(7, 8, 9),
    trstprl = c(77, 88, 99),
    trstlgl = c(77, 88, 99),
    trstplc = c(77, 88, 99),
    trstplt = c(77, 88, 99),
    trstprt = c(77, 88, 99),
    trstep = c(77, 88, 99),
    trstun = c(77, 88, 99),
    vote = c(7, 8, 9),
    # prtvtebe = c(14, 15, 66, 77, 88, 99),
    # subjective well-being, social exclusion, etc.
    happy = c(77, 88, 99),
    sclmeet = c(77, 88, 99),
    inprdsc = c(77, 88, 99),
    sclact = c(7, 8, 9),
    crmvct = c(7, 8, 9),
    aesfdrk = c(7, 8, 9),
    health = c(7, 8, 9),
    hlthhmp = c(7, 8, 9),
    atchctr = c(77, 88, 99),
    atcherp = c(77, 88, 99),
    rlgblg = c(7, 8, 9),
    # rlgdnm = c(77, 88, 99),
    # rlgdnme = c(77, 88, 99),
    rlgdgr = c(77, 88, 99),
    rlgatnd = c(77, 88, 99),
    pray = c(77, 88, 99),
    dscrgrp = c(7, 8, 9),
    ctzcntr = c(7, 8, 9),
    brncntr = c(7, 8, 9),
    # cntbrthd = c(6666, 7777, 8888, 9999),
    livecnta = c(6666, 7777, 8888, 9999),
    feethngr = c(7, 8, 9),
    facntr = c(7, 8, 9),
    mocntr = c(7, 8, 9),
    ccrdprs = c(66, 77, 88, 99),
    wrclmch = c(6, 7, 8, 9),
    # health and inequality
    ctrlife = c(77, 88, 99),
    etfruit = c(77, 88, 99),
    eatveg = c(77, 88, 99),
    dosprt = c(77, 88, 99),
    cgtsmok = c(7, 8, 9),
    alcfreq = c(77, 88, 99),
    alcwkdy = c(6666, 7777, 8888, 9999),
    alcwknd = c(6666, 7777, 8888, 9999),
    alcbnge = c(7, 8, 9),
    height = c(777, 888, 999),
    weighta = c(777, 888, 999),
    medtrun = c(7, 8, 9),
    
    stflife = c(77, 88, 99),
    gndr = c(9),
    agea = c(999),
    rshpsts = c(66, 77, 88, 99),
    ipstrgva = c(66, 77, 88, 99),
    vacc19 = c(7, 8, 9)
  ))

# Reverse scales
dat <- dat |>
  mutate(
    polintr = 5 - polintr,
    aesfdrk = 5 - aesfdrk,
    health = 6 - health,
    hlthhmp = 4 - hlthhmp,
    rlgatnd = 8 - rlgatnd,
    pray = 8 - pray,
    etfruit = 8 - etfruit,
    eatveg = 8 - eatveg,
    cgtsmok = 7 - cgtsmok,
    alcfreq = 8 - alcfreq,
    alcbnge = 6 - alcbnge,
    
    ipstrgva = 7 - ipstrgva
  )

## Recoding dat$vote
dat$vote <- dat$vote %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2",
    "Not eligible to vote" = "3"
  )

# # Recoding prtvtebe
# dat$prtvtebe <- dat$prtvtebe %>%
#   as.character() %>%
#   fct_recode(
#     "Groen!" = "1",
#     "CD&V" = "2",
#     "N-VA" = "3",
#     "Sp.a (Vooruit)" = "4",
#     "PVDA" = "5",
#     "Vlaams Belang" = "6",
#     "Open VLD" = "7",
#     "cdH (Les Engagés)" = "8",
#     "Ecolo" = "9",
#     "MR" = "10",
#     "PS" = "11",
#     "PTB" = "12",
#     "DéFI" = "13",
#     "Other" = "16"
#   )

## Recoding dat$crmvct
dat$crmvct <- dat$crmvct %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$rlgblg
dat$rlgblg <- dat$rlgblg %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$dscrgrp
dat$dscrgrp <- dat$dscrgrp %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$ctzcntr
dat$ctzcntr <- dat$ctzcntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$brncntr
dat$brncntr <- dat$brncntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$feethngr
dat$feethngr <- dat$feethngr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$facntr
dat$facntr <- dat$facntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$mocntr
dat$mocntr <- dat$mocntr %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

## Recoding dat$medtrun
dat$medtrun <- dat$medtrun %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

# Recoding gndr
dat$gndr <- dat$gndr %>%
  as.character() %>%
  fct_recode(
    "Male" = "1",
    "Female" = "2"
  )

# Recoding rshpsts
dat$rshpsts <- dat$rshpsts %>%
  as.character() %>%
  fct_recode(
    "Legally married" = "1",
    "Living with my partner - not legally recognised" = "3",
    "Living with my partner - legally recognised" = "4",
    "Legally divorced/Civil union dissolved" = "6"
  )

# Recoding vacc19
dat$vacc19 <- dat$vacc19 %>%
  as.character() %>%
  fct_recode(
    "Yes" = "1",
    "No" = "2"
  )

write.csv(dat, "data/ESS11_csv/ESS11_BE_data.csv", row.names = FALSE)
