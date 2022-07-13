# PrEP Community-Level Behaviors Cleaning Script

# Packages for Cleaning and Analysis
library(ARTnetData)
library(tidyverse)
library(backports)
library(rlang)
library(MASS)
library(rethinking)
library(dplyr)

# Load data
d <- ARTnet.wide
l <- ARTnet.long

# Limit to HIV-negative and ever HIV tested
d <- filter(d, hiv2 == 0 & EVERTEST == 1)
nrow(d)

# 3-level PrEP Categories: never, non-current, current
d$prep_cat <- ifelse(d$PREP_REVISED == 0, 0,
                     ifelse(d$artnetPREP_CURRENT == 0, 1,
                            ifelse(d$artnetPREP_CURRENT == 1, 2, NA)))

# Limit to MSM not missing Current/Non-Current PrEP use
d <- filter(d, !is.na(prep_cat))
nrow(d)

# Calculate sum of number of weekly anal acts in partnership - ongoing, casual partnerships only
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ongoing2 == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(num.acts = sum(anal.acts.week)) %>%
  right_join(d, by = "AMIS_ID")

# Calculate sum of number of weekly anal acts in partnership with condom
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ongoing2 == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(num.cp.acts = sum(anal.acts.week.cp)) %>%
  right_join(d, by = "AMIS_ID")

d <- d %>%
  mutate(
    # If missing, then set to 0
    across(c(num.cp.acts), ~ if_else(is.na(.x), 0, .x))
  )

d <- d %>%
  mutate(
    # If missing, then set to 0
    across(c(num.acts), ~ if_else(is.na(.x), 0, .x))
  )

# sum of number of weekly anal acts in partnership without condom
d$num.up.acts <- d$num.acts - d$num.cp.acts


# Calculate probability of always using condoms in one-off partnership(s)

# Unload MASS which also has select()
detach(package:MASS, unload=TRUE) 

# Calculating proportion of individuals that always used condoms in one-off partnerships
lb <- select(l, AMIS_ID, ptype, RAI, IAI, RECUAI, INSUAI) %>%
  filter(ptype == 3) %>%
  filter(RAI == 1 | IAI == 1)

lb$prob.cond <- rep(NA, nrow(lb))
lb$prob.cond[lb$RAI == 1 & lb$IAI == 0] <- lb$RECUAI[lb$RAI == 1 & lb$IAI == 0] /
  lb$RAI[lb$RAI == 1 & lb$IAI == 0]
lb$prob.cond[lb$RAI == 0 & lb$IAI == 1] <- lb$INSUAI[lb$RAI == 0 & lb$IAI == 1] /
  lb$IAI[lb$RAI == 0 & lb$IAI == 1]
lb$prob.cond[lb$RAI == 1 & lb$IAI == 1] <- (lb$RECUAI[lb$RAI == 1 & lb$IAI == 1] +
                                              lb$INSUAI[lb$RAI == 1 & lb$IAI == 1]) /
  (lb$RAI[lb$RAI == 1 & lb$IAI == 1] + lb$IAI[lb$RAI == 1 & lb$IAI == 1])
lb$prob.cond[which(lb$prob.cond == 0.5)] <- 0
lb$prob.cond[which(lb$prob.cond %in% c(88, 99, 44))] <- NA
lb <- select(lb, -c(RAI, IAI, RECUAI, INSUAI))
d <- lb %>%
  right_join(d, by = "AMIS_ID")

