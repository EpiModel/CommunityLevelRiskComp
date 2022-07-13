# PrEP Community-Level Behaviors Analysis Script


library(rethinking)
library(dplyr)

# Load data
d <- ARTnetData::ARTnet.wide
l <- ARTnetData::ARTnet.long

# Table 1 -----------------------------------------------------------------

# PrEP
table(d$prep_cat, useNA = "always")

# Race
addmargins(table(d$race.cat, d$prep_cat, useNA = "always"))

# Age
# 0: 15-24
# 1: 25-34
# 2: 35-44
# 3: 45-54
# 4: 55-65
# 5: 66+
d$age_cat <- ifelse(d$age>=15 & d$age<=24, 1,
                    ifelse(d$age>=25 & d$age<=34, 2,
                           ifelse(d$age>=35 & d$age<=44, 3,
                                  ifelse(d$age>=45 & d$age<=54, 4,
                                         ifelse(d$age>=55 & d$age <=65, 5, NA)))))
addmargins(table(d$age_cat, d$prep_cat, useNA = 'always'))

mean(d$age)
range(d$age)


# Restricting to PrEP/no PrEP based on prep_cat
dprep <- subset(d, prep_cat==2)  # currently on PrEP
dnoprep <- subset(d, prep_cat != 2)  # not currently on PrEP (includes formerly on PrEP and never users)

# Census division
addmargins(table(d$DIVCODE, d$prep_cat, useNA = 'always'))
addmargins(table(d$city2, useNA = 'always'))

# Health insurance
d$insurance <- ifelse(d$TYP_INSA == 1 | d$TYP_INSA2 == 1, 2, 
                      ifelse(d$TYP_INSG == 1 | d$TYP_INSH == 1 |
                               d$TYP_INSD == 1 | d$TYP_INSE == 1 |
                               d$TYP_INSF == 1, 1,
                             ifelse(d$TYP_INSI == 1, 0, NA)))

addmargins(table(d$insurance, d$prep_cat, useNA = 'always'))

# Degree
l$ONGOING <- as.numeric(l$ONGOING)
l$ongoing2 <- ifelse(is.na(l$ONGOING), 0, l$ONGOING)
l$ONGOING <- NULL

d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype == 2) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

# If missing degree, then set to 0
d$deg.main <- ifelse(is.na(d$deg.main), 0, d$deg.main)
d$deg.casl <- ifelse(is.na(d$deg.casl), 0, d$deg.casl)

d$deg.tot <- d$deg.main + d$deg.casl

# Average Total Degree
md <- group_by(d) %>%
  summarise(dt = mean(deg.tot), dtsd=sd(deg.tot))
print(md, n = nrow(md))

# Average Casual Degree
md <- group_by(d) %>%
  summarise(dt = mean(deg.casl), dcsd=sd(deg.casl))
print(md, n = nrow(md))

# Average Main Degree
md <- group_by(d) %>%
  summarise(dt = mean(deg.main), dmsd=sd(deg.main))
print(md, n = nrow(md))

# Median total degree overall
md <- group_by(d) %>%
  summarise(dt = median(deg.tot))
print(md, n = nrow(md))

# Average total degree by PrEP status
md <- group_by(d, prep_cat) %>%
  summarise(dt = mean(deg.tot), dtsd=sd(deg.tot))
print(md, n = nrow(md))

# Median total degree by PrEP status
md <- group_by(d, prep_cat) %>%
  summarise(dt = median(deg.tot))
print(md, n = nrow(md))

# One-off partnerships
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.part = n) %>%
  right_join(d, by = "AMIS_ID")
d$count.mc.part <- ifelse(is.na(d$count.mc.part), 0, d$count.mc.part)

d$count.oo.part <- d$ai.part - d$count.mc.part
d$count.oo.part <- pmax(0, d$count.oo.part)

# Average count of one-time partners overall
meanoo <- group_by(d) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE), oosd=sd(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

meanoo <- group_by(dnoprep) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE), oosd=sd(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Median count of one-time partners overall
medoo <- group_by(d) %>%
  summarise(medoo = median(count.oo.part, na.rm = TRUE))
print(medoo, n = nrow(medoo))

# Average count of one-time partners by PrEP status
meanoo <- group_by(d, prep_cat) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE), oosd=sd(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Median count of one-time partners by PrEP status
medoo <- group_by(d, prep_cat) %>%
  summarise(medoo = median(count.oo.part, na.rm = TRUE))
print(medoo, n = nrow(medoo))

# STI test in past 12 months
d$EVERSTI_TEST <- ifelse(d$EVERSTI_TEST == 7 | d$EVERSTI_TEST == 9,
                         NA, d$EVERSTI_TEST)
d$ANYSTI_TEST <- ifelse(d$ANYSTI_TEST == 7 | d$ANYSTI_TEST == 9,
                        NA, d$ANYSTI_TEST)
d$STITEST_P12M <- ifelse(d$EVERSTI_TEST == 0, 0, d$ANYSTI_TEST)

addmargins(table(d$STITEST_P12M, d$prep_cat, useNA = "always"))


# Table 2 -----------------------------------------------------------------

# PrEP usage by city
addmargins(table(d$city2, d$prep_cat))


# Table 3 -----------------------------------------------------------------

# Average main degree, casual degree, total degree by city (any PrEP status)
md <- group_by(d, city2) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree overall (any PrEP status)
md <- group_by(d) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot), dmsd=sd(deg.main), dcsd=sd(deg.casl), dtsd=sd(deg.tot))
print(md, n = nrow(md))

# Average count of one-time partners by city (any PrEP status)
meanoo <- group_by(d, city2) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Average count of one-time partners overall (any PrEP status)
meanoo <- group_by(d) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# AI rate with ptype=1
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype == 1) %>%                                                
  group_by(AMIS_ID) %>%
  summarise(AIrate = sum(anal.acts.week)) %>%
  right_join(d, by = "AMIS_ID")

# Average AI rate within persistent partnership by city (any PrEP status)
meanAI <- group_by(d, city2) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))

# Average AI rate within persistent partnership overall (any PrEP status)
meanAI <- group_by(d) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))


# Supplemental Table 1 ----------------------------------------------------

# Restricting to PrEP/no PrEP based on prep_cat
dprep <- subset(d, prep_cat==2)  # currently on PrEP
dnoprep <- subset(d, prep_cat != 2)  # not currently on PrEP (includes formerly on PrEP and never users)

# Average main degree, casual degree, total degree overall (PrEP)
md <- group_by(dprep) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot), dmsd=sd(deg.main), dcsd=sd(deg.casl), dtsd=sd(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree overall (not on PrEP)
md <- group_by(dnoprep) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot), dmsd=sd(deg.main), dcsd=sd(deg.casl), dtsd=sd(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree by city (on PrEP)
md <- group_by(dprep, city2) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree overall (on PrEP)
md <- group_by(dprep) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree by city (not on PrEP)
md <- group_by(dnoprep, city2) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))

# Average main degree, casual degree, total degree overall (not on PrEP)
md <- group_by(dnoprep) %>%
  summarise(dm = mean(deg.main), dc = mean(deg.casl), dt = mean(deg.tot))
print(md, n = nrow(md))

# Average count of one-time partners by city (on PrEP)
meanoo <- group_by(dprep, city2) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Average count of one-time partners overall (on PrEP)
meanoo <- group_by(dprep) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Average count of one-time partners by city (not on PrEP)
meanoo <- group_by(dnoprep, city2) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Average count of one-time partners overall (not on PrEP)
meanoo <- group_by(dnoprep) %>%
  summarise(meanoo = mean(count.oo.part, na.rm = TRUE))
print(meanoo, n = nrow(meanoo))

# Average AI rate within persistent partnership by city (on PrEP)
meanAI <- group_by(dprep, city2) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))

# Average AI rate within persistent partnership overall (on PrEP)
meanAI <- group_by(dprep) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))

# Average AI rate within persistent partnership by city (not on PrEP)
meanAI <- group_by(dnoprep, city2) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))

# Average AI rate within persistent partnership overall (not on PrEP)
meanAI <- group_by(dnoprep) %>%
  summarise(meanAI = mean(AIrate, na.rm = TRUE))
print(meanAI, n = nrow(meanAI))

# Network degree summary measures
summary(d$deg.tot)
sd(d$deg.tot)

summary(d$deg.main)
sd(d$deg.main)

summary(d$deg.casl)
sd(d$deg.casl)

# ------------------------------------------------------------------------

# Modeling prep

# Community-level PrEP proportions using ARTnet values
d$prop.prep.city <- ifelse(d$city2 == "Atlanta", 0.23529,
                           ifelse(d$city2 == "Boston", 0.21795,
                                  ifelse(d$city2 == "Chicago", 0.29412,
                                         ifelse(d$city2 == "Dallas", 0.20238,
                                                ifelse(d$city2 == "Denver", 0.10909,
                                                       ifelse(d$city2 == "Detroit", 0.10870,
                                                              ifelse(d$city2 == "Houston", 0.23288,
                                                                     ifelse(d$city2 == "Los Angeles", 0.27273,
                                                                            ifelse(d$city2 == "Miami", 0.12727,
                                                                                   ifelse(d$city2 == "New York City", 0.26540,
                                                                                          ifelse(d$city2 == "Philadelphia", 0.10256,
                                                                                                 ifelse(d$city2 == "San Diego", 0.20408,
                                                                                                        ifelse(d$city2 == "San Francisco", 0.38889,
                                                                                                               ifelse(d$city2 == "Seattle", 0.33766,
                                                                                                                      ifelse(d$city2 == "Washington", 0.26316,
                                                                                                                             ifelse(d$city2 == "zOther1", 0.16092,
                                                                                                                                    ifelse(d$city2 == "zOther2", 0.17610,
                                                                                                                                           ifelse(d$city2 == "zOther3", 0.16549,
                                                                                                                                                  ifelse(d$city2 == "zOther4", 0.17112,
                                                                                                                                                         ifelse(d$city2 == "zOther5", 0.12129,
                                                                                                                                                                ifelse(d$city2 == "zOther6", 0.14394,
                                                                                                                                                                       ifelse(d$city2 == "zOther7", 0.16265,
                                                                                                                                                                              ifelse(d$city2 == "zOther8", 0.10619,
                                                                                                                                                                                     ifelse(d$city2 == "zOther9", 0.22430, 0))))))))))))))))))))))))
d$prep <- ifelse(d$artnetPREP_CURRENT == 0 | is.na(d$artnetPREP_CURRENT), 0, 1)

d <- d %>%
  # Limit analysis to HIV-negative and ever HIV tested
  filter(hiv2 == 0 & EVERTEST == 1) %>%
  # 3-level PrEP Categories: never, non-current, current
  mutate(
    prep_cat = case_when(
      PREP_REVISED == 0 ~ 0,
      artnetPREP_CURRENT == 0 ~ 1,
      artnetPREP_CURRENT == 1 ~ 2,
      TRUE ~ NA_real_
    )
  ) %>%
  # Limit analysis to MSM not missing Current/Non-Current PrEP use
  filter(!is.na(prep_cat))

# Degree
l <- l %>%
  mutate(
    ONGOING = as.numeric(ONGOING),
    ongoing2 = if_else(is.na(ONGOING), 0, ONGOING)
  )

d <- l %>%
  filter(
    RAI == 1 | IAI == 1,
    ptype == 1
  ) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.main = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

d <- l %>%
  filter(
    RAI == 1 | IAI == 1,
    ptype == 2
  ) %>%
  group_by(AMIS_ID) %>%
  summarise(deg.casl = sum(ongoing2)) %>%
  right_join(d, by = "AMIS_ID")

d <- d %>%
  mutate(
    # If missing degree, then set to 0
    across(c(deg.main, deg.casl), ~ if_else(is.na(.x), 0, .x)),
    deg.tot = deg.main + deg.casl,
    # if missing `artnetPREP_CURRENT` then 0
    prep = if_else(artnetPREP_CURRENT %in% 1, 1, 0)
  )

# Frequency of anal intercourse
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(num.acts = sum(anal.acts.week)) %>%
  right_join(d, by = "AMIS_ID")

# Frequency of condomless anal intercourse
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  group_by(AMIS_ID) %>%
  summarise(num.cp.acts = sum(anal.acts.week.cp)) %>%
  right_join(d, by = "AMIS_ID")

city_prep <- d %>%
  group_by(city2) %>%
  summarise(
    city_prep = mean(prep),
    city_pop = n(),
    deg.tot_mean = mean(deg.tot)
  ) %>%
  mutate(city_num = as.numeric(city2))

d$race_black <- ifelse(d$race.cat == "black", 1, 0)
d$race_white <- ifelse(d$race.cat == "white", 1, 0)
d$race_hisp <- ifelse(d$race.cat == "hispanic", 1, 0)
d$race_oth <- ifelse(d$race.cat == "other", 1, 0)

d <- right_join(d, city_prep, by = "city2")


# Modeling (Tables 4 and 5) -------------------------------------------------------------------------------------

# outcome = total degree ----------------------------------------------------------------------------------------

# total degree ~ individual prep
x <- dplyr::select(d, y = deg.tot, age, race_black, race_white, race_hisp, race_oth, prep)

# bivariate: outcome ~ individual prep
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + bp * prep,
    a ~ dnorm(0, 5),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_simple)
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome ~ individual prep, adjusted for age and race
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_simple)
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age and race
x <- select(d, y = deg.tot, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

# multilevel model with partial pooling for city_covs (for cities with fewer individuals, get a less biased posterior distribution)
m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_mix_multi)
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age, race, and individual prep
x <- select(d, y = deg.tot, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

# multilevel model with partial pooling for city_covs (for cities with fewer individuals, get a less biased posterior distribution)
m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_mix_multi)
str(rethinking::extract.samples(m_mix_multi))
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome = main degree ----------------------------------------------------------------------------------------

# outcome ~ individual prep
x <- select(d, y = deg.main, age, race_black, race_white, race_hisp, race_oth, prep)

# bivariate: outcome ~ individual prep
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + bp * prep,
    a ~ dnorm(0, 5),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_simple)
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome ~ individual prep, adjusted for age and race
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_simple)
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age and race
x <- select(d, y = deg.main, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

# multilevel model with partial pooling for city_covs (for cities with fewer individuals, get a less biased posterior distribution)
m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age, race, and individual prep
x <- select(d, y = deg.main, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_mix_multi)
str(rethinking::extract.samples(m_mix_multi))
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome = casual degree ----------------------------------------------------------------------------------------

# outcome ~ individual prep
x <- select(d, y = deg.casl, age, race_black, race_white, race_hisp, race_oth, prep)

# bivariate: outcome ~ individual prep
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + bp * prep,
    a ~ dnorm(0, 5),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
summary(m_simple)
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome ~ individual prep, adjusted for age and race
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age and race
x <- select(d, y = deg.casl, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

# multilevel model with partial pooling for city_covs (for cities with fewer individuals, get a less biased posterior distribution)
m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age, race, and individual prep
x <- select(d, y = deg.casl, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome = count of one time partners ----------------------------------------------------------------------------------------

# One-off partnerships
d <- l %>%
  filter(RAI == 1 | IAI == 1) %>%
  filter(ptype %in% 1:2) %>%
  group_by(AMIS_ID) %>%
  count() %>%
  rename(count.mc.part = n) %>%
  right_join(d, by = "AMIS_ID")
d$count.mc.part <- ifelse(is.na(d$count.mc.part), 0, d$count.mc.part)
d$count.oo.part <- d$ai.part - d$count.mc.part
d$count.oo.part <- pmax(0, d$count.oo.part)

d$count_oo <- d$count.oo.part
d <- d %>% 
  filter(!is.na(count_oo))

# outcome ~ individual prep
x <- select(d, y = count_oo, age, race_black, race_white, race_hisp, race_oth, prep)

# bivariate: outcome ~ individual prep
m_simple <- ulam(
  alist(
    y ~ dgampois(lambda, phi),        
    log(lambda) <- a + bp * prep,
    a ~ dnorm(0, 5),
    bp ~ dnorm(0, 2),
    phi ~ dnorm(0, 2) 
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome ~ individual prep, adjusted for age and race
m_simple <- ulam(
  alist(
    y ~ dgampois(lambda, phi), 
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    phi ~ dnorm(0, 2) 
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age and race (poisson)
x <- select(d, y = count_oo, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 200,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# multivariate - adjusted for age, race, and individual prep (poisson)
x <- select(d, y = count_oo, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 200,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df


# multivariate - adjusted for age and race (negative binomial)
x <- select(d, y = count_oo, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # negative binomial model of the outcome
    y ~ dgampois(lambda, phi),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1),
    phi ~ dnorm(0, 2) 
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age, race, and individual prep (negative binomial)
x <- select(d, y = count_oo, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # negative binomial model of the outcome
    y ~ dgampois(lambda, phi),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1),
    phi ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome = consistent condom usage -----------------------------------------------------------------------------------

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

d <- d %>% 
  filter(!is.na(prob.cond))


# outcome ~ individual prep
x <- select(d, y = prob.cond, age, race_black, race_white, race_hisp, race_oth, prep)

# bivariate: outcome ~ individual prep
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + bp * prep,
    a ~ dnorm(0, 5),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# outcome ~ individual prep, adjusted for age and race
m_simple <- ulam(
  alist(
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep,
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_simple, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_simple)
options(max.print=10000)
list <- list(post$bp)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age and race
x <- select(d, y = prob.cond, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

# multilevel model with partial pooling for city_covs (for cities with fewer individuals, get a less biased posterior distribution)
m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)


# multivariate - adjusted for age, race, and individual prep
x <- select(d, y = prob.cond, age, race_black, race_white, race_hisp, race_oth, prep, city_prep, city_num)

m_mix_multi <- ulam(
  alist(
    # poisson model of the outcome
    y ~ dpois(lambda),
    log(lambda) <- a + ba * age + bb * race_black + bh * race_hisp + bo * race_oth + bp * prep + bc * p,
    # priors for params
    a ~ dnorm(0, 5),
    ba ~ dnorm(0, 2),
    bb ~ dnorm(0, 2),
    bh ~ dnorm(0, 2),
    bo ~ dnorm(0, 2),
    bp ~ dnorm(0, 2),
    bc ~ dnorm(0, 2),
    # binomial model of individual PrEP (observed)
    prep ~ dbinom(1, p),
    logit(p) <- city_covs[city_num], # depends only on the city coverage (unobserved)
    city_covs[city_num] ~ dnorm(a_bar, sigma), # Prior for the city coverages
    a_bar ~ dnorm(-1.5, 1),
    sigma ~ dexp(1)
  ),
  data = x, chains = 4, cores = 4, iter = 5000,
)

df <- precis(m_mix_multi, depth = 2, prob=0.95)
df <- transform(df, emean = exp(mean)) %>%
  select(-c(sd, Rhat4, n_eff))
df

# to look at what % of simulation posteriors are > 1.0:
post <- extract.samples(m_mix_multi)
options(max.print=10000)
list <- list(post$bc)
df <- data.frame(list)
colnames(df) <- c('value')
df <- transform(df, evalue = exp(value))
df$one <- ifelse(df$evalue>1.00, 1, 0)
table(df$one)

# ----------------------------------------------------------------------------------------------------------