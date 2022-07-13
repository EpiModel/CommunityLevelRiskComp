# PrEPCommunityLevelBehaviors

This repository contains the code for our study, "Correlations Between Community-Level HIV Preexposure Prophylaxis Coverage and Individual-Level Sexual Behaviors among US Men Who Have Sex with Men." This study examined the association between community-level PrEP use and seven behavioral risk indicators to determine whether behavioral change at a community-level is occurring independent of individual-level PrEP-related behavioral changes among US MSM.

## Citation
> TBD

# Abstract

## Objective
To evaluate if community-level HIV PrEP coverage is correlated with individual sexual behaviors.

## Design
We used demographic, behavioral, and sexual network data from ARTnet, a 2017–2019 study of US MSM.

## Methods
Multivariable regression models with a Bayesian modeling framework were used to estimate associations between area-level PrEP coverage and seven sexual behavior outcomes (number of total, main, and casual male partners [network degree]; count of one-time partnerships; consistent condom use in one-time partnerships; and frequency of casual partnership anal sex (total and condomless)), controlling for individual PrEP use.

## Results
PrEP coverage ranged from 10.3% (Philadelphia) to 38.9% (San Francisco). Total degree was highest in Miami (1.35) and lowest in Denver (0.78), while the count of one-time partners was highest in San Francisco (11.7/year) and lowest in Detroit (1.5/year). Adjusting for individual PrEP use and demographics, community PrEP coverage correlated with total degree (aIRR=1.73; 95% CrI, 0.92–3.44), casual degree (aIRR=2.05; 95% CrI, 0.90–5.07), and count of one-time partnerships (aIRR=1.90; 95% CrI, 0.46–8.54). Without adjustment for individual PrEP use, these associations strengthened. There were weaker or no associations with consistent condom use in one-time partnerships (aIRR=1.68; 95% CrI, 0.86–3.35), main degree (aIRR=1.21; 95% CrI, 0.48–3.20), and frequency of casual partnership condomless anal sex (aIRR=0.23; 95% CrI, 0.01–3.60).

## Conclusion
Most correlations between community PrEP coverage and sexual behavior were explained by individual PrEP use. However, some residual associations remained after controlling for individual PrEP use, suggesting that PrEP coverage may partially drive community-level differences in sexual behaviors.


# Data
We used data from ARTnet, a cross-sectional online survey of study of US MSM conducted during 2017–2019. Additional documentation on ARTnet and information to accessing the data can be found at https://github.com/EpiModel/ARTnetData. Code to install the `ARTnetData` package can be found below, but it may require a Github Personal Access Token (https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) since it is a private repository.

```
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

# Code Organization
The cleaning script `cleaning.R` is the script used to prepare the ARTnet data for analysis. The analysis script `analysis.R` is the script used for analysis and table creation.
