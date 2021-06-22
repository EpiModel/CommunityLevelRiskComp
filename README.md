# CommunityLevelRiskComp

This repository contains the code for our analysis examining variable HIV PrEP coverage and community-level behavioral differences. This study examined the association between community-level PrEP use and five behavioral risk indicators to determine whether behavioral change at a community-level is occurring independent of individual-level PrEP-related behavioral changes among US MSM.


# Abstract

## Background
HIV preexposure prophylaxis (PrEP) is associated with higher sexual risk behaviors after PrEP initiation (referred to as “risk compensation”), particularly among men who have sex with men (MSM). However, behavioral differences may also emerge among PrEP non-users in communities with high PrEP coverage.

## Methods
We used demographic, behavioral, and sexual network data from ARTnet, a cross-sectional study of US MSM conducted during 2017–2019. Multivariable regression models with a Bayesian modeling framework in which individuals were nested within their residential geographic areas were used to estimate associations between area-level PrEP coverage and five sexual behavior outcomes (number of total, main, and casual male partners (network degree); count of one-time partnerships; and consistent condom use in one-time partnerships), controlling for individual PrEP use.

## Findings
PrEP coverage ranged from 10.3% (Philadelphia) to 38.9% (San Francisco). Total degree was highest in Miami (1.35) and lowest in Denver (0.78), while the count of one-time partners was highest in San Francisco (11.7/year) and lowest in Detroit (1.5/year). Adjusting for individual PrEP use and demographics, community PrEP coverage was associated with higher total degree (adjusted incidence rate ratio [aIRR]=1.73; 95% CrI, 0.92–3.44), casual degree (aIRR=2.05; 95% CrI, 0.90–5.07), and count of one-time partnerships (aIRR=1.90; 95% CrI, 0.46–8.54). Without adjustment for individual PrEP use, these associations were amplified. There were weaker associations with main degree (aIRR=1.21; 95% CrI, 0.48–3.20) and consistent condom use in one-time partnerships (aIRR=1.68; 95% CrI, 0.86–3.35).

## Interpretation
Most of the associations between community PrEP coverage and sexual risk were explained by individual PrEP use. However, there were residual associations after controlling for individual PrEP use, providing evidence of community-level behavioral differences driven by PrEP coverage.


# Data
We used data from ARTnet, a cross-sectional online survey of study of US MSM conducted during 2017–2019. Additional documentation on ARTnet and information to accessing the data can be found at https://github.com/EpiModel/ARTnetData. Code to install the `ARTnetData` package can be found below, but it may require a Github Personal Access Token (https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) since it is a private repository.

```
install.packages("remotes")
remotes::install_github("EpiModel/ARTnetData")
```

# Code Organization
The cleaning script `cleaning.R` is the script used to prepare the ARTnet data for analysis. The analysis script `analysis.R` is the script used for analysis.
