# The CoRisk-Index: <br> Mining industry-specific risks related to COVID-19 in real time

Code and data accompanying the work to produce figures of the paper 'CoRisk-Index: Mining industry-specific risks related to COVID-19 in real time'.

### The CoRisk-Index online:
http://oxford.berlin/corisk

### Working paper:
https://arxiv.org/abs/2003.12432

## Description
**Last modified:** 18 June 2020 <br>
**Authors:** Fabian Stephany, Leonie Neuhäuser, Niklas Stoehr, Philipp Darius, Ole Teutloff, Fabian Braesemann<br>
**Licence:** CC-BY (4.0)

**Abstract:** The spread of the coronavirus has caused a global economic disruption. Travel bans, supply chain failures, and store closures pose significant risks to entire industries. Business expectations are constantly changing in this dynamic situation and timely information on industry outlooks is highly valuable for economic analysis and targeted policy intervention. We propose the CoRisk Index as the result of a data-mining approach to measure industry-specific risk assessments in real-time. It is the first economic indicator measuring business risks related to Covid-19, distinguishing risk assessments across industries and topics. Our measures are highly correlated with U.S. unemployment data and preempt stock market losses of February 2020. The CoRisk data supports economists and policymakers in effectively estimating and, ultimately, mitigating the economic shocks of the crisis. <br>
**Keywords:** COVID-19, Coronavirus 2, Economic risk, Risk reports, SEC filings, Data mining, Natural language processing, Social data science


## Folder Structure
In this repository, we provide all the data and code to replicate the CoRisk-Index (the online dashboard) and the figures presented in the main part of the paper.

<br>

```
.
+-- code
|
|   +-- Paper_Figures.R (R code to produce all figures from the main paper; needs Paper_Figures_Data.zip)
|   +-- SIC to NAICS.R (R code to merge the SEC 1987 SIC classification with NAICS 2017 sectors)
|   +-- Supplement_Figures.R (R code to generate figures from the paper's supplementary materials)
|
|   +-- CoRisk-Index-Code
    |   +-- Scrape 10-X Report Sentences.ipynb (Python script to collect SEC data from EDGAR)
    |   +-- process 10k summaries_GH.R (R code to calculate main CoRisk statistics)
    |   +-- app.R (R code to produce dashboard)
|
+-- data
|   +-- Paper_Figures_Data.zip (Data to reproduce the main figures of the paper; with Paper_Figures.R)
|   +-- Supplement_Figures_Data.zip (Data to generate supplement figures;
                                     Additional data for S9 and S12 needed: explained in R Code)
|
|   +-- CoRisk-Index-Data
    |   +-- CoRisk-Index-Data.zip (Data needed to run the scripts in CoRisk-Index-Code directory)
    |   +-- Raw data (report level).zip (Raw data [output of process 10k summaries_GH.R]
                      on the level of individual reports; allows for more granular analyses) 
|
|   +-- SICtoNAICS
    |   +-- 1987_SIC_to_2002_NAICS.xls (Merging table 1987 SIC to 2002 NAICS, from US Census Bureau)
    |   +-- 2002_to_2007_NAICS.xls (Merging table 2002 NAICS to 2007 NAICS, from US Census Bureau)
    |   +-- 2007_to_2012_NAICS.xls (Merging table 2007 NAICS to 2012 NAICS, from US Census Bureau)
    |   +-- 2012_to_2017_NAICS.xlsx (Merging table 2012 NAICS to 2017 NAICS, from US Census Bureau)
|
+-- CoRisk_Detailed_Documentation.pdf (Detailed documentation to replicate the CoRisk-Index;
                                       Robustness checks, validations)
+-- readme.md
```

## Reference
Please cite as follows 

```
@article{
stephany2020corisk,
title={The CoRisk-Index: Mining industry-specific risks related to COVID-19 in real time},
author={Stephany, Fabian and Neuhäuser, Leonie and Stoehr, Niklas and Darius, Philipp and Teutloff, Ole and Braesemann, Fabian},
year={2020},
url={https://arxiv.org/abs/2003.12432},
}
```




