# The CoRisk-Index: <br> Mining industry-specific risks related to COVID-19 in real time

Code and data accompanying the work to produce figures of the paper 'CoRisk-Index: Mining industry-specific risks related to COVID-19 in real time'.

### The CoRisk-Index online:
http://oxford.berlin/corisk

### Working paper:
https://arxiv.org/abs/2003.12432

## Description
**Last modified:** 05 June 2020 <br>
**Authors:** Fabian Stephany, Leonie Neuhäuser, Niklas Stoehr, Philipp Darius, Ole Teutloff, Fabian Braesemann<br>
**Licence:** CC-BY (4.0)

**Abstract:** The spread of the coronavirus has caused a global economic disruption. Travel bans, supply chain failures, and store closures pose significant risks to entire industries. Business expectations are constantly changing in this dynamic situation and timely information on industry outlooks is highly valuable for economic analysis and targeted policy intervention. We propose the CoRisk Index as the result of a data-mining approach to measure industry-specific risk assessments in real-time. It is the first economic indicator measuring business risks related to Covid-19, distinguishing risk assessments across industries and topics. Our measures are highly correlated with U.S. unemployment data and preempt stock market losses of February 2020. The CoRisk data supports economists and policymakers in effectively estimating and, ultimately, mitigating the economic shocks of the crisis. <br>
**Keywords:** COVID-19, Coronavirus 2, Economic risk, Risk reports, SEC filings, Data mining, Natural language processing, Social data science


## Folder Structure and Disclaimer
Bla bla bla

<br> <br> 

```
.
+-- code
|   +-- Crawling
    |   +-- 1_Crawler.ipynb (code for crawling web pages)
    |   +-- 2_CleanUpEdges.ipynb (cleaning the crawled network data)
    +-- Analysis
    |   +-- 3_ContentAnalysis.ipynb (code for analysing web page content after cleaned up)
    |   +-- 4_NetworkAnalysisPlots.ipynb (visualizing and analysing networks)
    |   +-- 5_AutomotiveFigures.R (code for reproducing figures)
+-- data
|   +-- car_sales.csv (data of global car sales)
|   +-- node_data.csv (tagged web page data including the features "sentiment", "digital trend"...)
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




