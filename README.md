# Mining the Automotive Industry – <br> A Network Analysis of Manufacturer Web Pages

Code and data accompanying the work to produce figures of the paper 'Mining the Automotive Industry – A Network Analysis of Manufacturer Web Pages' (**link to paper will follow**). 

## Description
**Last modified:** September 2019 <br>
**Authors:** Niklas Stoehr, Fabian Braesemann, Shi Zhou <br>
**Licence:** CC-BY (4.0)

**Abstract:** The digital transformation is driving revolutionary innovations and new market entrants threaten established sectors of the economy such as the automotive industry. Following the need for monitoring shifting industries, we present a network-centred analysis of car manufacturer web pages. Solely exploiting publicly-available information, we construct large networks from web pages and hyperlinks. The network properties disclose the internal corporate positioning of the three largest automotive manufacturers, Toyota, Volkswagen and Hyundai with respect to innovative trends and their international outlook. We tag web pages concerned with topics like e-mobility & environment or autonomous driving, and investigate their relevance in the network. Toyota and Hyundai are concerned with e-mobility throughout large parts of their web page network; Volkswagen devotes more specialized sections to it, but reveals a strong focus on autonomous driving. Sentiment analysis on individual web pages uncovers a relationship between page linking and use of positive language, particularly with respect to innovative trends. Web pages of the same country domain form clusters of different size in the network that reveal strong correlations with sales market orientation. Our approach is highly transparent, reproducible and data driven, and could be used to gain complementary insights into innovative strategies of firms and competitive landscapes. <br>
**Keywords:** Automotive Industry, Network Analysis, Complex Networks, Digitization, Web Page Mining, Competition


## Folder Structure and Disclaimer
Since some data has been crawled with the help of commercial tools, neither all data nor all code to reproduce the entire work is included. The provided code allows crawling textual components from web pages, analysing sentiment and tagging the following keywords:

**E-mobility & environment:** e-mobility, battery, environment, biological, eco, ecological, electric, hybrid, environment, environmental-friendly;<br> 
**Connectivity & shared mobility:** connectivity, shared, mobility, sharing, interconnectedness, cloud, cloud computing, wifi, 5G;<br> 
**Autonomous driving & artificial intelligence:** autonomous, self-driving, ai, machine learning, artificial intelligence, intelligent, neural network, algorithm.
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
stephany2020mining,
title={Mining Automotive the Industry – A Network Analysis of Manufacturer Web Pages},
author={Niklas Stoehr, Fabian Braesemann and Shi Zhou},
booktitle={under review at Complex Networks 2019},
year={2019},
url={www.github.com/Braesemann/MiningAutomotiveIndustry},
}
```




