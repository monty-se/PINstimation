# Scientific Research

### Overview

Below, we give a relatively detailed description of the original research articles
related to the package by authors. We report the abstract of each paper, a link to
download it, as well as the functions that have been implemented by on the research
in the paper. 
&nbsp;
We also report a list of other research papers underlying the different functions
implemented in the package.


### Original Research papers

The package ‘PINstimation’ implements original research by the
authors, which led to four research articles:

1.  The first article provides a comprehensive overview of the package: 
    it details the underlying theoretical background, provides
    a thorough description of the functions, before using them
    to tackle relevant research questions.

2.  The second article presents a methodological approach to the
    computational problems of the estimation of the adjusted probability of informed trading by Duarte and Young (2009).

3.  The third article presents a theoretical derivation, and an
    empirical investigation for the estimation of the PIN models 
    using the expectation-conditional maximization (ECM) algorithm.

4.  The fourth article develops an algorithm for detection of
    information layers in trade data, in line with the model of
    multilayer probability of informed trading of Ersan (2016).
&nbsp;


### PINstimation: An R package for estimating models of probability of informed trading

------------------------------------------------------------------------

#### **Abstract**

The purpose of this paper is to introduce the R package PINstimation. The package is designed for estimating, in a precise and fast way, the probability of informed trading models through the implementation of the main estimation methods suggested in the literature so far. The models covered are the original PIN model of Easley and O'Hara (1992), and Easley et al. (1996); the multilayer PIN model of Ersan (2016); the adjusted PIN model of Duarte and Young (2009); and the volume-synchronized PIN of Easley, De Prado, and O’Hara (2011), and Easley, López De Prado, and O’Hara (2012). These core functionalities of the package are supplemented with utilities for data simulation, aggregation and classification tools. In addition to a detailed overview of the package functions, their arguments, and their outputs, we have included a theoretical review of the methods behind these functions while presenting the major challenges and related solutions. Finally, we conduct two applications with trade-level data for 58 Swedish stocks, and report straightforward, comparative and intriguing findings on informed trading. These applications aim to highlight the capabilities of the package in tackling relevant research questions and provide instances of the wide possibilities of use of PINstimation for both academicians and practitioners.

#### **Download**

-   Data: The trade-level data for 58 Swedish stocks used in the paper is available for
    download [here](data/swdata.RDS).

-   Article: The article is available on <a href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4117946" target="_blank">SSRN</a>.

&nbsp;

### A methodological approach to the computational problems in the estimation of adjusted PIN model

------------------------------------------------------------------------

####  **Abstract**

It is well documented that computational problems may lead to large biases in the estimation of probability of informed trading (PIN) models. While effective remedial solutions have been suggested for the case of original PIN model (Easley et al., 1996), computational problems for its most broadly applied extension, the adjusted PIN model of Duarte and Young (2009) , are yet to be addressed. Given its larger parameter set, estimates of the AdjPIN model are more likely to suffer from computational problems. We address these computational problems by developing an estimation method comprising a) a logarithmic factorization of the likelihood function, and b) an algorithm to strategically generate initial parameter sets. We show that the introduced method outperforms existing best-practices and strongly suggest its use in further studies.

####  **Functions**

The paper derives a factorization of the likelihood function of the AdjPIN model, as well as proposes a strategic, and efficient algorithm to derive initial parameter sets that prime the maximum likelihood estimation of the model, either using the standard methods, or the expectation-conditional maximization algorithm. The function that implements the factorization of the likelihood function is `fact_adjpin()`; while the function that implements the algorithm of initial parameter sets is `initials_adjpin()`.

####  **Download**

The article is available on <a href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4117954" target="_blank">SSRN</a>.

&nbsp;


### Estimation of the Probability of Informed Trading Models Via an Expectation-Conditional Maximization Algorithm

------------------------------------------------------------------------

####  **Abstract**

The PIN model and its extensions have proven challenging in their estimation, as they suffer 
from several computational problems. We set in this paper to address these computational issues
by proposing the use of the expectation-conditional maximization (ECM) algorithm to estimate the
various models of probability of informed trading. In particular, we derive optimal
estimates of two of the extensions of the original PIN model, which are the MPIN model
as introduced by Ersan (2016), and the adjusted PIN of Duarte and Young (2009)), as well as its
restricted variants. The derivation provides a reliable and mathematically sound method for the
estimation of the number of information layers for the MPIN model, as well as, stable estimates
for the adjusted PIN model despite the large number of free variables. We show that the maximum
likelihood estimation via the ECM algorithm is faster, and more reliable, and provides a
viable alternative to the standard methods used in the literature. In addition to providing more
accurate estimates of probability parameters, the ECM algorithm allows for an endogenous 
determination of the number of layers in the MPIN model. This paper has served as the basis
of the implementation of the ECM estimation in the R package dedicated to the estimation of
probability of informed trading models: PINstimation.

####  **Functions**

The paper details the theoretical adaptation of the ECM algorithm to estimate the MPIN model
of Ersan (2016), and the adjusted PIN model of Duarte and Young (2009). The functions of the 
package that implements the estimation of the MPIN model is `mpin_ecm()`; while the one 
implementing the estimation of the AdjPIN model is `adjpin()`.

####  **Download**

The article is available on <a href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4117952" target="_blank">SSRN</a>.

&nbsp;

### Identifying information types in probability of informed trading (PIN) models: An improved algorithm

------------------------------------------------------------------------

####  **Abstract**

The multilayer probability of informed trading (MPIN) model, developed by Ersan (2016), releases the assumption of single type of information events in the original PIN model of Easley et al. (1996). Identification of the number of layers in a dataset is applied through a layer detection algorithm suggested in Ersan (2016). The algorithm is based on clustering absolute order imbalances and examination of confidence intervals for the skellam distribution. When uninformed trading intensity is assumed to be identical in the buy and sell sides, the algorithm performs extremely well. When uninformed intensities are not equal, Ersan (2016) suggests the adjustment of the data using a correction term, proxied by the minimum levels of buys and sells in the data. We improve the algorithm of Ersan (2016) in two ways. We provide accurate estimates of uninformed trading intensities used for data adjustment, and slightly modify the algorithm of determining the information layers. The improved algorithm identifies the number of layers with substantially increased precision, between 86% and 95% accuracy for the simulated data with various settings.

####  **Functions**

The paper develops an algorithm to detect the number of layers in the trade dataset, inline with the MPIN model of Ersan (2016). The function implementing this algorithm is `detectlayers_eg()`, which constitutes an improvement of a previous algorithm developed by Ersan (2016), that is implemented in `detectlayers_e()`.

####  **Download**

The article is available on <a href="https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4117956" target="_blank">SSRN</a>.

### Other research papers

- Cheng T, Lai H (2021). “Improvements in estimating the probability of informed trading models.”
Quantitative Finance, 21(5), 771-796.
- Duarte J, Young L (2009). “Why is PIN priced?” Journal of Financial Economics, 91(2), 119–138. ISSN 0304405X.
- Easley D, De Prado MML, Ohara M (2011). “The microstructure of the \"flash crash\": flow toxicity,
liquidity crashes, and the probability of informed trading.” The Journal of Portfolio Management,
37(2), 118–128. 
- Easley D, Hvidkjaer S, Ohara M (2010). “Factoring information into returns.” Journal of Financial
and Quantitative Analysis, 45(2), 293–309. ISSN 00221090.
- Easley D, Kiefer NM, Ohara M, Paperman JB (1996). “Liquidity, information, and infrequently
traded stocks.” Journal of Finance, 51(4), 1405–1436. ISSN 00221082.
- Easley D, Lopez De Prado MM, OHara M (2012). “Flow toxicity and liquidity in a high-frequency
world.” Review of Financial Studies, 25(5), 1457–1493. ISSN 08939454.
Easley D, Ohara M (1992). “Time and the Process of Security Price Adjustment.” The Journal
of Finance, 47(2), 577–605. ISSN 15406261.
- Ellis K, Michaely R, Ohara M (2000). “The Accuracy of Trade Classification Rules: Evidence
from Nasdaq.” The Journal of Financial and Quantitative Analysis, 35(4), 529–551.
- Ersan O (2016). “Multilayer Probability of Informed Trading.” Available at SSRN 2874420.
- Ersan O, Alici A (2016). “An unbiased computation methodology for estimating the probability
of informed trading (PIN).” Journal of International Financial Markets, Institutions and Money,
43, 74–94. ISSN 10424431.
- Ersan O, Ghachem M (2022a). “Identifying information types in probability of informed trading
(PIN) models: An improved algorithm.” Available at SSRN 4117956.
- Ersan O, Ghachem M (2022b). “A methodological approach to the computational problems in
the estimation of adjusted PIN model.” Available at SSRN 4117954.
- Gan Q, Wei WC, Johnstone D (2015). “A faster estimation method for the probability of informed
trading using hierarchical agglomerative clustering.” Quantitative Finance, 15(11), 1805–1821.
- Ghachem M, Ersan O (2022a). “Estimation of the probability of informed trading models via an
expectation-conditional maximization algorithm.” Available at SSRN 4117952.
- Ghachem M, Ersan O (2022b). “PINstimation: An R package for estimating models of probability
of informed trading.” Available at SSRN 4117946.
- Lee CMC, Ready MJ (1991). “Inferring Trade Direction from Intraday Data.” The Journal of
Finance, 46(2), 733–746. ISSN 00221082, 15406261.
- Lin H, KeW(2011). “A computing bias in estimating the probability of informed trading.” Journal
of Financial Markets, 14(4), 625-640. ISSN 1386-4181.
- Yan Y, Zhang S (2012). “An improved estimation method and empirical properties of the probability
of informed trading.” Journal of Banking and Finance, 36(2), 454–467. ISSN 03784266.
