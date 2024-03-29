---
title: 'spINAR: An R Package for Semiparametric and Parametric Estimation and Bootstrapping of Integer-Valued Autoregressive (INAR) Models'
tags:
- R
- count data
- time series
- simulation
- semiparametric estimation
- parametric estimation
- penalization
- validation
- bootstrapping
authors:
  - name: Maxime Faymonville
    affiliation: 1
  - name: Javiera Riffo
    affiliation: 1
  - name: Jonas Rieger
    affiliation: 1
  - name: Carsten Jentsch
    affiliation: 1
    
affiliations:
 - name: TU Dortmund University
   index: 1
citation_author: Faymonville et. al.
date: \today
year: 2023
bibliography: paper.bib
output: rticles::joss_article
csl: apa.csl
journal: JOSS
---

# Summary

While the statistical literature on continuous-valued time series processes is vast and the toolbox for parametric, non-parametric and semiparametric approaches is methodologically sound, the literature on count data time series is considerably less developed. Such count data time series models are usually categorized in parameter-driven and observation-driven models. Among the observation-driven approaches, the integer-valued autoregressive (INAR) models that rely on the famous binomial thinning operation due to @steutel are arguably the most popular ones. They have a simple intuitive and easy interpretable structure and have been widely applied in practice [@dissweiss]. In particular, the INAR($p$) model can be seen as the discrete analogue of the well-known AR($p$) model for continuous-valued time series. The INAR(1) model was first introduced by @alosh and @mck, and its extension to the INAR($p$) model by Du and Li (1991) is defined according to $$X_t = \alpha_1 \circ X_{t-1} + \alpha_2 \circ X_{t-2} + \ldots + \alpha_p \circ X_{t-p} + \varepsilon_t, $$ with  $\varepsilon_t \overset{\text{i.i.d.}}{\sim} G$, where the innovation distribution $G$ has range $\mathbb{N}_0=\{0,1,2, \ldots\}$. The vector of INAR coefficients $\alpha = (\alpha_1, \ldots, \alpha_p)' \in (0,1)^p$ fulfills $\sum_{i=1}^p \alpha_i < 1$ and $$\alpha_i \circ X_{t-i} = \sum\limits_{j=1}^{X_{t-i}} Z_j^{(t,i)}, \, Z_j^{(t,i)} \sim \text{Bin}(1, \alpha_i), $$ where "$\circ$" denotes the binomial thinning operator first introduced by @steutel. Although many contributions have been made during the last decades, most of the literature focuses on parametric INAR models and estimation techniques. We want to emphasize the efficient semiparametric estimation of INAR models [@drost].

# Statement of need

INAR models find applications in a wide variety of fields such as medical sciences, environmentology and economics. For example, @franke_med model epileptic seizure counts using an INAR(1) model, @thy_rain use integer-valued autoregressive models to model the dynamics of rainfall and @mc_wage to analyze wage loss claims data. They all have in common assuming that the innovation distribution belongs to a parametric class of distributions. Non- or semiparametric estimation of the INAR model was not considered until @drost came up with their semiparametric estimation approach. A possible explanation is the complexity of the semiparametric setup since despite in the AR case the estimation in the INAR case cannot be based on the residuals: Even if the autoregressive coefficients were known, observing the data does not imply observing the innovations [@drost]. Nonetheless, one big advantage of semiparametric estimation is that we do not need to make a parametric distribution assumption on the innovations. The Poisson assumption is, for example, the most frequently used assumption for innovations and is characterized by equidispersion. In most cases, however, the data shows a higher variance than the mean value. The question arises when the distance between these two moments is large enough to not rather assume overdispersion, which would probably lead to assume negative binomially or geometrically distributed innovations. Furthermore, when dealing with low counts, we often observe many zeros in the data. This could be a sign for a zero-inflated innovation distribution such as the zero-inflated Poisson distribution [@jazi]. However, it is unclear at what point the zero is represented frequently enough in the data set to justify such an assumption. The mentioned points indicate that the assumption of an appropriate innovation distribution is often critical, bearing in mind that an incorrect assumption can lead to poor estimation performance. With semiparametric estimation, we do not have to commit to an innovation distribution, which makes this approach appealing. 

To deal with count data time series, [`R`](https://www.r-project.org/) [@rcoreteam] provides the package [`tscount`](https://cran.r-project.org/web/packages/tscount/) [@tscount] which, a.o., includes likelihood-based estimation of parameter-driven count data time series models which do not include INAR models and exclusively allows for conditional Poisson or negative binomially distributed data. The R package [`ZINARp`](https://CRAN.R-project.org/package=ZINARp) [@zinarp] allows to simulate and estimate INAR data by using MCMC algorithms for estimation but the package is limited to parametric estimation of INAR models, that is, of the INAR coefficients and of a parametrically specified innovation distribution $\{G_\theta \,  | \, \theta \in \mathbb{R}^q, \, q \in \mathbb{N}\}$ where they only cover the cases of Poisson or zero-inflated Poisson distributed innovations.The [`Julia`](https://julialang.org/) [@julia] package [`CountTimeSeries`](https://zenodo.org/record/7488440#.Y9ky9ISZNaQ) [@manuel] deals with integer counterparts of ARMA and GARCH models and some generalizations including the INAR model. It covers the parametric estimation setup for INAR models but does also not allow for non-parametric estimation of the innovation distribution. Such a semiparametric estimation technique that still relies on the binomial thinning operation, but comes along without any parametric specification of the innovation distribution was proposed and proven to be efficient by @drost. Also neither of the three packages contains procedures for bootstrapping INAR models within these parametric and semiparametric setups. The [`R`](https://www.r-project.org/) package [`spINAR`](https://github.com/MFaymon/spINAR) fills this gap and combines simulation, estimation and bootstrapping of INAR models in a single package. Both, the estimation and the bootstrapping, are implemented semiparametrically and also parametrically. The package covers INAR models of order $p \in \{1,2\}$, which are mainly used in applications.

# Features

For the simulation of INAR data, our package allows for flexible innovation distributions that can be inserted in form of a parametric probability mass function (pmf) or by simply passing a user-defined vector as pmf argument. Regarding the estimation, it allows for moment- and maximum likelihood-based parametric estimation of INAR models with Poisson, geometrically or negative binomially distributed innovations (see for example @bookweiss for details), but the main contribution lies in the semiparametric maximum likelihood estimation of INAR models introduced by @drost which they proved to be efficient. Additionally, a finite sample refinement for the semiparametric setup consisting of an estimation approach, that penalizes the roughness of the innovation distribution as well as a validation function for the penalization parameters is implemented [@faym]. Furthermore, the package includes the possibility to bootstrap INAR data. Again, the user is able to choose the parametric or the more flexible semiparametric model specification and to perform the (semi)parametric INAR bootstrap described in @jewe. 

# Acknowledgements

This research was funded by the Deutsche Forschungsgemeinschaft (DFG, German Research Foundation) - Project number 437270842.

# References
