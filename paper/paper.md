---
title: 'adaptest: Data-Adaptive Statistics for High-Dimensional Testing in R'
tags:
  - R language
  - data-adaptive statistics
  - data mining
  - multiple testing
  - computational biology
  - bioinformatics
  - targeted learning
authors:
 - name: Weixin Cai
   orcid: 0000-0003-2680-3066
   affiliation: 1
 - name: Alan Hubbard
   orcid: 0000-0002-3769-0127
   affiliation: 1
 - name: Nima Hejazi
   orcid: 0000-0002-7127-2789
   affiliation: 1
affiliations:
 - name: Group in Biostatistics, University of California, Berkeley
   index: 1
date: 15 October 2018
bibliography: manuscript.bib
---

# Summary

The `adaptest` R package contains an implementation of a methodology based on
using _data-adaptive statistics_ for estimating effect sizes, complete with
appropriate inference, in high-dimensional settings while avoiding the
inferential burdens of multiple testing corrections. To address the issue of
multiple testing in situations where the dimensionality is high but sample size
comparatively small (_e.g._, analysis of RNA-seq data), we expose an
implementation of a method for statistical inference on data-adaptive target
parameters [@hubbard2016statistical] in the form of a software package for the
R language and environment for statistical computing [@R].

Data-adaptive test statistics for multiple testing are motivated by efforts to
address the limitations of existing multiple testing methods such as the
popular Benjamini-Hochberg procedure to control the False Discovery Rate (FDR)
[@benjamini1995controlling] or the Bonferroni method to control the Family-Wise
Error Rate (FWER) [@dunn1961multiple]. Such methods are well studied in the
literature on multiple testing, and it is well established that, for a fixed
targeted effect size and fixed sample size, power decreases as the number of
tests and corresponding critical values increase [@lazzeroni2010cost]. Further,
@lazzeroni2010cost show that if the power for a single test is 80\%, the power
is approximately 50\% for 10; 10\% for 1000; and 1\% for 100,000
Bonferroni-adjusted tests, a classic method to correct for Type-I error when
facing multiple testing issues. This simple example demonstrates that data
analysts and other practitioners must invest, at a prohibitively high rate,
additional resources to collect samples in order to obtain meaningful results
under high-dimensional multiple testing constraints.

Utilizing this recently developed data-adaptive statistical framework, our
method reduces information loss induced by standard multiple testing procedures
through data-adaptive dimensionality reduction. This recent methodological
advance, a data-adaptive multiple testing technique [@cai2018data-adaptive], is
a natural extension of the data-adaptive target parameter framework introduced
in @hubbard2016statistical and @hubbard2016mining, which present a new class of
inference procedures that introduce more rigorous statistical inference into
problems being increasingly addressed by smart yet _ad hoc_ algorithms for data
mining.

The approach of data-adaptive test statistics improves on current approaches to
multiple testing by applying a set of data-mining algorithms (specified by the
user) across splits of a particular sample of data, allowing for parameters of
interest to be discovered from the data. Such methods uncover associations that
are stable across the full sample and restrict multiple testing to a smaller
subset of covariates by allowing for variable importance to be measured via the
data-adaptive procedure. Test statistics are formulated on a separately
held-out subset of data and are expected to both outperform pre-specified test
statistics and provide improved power, all while simultaneously allowing for
appropriate statistical inference to be performed.

We illustrate how to apply the ``data-adaptive test statistics`` for multiple
testing by considering a simulated randomized trial with binary treatment and
1000 outcomes (e.g., biomarkers in the microarray analysis). The dataset size
is 100 observations. Of the 1000 outcomes (biomarkers), outcome 1 - 10 have
effect sizes equal to 0.6, while the treatment has no effect on outcomes 11 -
1000. After applying our ``data-adaptive test statistics`` method (using the
`adaptest` function in the R package), we obtain a rank order (regarding effect
size) for all outcomes across multiple cross-validation folds. We then average
the rank order across folds, sort in ascending order, which gives us Figure
\ref{avg_rank}. By looking at the top 15 outcomes in Figure \ref{avg_rank}, we
observe that there are two large jumps in average rank order of the top 15
outcomes: between outcome 9 and 4, and between outcome 3 and 2. These jumps
naturally divide the outcomes into tiers regarding importance. Outcome 9
consistently ranks highly in the importance measure employed across the many
rounds of cross-validation performed. In this example, we recommend
practitioner first to analyze outcome 9, and if data size allows, extend the
analysis to the group of outcome from 4 to 3, and so on. Figure \ref{q_value}
displays adjusted p-values of the same set of outcomes as in Figure
\ref{avg_rank}, with a group of outcomes (outcome 9 to outcome 3) with very
significant effect.

![Average rank order of outcomes regarding absolute estimated effect size across cross-validation folds (simulated data). The top outcomes are displayed after being sorted in ascending order. \label{avg_rank}](figs/mean_rank.pdf)

![Adjusted p-values (using the Benjamini-Hochberg procedure) of the same set of candidate outcomes, computed on a validation set that is mutually exclusive from the data used to compute the rank order in Figure \ref{avg_rank}. The top outcomes are displayed after being sorted in ascending order. \label{q_value}](figs/adj_p_val.pdf)

The `adaptest` R package provides utilities for performing the estimation and
hypothesis testing procedures discussed above, and detailed in
@cai2018data-adaptive, alongside utilities for easily producing data
visualizations based on the results. The software introduces new classes and
methods, based on R's S4 class system, to facilitate its integration into the
Bioconductor ecosystem [@huber2015orchestrating], making it well-suited for
applications in computational biology, where high-dimensional data structures
very often arise. The R package includes documentation and detailed vignettes
that will allow for both (bio)statisticians and computational biologists to
efficiently make use of this new tool in such data analytic problem settings.

\newpage

# References

