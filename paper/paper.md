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
date: 20 June 2018
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
Error Rate (FWER) [@dunn1961multiple]. Such methods have been well studied in
the literature on multiple testing, and it was been well establish that, for a
fixed targeted effect size and fixed sample size, power decreases as the number
of tests and corresponding critical values increase [@lazzeroni2010cost].
Further, @lazzeroni2010cost show that if the power for a single test is 80\%,
the power is approximately 50\% for 10; 10\% for 1000; and 1\% for 100,000
Bonferroni-adjusted tests, a classic method to correct for Type-I error when
facing multiple testing issues. This simple example demonstrates that data
analysts and other practitioners must invest, at a prohibitively high rate,
additional resources to collect samples in order to obtain meaningful results
under high-dimensional multiple testing constraints.

Utilizing this recently developed data-adaptive statistical framework,
information loss induced by standard multiple testing procedures can be avoided
by reducing the dimensionality of problems via data-adaptive variable reduction.
This recent methodological advance, a data-adaptive multiple testing technique
[@cai2018data-adaptive], is a natural extension of the data-adaptive target
parameter framework introduced in @hubbard2016statistical and
@hubbard2016mining, which present a new class of inference procedures that
introduce more rigorous statistical inference into problems being increasingly
addressed by clever yet _ad hoc_ algorithms for data mining.

The approach of data-adaptive test statistics improves on current approaches to
multiple testing by applying a set of estimation algorithms (specified by the
user) across splits of a particular sample of data, allowing for parameters of
interest to be discovered from the data. Such methods uncover associations that
are stable across the full sample and restrict multiple testing to a smaller
subset of covariates by allowing for variable importance to be measured via the
data-adaptive procedure. Test statistics formulated in this framework are
expected to both outperform pre-specified test statistics and provide improved
power as well as Type I error control, all while simultaneously allowing for
appropriate statistical inference to be performed.

We illustrate the use of data-adaptive test statistics for parameter discovery
by considering a simulated data set with 100 observations in 1000 dimensions,
with a "true" signal constrained to just 10 covariates/dimensions. By applying
the approach discussed above, using cross-validation to rank features, we obtain
a ranking of the most important covariates -- that is, those dimensions most
closely associated with the "true" signal. A ranking of features across folds of
cross-validation is displayed below:

![Average Rank of Top Covariates: here, the top ten covariates have CV-rank
aligning linearly, indicating a stable ranking pattern.](figs/mean_rank.pdf)

From the plot displayed above, it is clear to see that there is a rather sharp
divide in the ranking of covariates associated with the "true" signal -- that
is, these are those covariates that consistently rank highly in the importance
measure employed across the many rounds of cross-validation performed. The plot
of p-values displayed below shows these same features with low p-values, with a
clearly strong divide consistent with that displayed in the previous plot:

![Adjusted P-values for the Reduced Set of Hypotheses](figs/adj_p_val.pdf)

The `adaptest` R package provides utilities for performing the estimation and
hypothesis testing procedures discussed above, and detailed in
@cai2018data-adaptive, alongside utilities for easily producing data
visualizations based on the results. The software introduces new classes and
methods, based on R's S4 class system, to facilitate its integration into the
Bioconductor ecosystem [@huber2015orchestrating], making it well-suited for
applications in computational biology, where high-dimensional data structures
very often arise. The R package includes documentation and detailed vignettes
that will allow for both (bio)statisticians and computational biologists to
easily make use of this new tool in such data analytic problem settings.

\newpage

# References

