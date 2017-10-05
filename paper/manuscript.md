---
title: 'data.adapt.multi.test: Data-Adaptive Statistics for
        High-Dimensional Testing'
tags:
  - R language
  - data-adaptive statistics
  - data mining
  - multiple testing
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
 - name: Division of Biostatistics, University of California, Berkeley
   index: 1
date: 16 November 2016
bibliography: manuscript.bib
---

# Summary

This package contains an implementation of the data-adaptive statistical
approach to estimating effect sizes in high-dimensional settings. To address
the issue of multiple testing in situations where the dimensionality is high
but sample size is comparatively small (_e.g._, genomics problems), we provide
here an implementation of data-adaptive multiple-testing procedures in the form
of a package for the R language for statistical computing [@R].

Data-adaptive test statistics for multiple testing are motivated by efforts to
address the limitations of existing multiple testing methods such as the
popular Benjamini-Hochberg procedure to control the False Discovery Rate (FDR)
[@benjamini1995controlling] or Bonferroni method to control the Family-Wise Error Rate (FWER) [@dunn1961multiple]. It has been well studied in literature that for a fixed targeted effect size and fixed sample size, power decreases as the number of tests and corresponding critical value increase [@lazzeroni2010cost]. @lazzeroni2010cost shows that if the power for a single test is 80\%, the power is approximately 50\% for 10; 10\% for 1000; and 1\% for 100,000 Bonferroni-adjusted tests, a classic method to correct for Type-I error when doing multiple testing.  This means that practitioners need to invest in prohibitively more resources to collect samples in order to get meaningful results under high-dimensional multiple testing.

Utilizing this recently developed data-adaptive
statistical framework, information loss induced by standard multiple testing
procedures can be avoided by reducing the dimensionality of problems via
variable reduction. This newly developed methodology is a natural extension of
the data-adaptive target parameter framework introduced in
@hubbard2016statistical and @hubbard2016mining, which present a new class of
inference procedures that provide a way to introduce more rigorous statistical
inference into problems being increasingly addressed by clever yet _ad hoc_
algorithms for data mining.

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

\newpage

# References
