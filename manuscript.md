---
title: 'Data.Adapt.Multi.Test: data-adaptive statistics for
        high-dimensional testing'
tags:
  - R
  - data-adaptive statistics
  - multiple testing
authors:
 - name: Wilson Cai
   orcid: 0000-0003-2680-3066
   affiliation: 1
 - name: Nima Hejazi
   orcid: 0000-0002-7127-2789
   affiliation: 1
affiliations:
 - name: Division of Biostatistics, University of California, Berkeley
   index: 1
date: 28 October 2016
bibliography: manuscript.bib
---

# Summary

This package contains an implementation of the data-adaptive statistical
approach to estimating effect sizes in high-dimensional settings while avoiding
many of the pitfalls common to multiple testing. To address the issue of
multiple testing in situations where the dimensionality is high but sample size
is comparatively small (i.e., as is common in genomics settings), we provide
here an implementation of data-adaptive multiple-testing procedures in the form
of a package for the R language and environment for statistical computing.

Data-adaptive test statistics for multiple testings, motivated by the broad use
of very large numbers of tests and the limitations of existing multiple testing
methods, utilize a recently developed data-adaptive technique for performing
variable reduction. This newly developed methodology is a natural extension of
the data-adaptive target parameter framework introduced by Hubbard _et al._
(2016), who demonstrated that these new inference procedures provide a way to
introduce more rigorous statistical inference into problems being increasingly
addressed by clever yet _ad hoc_ data-mining algorithms [@hubbard].

A fundamental assumption in settings where multiple testing is performed is
that of convergence to multivariate normality. That is, in the context of
small-sample inference, it is often assumed that the sampling distribution of
parameter estimates or test statistics converges to a multivariate normal
distribution, with multiple testing procedures often making heavy use of this
assumption. Such an assumption can be flawed however: convergence behavior in
the tail of joint multivariate normal distributions has not been well studied.

The approach of data-adaptive test statistics improves on current approaches to
multiple testing by applying a set of estimation algorithms (specified by the
user) across splits of a particular sample of data, allowing for paramteres of
interest to be discovered from the data. Such methods uncover associations that
are stable across the full sample and restrict multiple testing to a smaller
subset of covariates by allowing for variable importance to be measured via the
data-adaptive procedure. Test statistics formulated in this framework are
expected to both outperform pre-specified test statistics and provide improved
power as well as type-I error control, all while simultaneously allowing for
appropriate statistical inference to be performed.

The tools provided in this package are intended for use in high-dimensional settings...

-![Illustration of data-adaptive statistics.](data_adapt_1.png)

-![Illustration of data-adaptive statistics.](data_adapt_2.png)

\clearpage

# References
