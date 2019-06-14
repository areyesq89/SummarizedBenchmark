
# SummarizedBenchmark <img src="man/figures/sbhex.png" align="right" alt="" width="160"/>

[![Bioc Devel build
results](https://bioconductor.org/shields/build/devel/bioc/SummarizedBenchmark.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/SummarizedBenchmark/)
[![Whether the package is available on all
platforms.](https://bioconductor.org/shields/availability/3.8/SummarizedBenchmark.svg)](http://bioconductor.org/packages/devel/SummarizedBenchmark/)
[![How long since the package was first in a released Bioconductor
version (or is it in devel
only).](https://bioconductor.org/shields/years-in-bioc/SummarizedBenchmark.svg)](http://bioconductor.org/packages/devel/SummarizedBenchmark/)
[![time since last commit. possible values: today, \< 1 week, \< 1
month, \< 3 months, since release, before
release](https://bioconductor.org/shields/lastcommit/devel/bioc/SummarizedBenchmark.svg)](http://bioconductor.org/checkResults/devel/bioc-LATEST/SummarizedBenchmark/)

SummarizedBenchmark defines a flexible framework for benchmarking
computational methods in R. Classes and functions are provided for
defining, executing and evaluating benchmark experiments. The package
builds on the
[SummarizedExperiment](http://bioconductor.org/packages/SummarizedExperiment/)
class to keep results organized, with outputs tied directly with
important method metadata. This site is for the development version of
the package. Documentation and examples for the current Bioconductor
release version of the package can be found at the official [release
page](http://bioconductor.org/packages/SummarizedBenchmark/).

If you have any suggestions on how we can improve the package, [let us
know](https://github.com/areyesq89/SummarizedBenchmark/issues)\!

## Installation

``` r
# Install development version from Bioconductor
BiocManager::install("SummarizedBenchmark", version = "devel")

# Install release version from Bioconductor
BiocManager::install("SummarizedBenchmark")
```

## Usage

SummarizedBenchmark can be used to apply several computational methods
in R on a data set and to store and compare the subsequent results. This
can include just a single method with different parameter settings, or
methods written outside of R, but called using a system call from an R
session. Generally, the package should be useful if you are trying to
decide between several competing methods and would like to compare how
they perform on one or more data sets.

## Related Work

While we hope users find SummarizedBenchmark useful, it might not be
suitable for all problems or data sets. Other frameworks for
benchmarking methods include
[iCOBRA](http://bioconductor.org/packages/iCOBRA/) (a package for
comparing results of “binary classification and ranking methods” with a
Shiny web application for interactive analyses),
[rnaseqcomp](http://bioconductor.org/packages/rnaseqcomp) (a package for
comparing results of RNA-seq quantification pipelines), and
[dsc](https://github.com/stephenslab/dsc) (a framework for “managing
computational benchmarking experiments that compare several competing
methods” written in Python but capable of running methods implemented in
both Python and R).
