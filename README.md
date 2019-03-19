<!-- README.md is generated from README.Rmd. Please edit that file -->
Metabase
========

Description
-----------

This is a R package that provides a solution to handle data from
quantitative experiments such as metabolomics and proteomics. It aims to
reduce the data analysis workload, simplify tedious and repeating steps
during data cleaning, transforming, summarizing and visualizing, and
help researchers to analyze and explore their data easily. It uses R’s
S4 object oriented programing system, inspired by the bioconductor R
package Biobase and phyloseq for sequencing data.

The virtual super class *mSet* is the base of this package. It has 4
slots, a *conc\_table* slot that contains the numeric concentration
values for each sample and feature, a *sample\_table* slot that contains
all the sample metadata, a *feature\_data* slot contains all the feature
information from the experiment, and a *experiment\_data* slot that
contains all the additional experiment information. The mSet is a
virtual class so it can not be constructed directly. There are several
classes that inherit from it, MetabolomicsSet, LipidomicsSet,
GlycomicsSet, MicrobiomeSet, and MultxSet. They do not have huge
differences in terms of the design.

Vignettes
---------

-   [Introduction to
    Metabase](https://zhuchcn.github.io/docs/packages/Metabase/intro/)
-   [Data input from WCMC
    report](https://zhuchcn.github.io/docs/packages/Metabase/lipidomics_wcmc/)

To do:
------

-   \[ \] The ExperimentData-class is currently too strict. The
    structure should be more flexible to handle different study type.
