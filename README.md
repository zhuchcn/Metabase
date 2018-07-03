<!-- README.md is generated from README.Rmd. Please edit that file -->
Metabase
========

This is a R package that provides a solution to handle data from quantitative experiments such as metabolomics and proteomics. It uses R's S4 object oriented programing system, inspired by the bioconductor R package Biobase and phyloseq for sequencing data.

The virtual super class *mSet* is the base of this package. It has 4 slots, a *conc\_table* slot that contains the numeric concentration values for each sample and feature, a *sample\_table* slot that contains all the sample metadata, a *feature\_data* slot contains all the feature information from the experiment, and a "experiment\_data" slot that contains all the additional experiment information.

The mSet is a virtual class so it can not be constructed directly. There is currently one class inherits from the mSet, the MetabolomicsSet, that is designated for metabolomics dataset. The ProteomicsSet and GlycomicsSet are on the way.

                             .--(0)    (0)--.
                            (       \/       )
                            (     \____/     )
                            (                )
                            (     谢  谢     )
                            (                )
                             '--------------'
