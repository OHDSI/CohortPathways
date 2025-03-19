CohortPathways
================


[![Build Status](https://github.com/OHDSI/CohortPathways/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/CohortPathways/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/CohortPathways/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/CohortPathways?branch=main)

Introduction
============

Cohort Pathway is defined as the process of generating an aggregated sequence of transitions between the Event Cohorts among those people in the Target Cohorts. CohortPathways is a R package that given a set of instantiated target and event cohorts, calculates such sequence of transitions.

Features
========
- Calculate aggregate sequence of transitions between the Event Cohorts among those people in the Target Cohorts.

Acknowledgements
============

The core implementation of CohortPathway analysis was developed by Christopher Knoll (github: chrisknoll), and enhanced by members of the OHDSI community: mick-iqvia (github), Odysseus Inc., and many other collaborators on the OHDSI/WebAPI team.


Technology
============
CohortPathways is an R package.

System Requirements
============
Requires R (version 3.6.0 or higher). 

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install CohortPathways:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/CohortPathways")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://github.com/OHDSI/CohortPathways).

PDF versions of the documentation are also available:
* Package manual: [CohortPathways.pdf](https://raw.githubusercontent.com/OHDSI/CohortPathways/main/extras/CohortPathways.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/CohortPathways/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
CohortPathways is licensed under Apache License 2.0

Development
===========
CohortPathways is being developed in R Studio.

### Development status

CohortPathways is under development.
