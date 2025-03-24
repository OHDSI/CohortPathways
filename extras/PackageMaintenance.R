# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of CohortPathways
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Format and check code ---------------------------------------------------
styler::style_pkg()
OhdsiRTools::checkUsagePackage("CohortPathways")
OhdsiRTools::updateCopyrightYearFolder()

# Devtools check -----------------------------------------------------------
devtools::spell_check()
devtools::check()

# Create manual -----------------------------------------------------------
unlink("extras/CohortPathways.pdf")
shell("R CMD Rd2pdf ./ --output=extras/CohortPathways.pdf")

# Create Vignettes---------------------------------------------------------
dir.create(file.path("inst","doc"), showWarnings = FALSE)
rmarkdown::render("vignettes/IntroductionToCohortPathways.Rmd",
                  output_file = "../inst/doc/IntroductionToCohortPathways.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

dir.create(file.path("inst","doc"), showWarnings = FALSE)
rmarkdown::render("vignettes/VisualizingPathwaysWithSunburstPlots.Rmd",
                  output_file = "../inst/doc/VisualizingPathwaysWithSunburstPlots.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

dir.create(file.path("inst","doc"), showWarnings = FALSE)
rmarkdown::render("vignettes/VisualizingPathwaysWithSankeyPlots.Rmd",
                  output_file = "../inst/doc/VisualizingPathwaysWithSankeyPlots.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

# Build site---------------------------------------------------------
pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Release package to CRAN ------------------------------------------------------
devtools::release()
devtools::check(cran=TRUE)

