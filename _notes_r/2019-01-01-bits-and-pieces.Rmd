---
title: "Bits and pieces"
date: 2019-01-01
tags: [statistics, r, miscellany]
---

The entry collects short notes, quotes, and links.

📌 rlang provides `done()` to break out of a loop.

Paul recommended using Frank Harrell's `rms::orm()` function for ordinal
regression.

📄 This paper uses by-word entropy as a measure of intelligibility. 
<https://doi.org/10.1017/S0305000921000714>

💡 Monads in one sentence:

> A monad is the minimum amount of structure needed to overload function
> composition in a way that "performs an extra computation" on the
> intermediate value. 
> -- <https://www.youtube.com/watch?v=Nq-q2USYetQ&feature=youtu.be>


I had these quotes in some old notes:

> In so complex a thing as human nature, we must consider, it is hard to find
> rules without exception. --- George Eliot

> If any one faculty of our nature may be called more wonderful than the rest, I
> do think it is memory...The memory is sometimes so retentive, so serviceable,
> so obedient; at others, so bewildered and so weak. We are, to be sure, a
> miracle every way; but our powers of recollecting and of forgetting do seem
> peculiarly past finding out. --- Jane Austen

Asked twitter if they knew any IRR tutorials

  - <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4913118/>
  - <https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3402032/>


## R 3.6.0 released! (2019-04-26)

Highlights for me

  - New `sample()` implementation
  - New function `asplit()` allow splitting an array or matrix by its margins.
  - Functions mentioned I didn't know about: `lengths()`, `trimws()`,
    `extendrange()`, `convertColor()`, `strwidth()`

## Phylogenetic regression (2019-02-26)

Listened to bits of McElreath's phylogenetic regression lecture.

  - You can model a simple linear regression as a multivariate
    regression.
  - Make the covariance matrix the identity matrix and multiple it by
    the error term sigma.
  - In this formulation, you can swap out the identity correlation
    matrix with something estimated using a correlation/distance matrix.
    If the distances were age, you can have units with similar ages have
    correlated errors. Now you have a gaussian process regression.
    
    
## multicomp (2019-02-25)

  - We are using the multcomp package and `glht()` to test hypotheses
    from fitted models. Never used this package before. Something to
    learn.
  - `glht()` will compute a group difference (like asymptote of SMI-LCT
    vs SMI-LCI) from a fitted model and give you a standard error, *z*
    statistic and *p* value for that difference.
  - I can get very similar results by sampling the multivariate normal
    distribution of the model coefficients/variance-covariance matrix
    and computing the group differences from the samples, using the
    standard deviation of the samples to get the standard error.


## Old rolling list of bookmarks

dtool: Manage scientific data <https://dtool.readthedocs.io/en/latest/>

A very first introduction to Hamiltonian Monte Carlo
<https://blogs.rstudio.com/tensorflow/posts/2019-10-03-intro-to-hmc/>

Some things you maybe didn't know about linear regression
<https://ryxcommar.com/2019/09/06/some-things-you-maybe-didnt-know-about-linear-regression/>

dbx database tools for R <https://github.com/ankane/dbx>

Dash for R
<https://medium.com/@plotlygraphs/announcing-dash-for-r-82dce99bae13>

How to interpret F-statistic
<https://stats.stackexchange.com/questions/12398/how-to-interpret-f-and-p-value-in-anova>

The origin of statistically significant
<https://www.johndcook.com/blog/2008/11/17/origin-of-statistically-significant/>

tidymv: Tidy Model Visualisation for Generalised Additive Models
<https://cran.r-project.org/web/packages/tidymv/index.html>

Step-by-step examples of building publication-quality figures in ggplot2
<https://github.com/clauswilke/practical_ggplot2>

From data to viz <https://www.data-to-viz.com/>

Shapley model explanation <https://github.com/slundberg/shap>

JavaScript versus Data Science
<https://software-tools-in-javascript.github.io/js-vs-ds/en/>

Penalized likelihood estimation
<https://modernstatisticalworkflow.blogspot.com/2017/11/what-is-likelihood-anyway.html>

UTF-8 everywhere <https://utf8everywhere.org/>

Unicode programming
<https://begriffs.com/posts/2019-05-23-unicode-icu.html>

R package to simulate colorblindness
<https://github.com/clauswilke/colorblindr>

Data version control <https://dvc.org/>

Email tips
<https://twitter.com/LucyStats/status/1131285346455625734?s=20>

colorcet library (python) <https://colorcet.pyviz.org/>

HCL wizard <http://hclwizard.org/hclwizard/>

Coloring for colorblindness. Has 8 palettes of color pairs
<https://davidmathlogic.com/colorblind/>

5 things to consider when creating your CSS style guide by
@malimirkeccita
<https://medium.com/p/5-things-to-consider-when-creating-your-css-style-guide-7b85fa70039d>

Tesseract OCR engine for R
<https://cran.r-project.org/web/packages/tesseract/vignettes/intro.html>

Lua filters for rmarkdown documents <https://github.com/crsh/rmdfiltr>

An NIH Rmd template <https://github.com/tgerke/nih-rmd-template>

Commit message guide
<https://github.com/RomuloOliveira/commit-messages-guide>

Linear regression diagnostic plots in ggplot2
<https://github.com/yeukyul/lindia>

A graphical introduction to dynamic programming
<https://avikdas.com/2019/04/15/a-graphical-introduction-to-dynamic-programming.html>

Why software projects take longer than you think
<https://erikbern.com/2019/04/15/why-software-projects-take-longer-than-you-think-a-statistical-model.html>

Automatic statistical reporting <https://github.com/easystats/report>

Multilevel models and CSD
<https://pubs.asha.org/doi/pdf/10.1044/2018_JSLHR-S-18-0075>

Map of cognitive science
<http://www.riedlanna.com/cognitivesciencemap.html>

An additive Gaussian process regression model for interpretable
non-parametric analysis of longitudinal data
<https://www.nature.com/articles/s41467-019-09785-8>

Common statistical tests are linear models (or: how to teach stats)
<https://lindeloev.github.io/tests-as-linear/>

Monte Carlo sampling does not "explore" the posterior
<https://statmodeling.stat.columbia.edu/2019/03/25/mcmc-does-not-explore-posterior/>

How to develop the five skills that will make you a great analyst
<https://mode.com/blog/how-to-develop-the-five-soft-skills-that-will-make-you-a-great-analyst>

Confidence intervals are a ring toss
<https://twitter.com/epiellie/status/1073385427317465089>

Mathematics for Machine Learning <https://mml-book.github.io/>

20 Tips for Senior Thesis Writers
<http://hwpi.harvard.edu/files/complit/files/twenty_tips_for_senior_thesis_writers_revised_august_2012.pdf>

Comparing common analysis strategies for repeated measures data
<http://eshinjolly.com/2019/02/18/rep_measures/>

Cosine similarity, Pearson correlation, and OLS coefficients
<https://brenocon.com/blog/2012/03/cosine-similarity-pearson-correlation-and-ols-coefficients/>

qqplotr is a nice package for plotting qqplots
<https://cran.r-project.org/web/packages/qqplotr/index.html>

Multidimensional item response theory
<https://github.com/philchalmers/mirt>

Aki's tutorials/materials on model selection
<https://github.com/avehtari/modelselection_tutorial>

An Introverts Guide to Conferences
<https://laderast.github.io/2018/05/17/a-introvert-s-survival-guide-to-conferences/>

Best practice guidance for linear mixed-effects models in psychological
science <https://psyarxiv.com/h3duq/>

Viewing matrices and probabilities as graphs
<https://www.math3ma.com/blog/matrices-probability-graphs>

Cross-validation for hierarchical models
<https://avehtari.github.io/modelselection/rats_kcv.html>

All of Aki's tutorials <https://avehtari.github.io/modelselection/>

User-friendly p values
<http://thenode.biologists.com/user-friendly-p-values/research/>

Iodide is a Javascript notebook <https://alpha.iodide.io/>

Interesting question about what do when transformation changes the
"test" of a highest-density interval.
<https://discourse.mc-stan.org/t/exponentiation-or-transformation-of-point-estimates/7848>

Some ways to rethink statistical rules
<https://allendowney.blogspot.com/2015/12/many-rules-of-statistics-are-wrong.html>

Toward a Principled Bayesian Workflow
<https://betanalpha.github.io/assets/case_studies/principled_bayesian_workflow.html>

Stumbled across an article on mixed models and effect sizes:
<https://www.journalofcognition.org/articles/10.5334/joc.10/>
