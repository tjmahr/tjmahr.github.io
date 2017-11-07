---
layout: single
title: Curriculum vitae
modified: 2017-03-31
excerpt: ""
share: false
paragraph-indent: false;
permalink: /cv.html
---

## Education

* Ongoing - Ph.D. candidate, Communication Sciences and Disorders, University of Wisconsin–Madison. 
  - Ph.D. minor course work in statistics and psychology.
* 2013 - M.S., Speech-Language Pathology, University of Wisconsin–Madison.
* 2009 - B.S., Linguistics and English, University of Wisconsin–Madison.

## Current position

**Research assistant** - Little Listeners project.

I prepare and analyze eyetracking data from a series of word recognition
experiments with toddlers. I develop R packages for handling, visualizing and
modeling these data, including an interactive Shiny dashboard.

**Statistical consulting** - Freelance.

I have consulted on generalized mixed-effects regression models. I fit models
and visualize data from psychology experiments. I deliver reproducible analyses
with tutorials/commentary on the analysis steps and code.
([Get in touch.](mailto:tjmahrweb@gmail.com))


### Previous position

**Research assistant** - Learning to Talk project.

This project was a three-year longitudinal study of preschoolers done in
collaboration by three universities. I was responsible for data organization,
validation, automation and general data-cleaning tasks for the project. I
developed R packages to automate these validation and back-up tasks, and I
developed a MySQL database supporting R package for our data. I analyzed
eyetracking and speech perception experiments from this project.


## Programming and statistics

**Languages**: Mastery of R. Regular practice writing SQL, HTML/CSS, XML, YAML,
Stan. Some practice with Java, JavaScript and Python. Exposure to Clojure.

**Computing tools and technologies**: bash/batch scripting, git and GitHub, tidy
data and the R tidyverse, Shiny, knitr/rmarkdown, Docker, Praat, makefiles,
regular expressions.

**Statistics**: Regression: generalized linear models, hierarchical and mixed
effects models, and Bayesian versions of these models. Structural equation
modeling and factor analysis. Neural networks: practice with shallow networks,
familiarity with how some deep network architectures work (CNNs, RBMs, RNNs,
denoising autoencoders).

## Publications

{% for pub in site.data.bib.publications %}

{% capture location %}
{% if pub.where %}
{% if pub.where.journal %}_{{ pub.where.journal }}_{% endif %}
{% if pub.where.volume %}, _{{ pub.where.volume }}_{% endif %}
{% if pub.where.pages %}, {{ pub.where.pages }}{% endif %}
.{% endif %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}
{% capture bonus %}{% if pub.bonus %} [[<i class="fa fa-{{pub.bonus.type}}" aria-hidden="true"></i> {{pub.bonus.text}}]({{pub.bonus.url}})] {% endif %}{% endcapture %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}). **{{ pub.title | strip_newlines }}**. {{ location | strip_newlines}} {{doi}} {{ bonus  | strip_newlines }}
{% endfor %}



## Software

**[bayesplot](http://mc-stan.org/bayesplot/)**. R package for visualizing 
Bayesian models and MCMC samples. It is the plotting library for 
the [Stan programming language](http://mc-stan.org/).

**[lookr](https://github.com/tjmahr/lookr)**. R package for dealing with
eyetracking data for the Learning To Talk lab.

**[rprime](http://cran.r-project.org/web/packages/rprime)**. R package for
working Eprime text files.

**[polypoly](http://cran.r-project.org/web/packages/polypoly)**. Tools for 
orthogonal polynomials.

**[L2TDatabase](https://github.com/LearningToTalk/L2TDatabase)**. R package for
working with the Learning To Talk lab's MySQL database. Helper functions for
creating, updating, backing-up MySQL tables in R.

Plus, countless other R packages for various projects or problems.

I have also peer-reviewed software for the [rOpenSci](https://ropensci.org/) 
project. Links to reviews:

* [charlatan](https://github.com/ropensci/onboarding/issues/94#issuecomment-283799109), 
  a tool for generating fake data




## Presentations

### Conference talks

{% assign authored_talks = site.data.bib.talks | where: "type", "authored" %}

{% for pub in authored_talks %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}
{% capture bonus %}{% if pub.bonus %} [[<i class="fa fa-{{pub.bonus.type}}" aria-hidden="true"></i> {{pub.bonus.text}}]({{pub.bonus.url}})] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title  | strip_newlines }}**. {{ pub.where  | strip_newlines }} {{doi}} {{ bonus | strip_newlines }}
{% endfor %}



#### Coauthored (i.e., I didn't talk, but probably did stats and made figures)

{% assign coauthored_talks = site.data.bib.talks | where: "type", "coauthored" %}

{% for pub in coauthored_talks %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}
{% capture bonus %}{% if pub.bonus %} [[<i class="fa fa-{{pub.bonus.type}}" aria-hidden="true"></i> {{pub.bonus.text}}]({{pub.bonus.url}})] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title  | strip_newlines }}**. {{ pub.where  | strip_newlines }} {{doi}} {{ bonus  | strip_newlines }}
{% endfor %}


### Invited talks

{% assign invited_talks = site.data.bib.talks | where: "type", "invited" %}

{% for pub in invited_talks %}

{% capture title %}
{% if pub.title.url %}[{{pub.title.text}}]({{pub.title.url}})
{% else %}{{pub.title}}
{% endif %}
{% endcapture %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}

{% capture bonus %}{% if pub.bonus %} [[<i class="fa fa-{{pub.bonus.type}}" aria-hidden="true"></i> {{pub.bonus.text}}]({{pub.bonus.url}})] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ title  | strip_newlines }}**. {{ pub.where  | strip_newlines }} {{doi}} {{ bonus  | strip_newlines }}
{% endfor %}



### Posters

{% for pub in site.data.bib.posters %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}
{% capture bonus %}{% if pub.bonus %} [[<i class="fa fa-{{pub.bonus.type}}" aria-hidden="true"></i> {{pub.bonus.text}}]({{pub.bonus.url}})] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title  | strip_newlines }}**. {{ pub.where  | strip_newlines }} {{doi}} {{ bonus  | strip_newlines }}
{% endfor %}


## Teaching

### Lecturing

* [Guest lecture on Bayesian regression](./bayes-intro-lecture-slides-2017) 
  for graduate statistics course in psychology department.
* Guest lecture on phonological disorders for introductory communicative
  disorders course.
* Guest lecture and accompanying online exercise on assessment of speech sounds
  for phonetics and phonological disorders course.

### Clinical experience

These are positions and roles I worked in as a student speech-language
clinician.

* 2013–2014: Speech-language diagnostics (eventually as a long-term substitute
  for position), Middleton Cross Plains School District.
* 2013: Pediatric voice clinic, American Family Children's Hospital.
* 2012: Fluency clinic, UW-Madison Speech & Hearing Clinic.
* 2012: Telling Life Stories program (helping individuals with aphasia create a
  scrapbook biography), UW-Madison Speech & Hearing Clinic.
* 2012: Transgender voice clinic, UW-Madison Speech & Hearing Clinic.
* 2011: Phonology clinic, UW-Madison Waisman Center.



## Awards

2016: [Jean Berko Gleason Award](https://twitter.com/TheBUCLD/status/795298601605992448) 
for highest scored student submission at the 41st Boston University Conference
on Language Development.

2014: Paula Menyuk Travel Award recipient at the 39th Boston University
Conference on Language Development.

2014: Emma Allen Scholarship. Given by the Dept. of Communication Sciences and
Disorder at UW–Madison.

2013–15: Supported by NIH Training Grant to Maryellen MacDonald:
_2-T32-HD049899, Training in Language: Acquisition and Adult Performance._

2012–13: Supported by NIH Training Grant to Susan Ellis Weismer: _5-T32-DC5359,
Interdisciplinary Research in Speech–Language Disorders._

2012: Students Preparing for Academic-Research Careers (SPARC) Award. Given by
the American Speech-Language-Hearing Association.
