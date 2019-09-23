---
title: Curriculum vitae
layout: single
modified: 2019-09-23
excerpt: ""
share: false
paragraph-indent: false;
permalink: /cv.html
---

## <i class="fas fa-graduation-cap"></i> Education

  - 2018 - Ph.D., Communication Sciences and Disorders, University of
    Wisconsin–Madison.
      - Dissertation: [Development of word recognition in
        preschoolers](https://www.tjmahr.com/dissertation/)
      - Ph.D. minor course work in statistics and psychology.
  - 2013 - M.S., Speech-Language Pathology, University of Wisconsin–Madison.
  - 2009 - B.S., Linguistics and English, University of Wisconsin–Madison.

## <i class="fas fa-user-astronaut"></i> Current position

**Data scientist** - Wisconsin Intelligibility, Speech and Communication Laboratory

I study how children with cerebral palsy learn to talk and communicate. The
project has amassed nearly 15 years of longitudinal speech and language data
from children with cerebral palsy, and I analyze the developmental trajectories
of these children.


**Statistical consulting** - Freelance.

I have consulted on generalized mixed-effects regression models. I fit models
and visualize data from psychology experiments. I deliver reproducible analyses
with tutorials/commentary on the analysis steps and code.
([Get in touch.](mailto:tjmahrweb@gmail.com))


## <i class="fas fa-list-ul"></i> Previous positions

**Research assistant** - Little Listeners project.

I prepared and analyzed eyetracking data from a series of word recognition
experiments with toddlers. I also developed R packages for handling, visualizing
and modeling these data, including a web-based interactive dashboard.

**Research assistant** - Learning to Talk project.

This project was a three-year longitudinal study of preschoolers done in
collaboration by three universities. I was responsible for data organization,
validation, automation and general data-cleaning tasks for the project. I
developed R packages to automate these validation and back-up tasks, and I
developed a MySQL database supporting R package for our data. I analyzed
eyetracking and speech perception experiments from this project.


## <i class="fas fa-chart-line"></i> Programming and statistics

**Languages**: Mastery of R. Regular practice writing SQL, HTML/CSS, XML, YAML,
Stan. Some practice with Java, JavaScript and Python. Exposure to Clojure.

**Computing tools and technologies**: bash/batch scripting, git and GitHub, tidy
data and the R tidyverse, Shiny, knitr/rmarkdown, Docker, Praat, makefiles,
regular expressions.

**Statistics**: Regression: generalized linear models, hierarchical and mixed
effects models, generalized additive models, and Bayesian versions of these
models. Structural equation modeling and factor analysis. Neural networks:
practice with shallow networks, familiarity with how some deep network
architectures work (CNNs, RBMs, RNNs, denoising autoencoders).

## <i class="fas fa-paragraph"></i> Publications

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



## <i class="fas fa-code"></i> Software

**[bayesplot](http://mc-stan.org/bayesplot/)**. R package for visualizing 
Bayesian models and MCMC samples. It is the plotting library for 
the [Stan programming language](http://mc-stan.org/).

**[rprime](http://cran.r-project.org/web/packages/rprime)**. R package for
working Eprime text files.

**[polypoly](http://cran.r-project.org/web/packages/polypoly)**. Tools for 
orthogonal polynomials.

**[littlelisteners](/littlelisteners/)**. A general-purpose R package for
dealing with eyetracking data.

**[lookr](https://github.com/tjmahr/lookr)**. R package for dealing with
eyetracking data for the Learning To Talk lab.

**[L2TDatabase](https://github.com/LearningToTalk/L2TDatabase)**. R package for
working with the Learning To Talk lab's MySQL database. Helper functions for
creating, updating, backing-up MySQL tables in R.

**[adventofcode17](https://github.com/tjmahr/adventofcode17)**. Completed a 
[series of programming puzzles](http://adventofcode.com/2017) in R.

Plus, countless other R packages for various projects or problems.

I have also peer-reviewed software:

* [charlatan](https://github.com/ropensci/onboarding/issues/94#issuecomment-283799109), 
  a tool for generating fake data
* [bayestestR](https://github.com/openjournals/joss-reviews/issues/1541#issuecomment-516067570), 
  functions for describing Bayesian models and posterior distributions




## <i class="fas fa-bullhorn"></i> Presentations

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




### Conference talks

{% assign authored_talks = site.data.bib.talks | where: "type", "authored" %}

{% for pub in authored_talks %}

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

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ title  | strip_newlines }}**. {{ pub.where  | strip_newlines }} {{doi}} {{ bonus | strip_newlines }}
{% endfor %}


### Coauthored talks (i.e., I didn't talk, but probably did stats and made figures)

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


## <i class="fas fa-chalkboard-teacher"></i> Teaching

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



## <i class="fas fa-award"></i> Awards

2017–18, 2012–13: Supported by NIH Training Grant to Susan Ellis Weismer: _5-T32-DC5359,
Interdisciplinary Research in Speech–Language Disorders._

2016: [Jean Berko Gleason Award](https://twitter.com/TheBUCLD/status/795298601605992448) 
for highest scored student submission at the 41st Boston University Conference
on Language Development.

2014: Paula Menyuk Travel Award recipient at the 39th Boston University
Conference on Language Development.

2014: Emma Allen Scholarship. Given by the Dept. of Communication Sciences and
Disorder at UW–Madison.

2013–15: Supported by NIH Training Grant to Maryellen MacDonald:
_2-T32-HD049899, Training in Language: Acquisition and Adult Performance._

2012: Students Preparing for Academic-Research Careers (SPARC) Award. Given by
the American Speech-Language-Hearing Association.
