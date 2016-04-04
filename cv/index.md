---
layout: page
title: CV [in prep]
modified: 2016-01-19
excerpt: ""
share: false
paragraph-indent: false;
---

## Education

tk tk 

## Current Position

**Research Assistant**

tk tk 

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

{% capture bonus %}{% if pub.bonus %} [{{pub.bonus}}] {% endif %}{% endcapture %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}). **{{ pub.title }}**. {{ location | strip_newlines}} {{doi}} {{bonus}}
{% endfor %}



## Software

**[L2TDatabase](https://github.com/LearningToTalk/L2TDatabase)**. R package for working with the Learning To Talk lab's MySQL database. Helper functions for creating, updating, backing-up MySQL tables in R.

**[lookr](https://github.com/tjmahr/lookr)**. R package for dealing with eyetracking data for the Learning To Talk lab.

**[rprime](http://cran.r-project.org/web/packages/rprime)**. R package for working Eprime text files.

**[retrace](https://github.com/tjmahr/retrace)**. A pure R implementation of the TRACE model of word recognition. Too slow to be useful, but great learning experience.



## Presentations

### Conference Talks

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
{% capture bonus %}{% if pub.bonus %} [{{pub.bonus}}] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title }}**. {{ pub.where}} {{doi}} {{bonus}}
{% endfor %}


#### Coauthored (i.e., I didn't talk, but did some stats and made figures)

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
{% capture bonus %}{% if pub.bonus %} [{{pub.bonus}}] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title }}**. {{ pub.where}} {{doi}} {{bonus}}
{% endfor %}


### Invited Talks

{% assign invited_talks = site.data.bib.talks | where: "type", "invited" %}

{% for pub in invited_talks %}

{% capture author_line %}
{% for author in pub.authors %}
{% if forloop.first %} {{author}}
{% elsif forloop.last %}, & {{author}}
{% else %}, {{author}}
{% endif %}
{% endfor %}
{% endcapture %}

{% capture doi %}{% if pub.doi %} [{{pub.doi}}](http://doi.org/{{pub.doi}}).{% endif %}{% endcapture %}
{% capture bonus %}{% if pub.bonus %} [{{pub.bonus}}] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title }}**. {{ pub.where}} {{doi}} {{bonus}}
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
{% capture bonus %}{% if pub.bonus %} [{{pub.bonus}}] {% endif %}{% endcapture %}

{{ author_line | strip_newlines }} ({{ pub.when.year }}, {{ pub.when.month }}). **{{ pub.title }}**. {{ pub.where}} {{doi}} {{bonus}}
{% endfor %}




## Awards

tk tk training-grants

tk tk bu-travel

2014: Emma Allen Scholarship. Given by the Dept. of Communication Sciences and Disorder at UW-Madison.

2012: Students Preparing for Academic-Research Careers (SPARC) Award. Given by the American
Speech-Language-Hearing Association.
