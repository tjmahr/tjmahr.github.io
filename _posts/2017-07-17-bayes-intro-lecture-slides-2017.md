---
title: "Slides from my intro to Bayesian regression talk"
excerpt: This time in LaTeX
tags:
  - bayesian
gallery:
  - url: /assets/images/2017-07-bayes-slide1.png
    image_path: /assets/images/2017-07-bayes-slide1.png
    alt: "Slide where I explain 'Bayesianism'"
  - url: /assets/images/2017-07-bayes-slide2.png
    image_path: /assets/images/2017-07-bayes-slide2.png
    alt: "Bayesian updating demo"
---



Back in April, I gave a guest lecture on Bayesian regression for the psychology 
department's graduate statistics class. This is the same course where I first
learned regression---and where I first started using R for statistics instead
of for data cleaning. It was fun drawing on my experience in that 
course and tailoring the materials for the level of training.

Here are the materials:

* [PDF version of slides](https://cdn.rawgit.com/tjmahr/Psych710_BayesLecture/55f446a0/bayes_slides_out.pdf)
* [Same content but as a (non-presentation) web page](https://cdn.rawgit.com/tjmahr/Psych710_BayesLecture/55f446a0/bayes_slides.html)
* [Github repository](https://github.com/tjmahr/Psych710_BayesLecture)

{% include gallery caption="Some slides from the talk." %}

## Observations (training data)

As I did with [my last Bayes talk](/rstanarm-tutorial-slides/), I'm going to note 
some questions from the audience, so I don't forget what kinds of questions
people have when they are introduced to Bayesian statistics.

One theme was frequentist baggage :handbag:. One person asked about 
[Type I and Type II error](https://twitter.com/chrisalbon/status/874683365865029632) 
rates. I did not have a satisfactory (that is, _rehearsed_) answer ready for
this question. I think I said something about how those terms are based on a
frequentist, repeated-sampling paradigm, whereas a Bayesian approach 
[worries about different sorts of errors](http://andrewgelman.com/2004/12/29/type_1_type_2_t/). 
(Statistical [power is still important](https://alexanderetz.com/2015/05/21/type-s-and-type-m-errors/), 
of course, for both approaches.) Next time, I should study up on the
frequentist properties of Bayesian models, so I can field these questions
better. 

Other questions:

* Another bit of frequentist baggage :handbag:. I mentioned that with a
posterior predictive distribution, we can put an uncertainty interval on any
statistic we can calculate, and this point brought up the question of _multiple
comparisons_. These are a bad thing in classical statistics. But for Bayes, there
is only one model, and the multiple comparisons are really only the implications 
of one model.
* Someone else said that they had heard that Bayesian models can provide evidence
for a null effect---*how does that work?* I briefly described the 
[ROPE approach](https://doingbayesiandataanalysis.blogspot.nl/2016/12/bayesian-assessment-of-null-values.html),
ignoring the existence of Bayes factors entirely.

For future iterations of this tutorial, I should have a worked example, maybe a
blog post, on each of these issues. 

It's kind of amusing now that I think about it. A big part of my enthusiasm for
Bayesian statistics is that I find it much more intuitive than frequentist
statistics. _Yes!_ I thought to myself. _I never have to worry about what the
hell a confidence interval is ever again!_ Well, actually---no. I need to know
this stuff even more thoroughly than ever if I am going to talk fluently about
what makes Bayes different. ¯\\\_(ツ)\_/¯
