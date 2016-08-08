---
title: "Another day, another language"
excerpt: "Using liquid to generate CV sections"
tags: []
---

I just started using Jekyll + Github pages, and I created a page for my CV. At first, I thought I would just write citations for my work in markdown and be done with it. But this proved tedious and error-prone. 

After poking around the codebase for this Jekyll theme, I observed that the site uses some light programming for generating content. For example, the blog index page features if statements, for-loops, dot properties (`post.date`), and piping (`post.excerpt | strip_html | truncate: 160`).

```html
{% raw %}{% capture written_year %}'None'{% endcapture %}
{% for post in site.posts %}  
  {% capture year %}{{ post.date | date: '%Y' }}{% endcapture %}
  {% if year != written_year %}
    <h3>{{ year }}</h3>
    {% capture written_year %}{{ year }}{% endcapture %}
  {% endif %}
  <article>
    {% if post.link %}
      <h2 class="link-post"><a href="{{ site.url }}{{ post.url }}" title="{{ post.title }}">{{ post.title }}</a> <a href="{{ post.link }}" target="_blank" title="{{ post.title }}"><i class="fa fa-link"></i></a></h2>
    {% else %}
      <h2><a href="{{ site.url }}{{ post.url }}" title="{{ post.title }}">{{ post.title }}</a></h2>
      <p>{{ post.excerpt | strip_html | truncate: 160 }}</p>
    {% endif %}
  </article>
{% endfor %}{% endraw %}
```

Time for some programming.

Academic work is just a list of citations, which are just templated strings. In APA style, it's basically `[Who] ([When]). [What]. [Where]. [Etc.].` So for each piece of academic work, 

Let's do some programming.
