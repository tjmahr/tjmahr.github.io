---
title: "Custom tag feeds in Jekyll"
excerpt:
tags:
  - jekyll
  - meta
---

This is a meta-blogging post: A quick note about how I solved a Jekyll problem.

I want to add my blog to the [R-bloggers](https://www.r-bloggers.com/) aggregator. If I submit the [regular, built-in RSS feed](/feed.xml), then my non-R posts---like the post you're currently reading---would leak onto that site. They recommend that authors use their respective blogging platform's tagging system to tag aggregate-able posts and use an RSS feed of those tagged posts. My [Jekyll theme](https://mademistakes.com/work/minimal-mistakes-jekyll-theme/) doesn't do this by default---which makes sense. That would be overkill out of the box.

Jekyll blogs run on the liquid templating system, and I was able to find a [set of templates](https://github.com/snaptortoise/jekyll-rss-feeds) for constructing RSS feeds using liquid, and I [adapted it](https://github.com/tjmahr/tjmahr.github.io/blob/c33a922a7542b5038556b06757c8f1c449c58e19/feed.r.xml) for my theme. The result is [this feed](/feed.r.xml). 

Now, I just need to test that [my other posts](/year-archive/) look okay in this feed and inside of feed readers. That's the main of the post, by the way. :satisfied:
