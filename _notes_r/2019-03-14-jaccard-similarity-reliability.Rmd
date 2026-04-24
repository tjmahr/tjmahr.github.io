---
title: "Jaccard similarity for transcription reliability"
date: 2019-03-14
tags: [statistics, similarity, reliability]
---

[Jaccard similarity](https://en.wikipedia.org/wiki/Jaccard_index) is the
size of intersection of A and B divided by size of union of A and B.

Suppose a listener types of a few sentences twice. These are probes for
intra-listener transcription reliability. Suppose that one of these
sentences is 6 words long. If a listener typed 8 unique words for the
two sentences, and 4 of them appear in both sentences, then they have
Jaccard similarity of 4 / 8 = 0.5. Then we take the weighted mean of the
Jaccard scores. We use the sentence length as the weight, so this
sentence would have a weight of 6.
