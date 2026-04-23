---
title: "Sentence similarity with dot product and cosine similarity"
date: 2024-01-01
tags: [nlp, vectors, similarity]
---


I read an
[article](https://txt.cohere.com/what-is-similarity-between-sentences/)
I had bookmarked for a while. Suppose you have word embedding (vector)
for each word. Or suppose you have a sentence embedding.

> The Cohere embedding assigns a vector of length 4096 (i.e., a list
> of 4096 numbers) to each sentence. Furthermore, the multilingual
> embedding does this for sentences in more than 100 languages. In this
> way, the sentence “Hello, how are you?” and its corresponding French
> translation, “Bonjour, comment ça va?” will be assigned very similar
> numbers, as they have the same semantic meaning.
> [[article](https://txt.cohere.com/what-is-similarity-between-sentences/)]

They have toy data about where some movies are assigned vectors:


``` r
df_movies <- data.frame(
  row.names = c("you've got mail", "rush hour", "rush hour 2", "taken"),
  action = c(0, 6, 7, 7),
  comedy = c(5, 5, 4, 0)
) 
movies <- df_movies|> as.matrix()
movies
#>                 action comedy
#> you've got mail      0      5
#> rush hour            6      5
#> rush hour 2          7      4
#> taken                7      0
```

One option for similarity is the dot product of the vectors. (Why?)


``` r
# the two rush hour movies
sum(movies[2, ] * movies[3, ])
#> [1] 62

# all at once
dot_products <- movies %*% t(movies)
```

Check this out. `as.dist()` will clean up the redundant output, but it
discards the diagonal. [Suggestion from
StackOverflow](https://stackoverflow.com/questions/2535234/find-cosine-similarity-between-two-arrays).


``` r
as.dist(dot_products)
#>             you've got mail rush hour rush hour 2
#> rush hour                25                      
#> rush hour 2              20        62            
#> taken                     0        42          49
```

Another option is cosine similarity. They plot the points on a coordinate
space and observe:

> [...] we want a measure of similarity that is high for sentences
> that are close to each other, and low for sentences that are far away
> from each other. [Euclidean] Distance does the exact opposite. So in
> order to tweak this metric, let’s look at the angle between the rays
> from the origin (the point with coordinates [0,0]), and each
> sentence. Notice that this angle is small if the points are close to
> each other, and large if the points are far away from each other. Now
> we need the help of another function, the cosine. The cosine of angles
> close to zero is close to 1 [...]
> [[article](https://txt.cohere.com/what-is-similarity-between-sentences/)] 

According to
[Wikipedia](https://en.wikipedia.org/wiki/Cosine_similarity), we can
derive the cosine similarity from the dot-product:

$$
\begin{aligned} 
\mathbf{A}\cdot\mathbf{B}&=\left\|\mathbf{A}\right\|\left\|\mathbf{B}\right\|\cos\theta \\ 
{\mathbf{A} \cdot \mathbf{B} \over \|\mathbf{A}\| \|\mathbf{B}\|} &= \cos(\theta) \\
\|\mathbf{m}\| &:= \sqrt{\mathbf{m} \cdot \mathbf{m}} ~~~~~\text{(Euclidean norm)}
\end{aligned}
$$

We have a matrix with all of the dot products ready to go, so we need a
matrix of all norms multiplied together.


``` r
dot_products
#>                 you've got mail rush hour rush hour 2 taken
#> you've got mail              25        25          20     0
#> rush hour                    25        61          62    42
#> rush hour 2                  20        62          65    49
#> taken                         0        42          49    49

norms <- sqrt(rowSums(movies ^ 2))
norm_products <- norms %*% t(norms)

(dot_products / norm_products) |> as.dist() |> round(3)
#>             you've got mail rush hour rush hour 2
#> rush hour             0.640                      
#> rush hour 2           0.496     0.985            
#> taken                 0.000     0.768       0.868
```

This value is cosine similarity. If we use 1 - similarity, it's cosine
distance. That would be more appropriate for anything piped into
`as.dist()`.

Still, it seems like two points could be very different distances from
the origin, but they could have an angle of 0.
