

```{r}
df <- carData::Wong
library(dplyr)
library(ggplot2)
ggplot(df) +
  aes(x = days, y = piq) +
  geom_line(aes(group = id))


gapminder::gapminder

ggplot(gapminder::gapminder) +
  aes(x = year, y = lifeExp) +
  geom_line(aes(group = country))


#
# # ggplot(totals) +
# #   aes(x = year, y = prop_born) +
# #   geom_line() +
# #   facet_wrap("name")
#
# percentile_years <- totals %>%
#   group_by(name) %>%
#   filter(abs(prop_born - .5) == min(abs(prop_born - .5))) %>%
#   ungroup()
#
# ggplot(percentile_years) +
#   aes(x = year, y = prop_born) +
#   ggrepel::geom_text_repel(aes(label = name))
#
# peak_year <- totals %>%
#   group_by(name) %>%
#   filter(n == max(n)) %>%
#   ungroup()
#
# ggplot(peak_year) +
#   aes(x = year, y = n) +
#   ggrepel::geom_text_repel(aes(label = name))
```

How many things are in dots?

  ```{r}
count_dots <- function(...) length(quos(...))
count_dots()
count_dots(1, 2, 3)

my_dots <- quos(1, 2, 3)

# it's counting just the one thing
count_dots(my_dots)

# but here it's spliced
count_dots(!!! my_dots)
```

We use `!!! dots` to splice in our `dots` values into the dots needed by `group_by()`.


group_by(name) %>%
  mutate(
    total_n = cumsum(n),
    prop_born = total_n / sum(n)) %>%
  ungroup()


Here's a side-note... Note that the `filter()` here does not use any of the
columns in the dataframe `data`. `group_indices(grouped) %in% sampled_groups`
returns a vector the same length as the number of rows in `data` This same
principle is how `filter(data, FALSE)` returns a 0-row version of a dataframe.

