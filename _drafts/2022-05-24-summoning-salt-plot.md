---
title: Creating a Summoning Salt-style speedrun plot
excerpt: "*Cool 8-bit music plays over a montage of me editing R code*"
tags:
  - r
  - ggplot2
share: true
header:
  overlay_image: "assets/images/void-plot-3-1.png"
  image_description: "The plot I created at the end of this post."
  overlay_filter: rgba(100, 100, 100, 0)
---




A videogame speedrun is a challenge to beat the game as quickly as
possible. It's time attack racing but for a videogame. There are, in my
mind, two ways to make a run's time go faster: Playing better and more
smoothly (optimizations, having better luck) and playing less of the
game (better routing, new glitches/skips). The history of a speedrun
category then is often an exciting mix of evolutionary improvements as
players level up their skills and revolutionary jumps as players find
new ways to cut through the game.

[Summoning Salt](https://www.youtube.com/c/SummoningSalt) is a Youtube
creator who creates documentaries that trace out the world record
progression in a speedrun. The videos are immensely enjoyable, as Salt
dishes out the history bit by bit, record by record, sometimes in a suspenseful fashion. 

As a data visualization person, I've noticed that Summoning
Salt recently started to use a new prop in the videos: A step graph of the
world record times. The graph is developed throughout a video as players 
(represented by individual colors) lower the times with new
records (points) until you get a full reveal of a timeline like the
following:


{% include figure image_path="/assets/images/2022-05-wr-plot-1.png" alt="Screenshot of a timeline from a Summoning Salt video." caption = "Screenshot of a timeline from a Summoning Salt video." %}{: style="max-width: 100%; display: block; margin: 2em auto;"}

Let's recreate this figure in R with ggplot2.


## Warp pipe: Obtaining the data

The game in question is *New Super Mario Bros Wii*, and the record
keeper is the site [speedrun.com](https://www.speedrun.com/nsmbw). There
is not just one speedrun category for this game, so in particular, we
want the "Any%" record history (i.e., "any percent": you don't have
play every level, and you can skip parts of the game.)

We need to get the leaderboard history data from speedrun.com. There is an
[official REST API](https://github.com/speedruncomorg/api) for the
site's data, but it's not straightforward how to query it to obtain the
data needed for a world record progression. (Apparently, one could
request [the leaderboard on different
dates](https://github.com/speedruncomorg/api/issues/123) and work
backwards through time.) But that's okay, we are not going to use the
API. Instead, the [statistics page for the
game](https://www.speedrun.com/nsmbw/gamestats) has a plot that is
tantalizingly close to the one we want to create.

{% include figure image_path="/assets/images/2022-05-wr-plot-2.png" alt="A timeline figure from speedrun.com." caption = "A timeline figure from speedrun.com." %}{: style="max-width: 100%; display: block; margin: 2em auto;"}


This plot is *interactive*, and our browser is downloading the data and
plotting it for us. If we snoop around the page, we can find the JSON
data behind the plot. In Firefox, when I right-click on the plot and hit
"Inspect", I see the HTML code that contains the plot. Just below the
plot's div is a chunk of Javascript.

{% include figure image_path="/assets/images/2022-05-firefox-shot2.png" alt="A screenshot of the Firefox inspector showing the speedrun data in a Javascript script tag." caption = "A screenshot of the Firefox inspector showing the speedrun data in a Javascript script tag." %}{: style="max-width: 100%; display: block; margin: 2em auto;"}



The first line of it is all the speedrun data that is being plotting. We
save that JSON into [its own file](https://github.com/tjmahr/tjmahr.github.io/blob/master/_R/data/2022-05-23-nsmbw-runs.json). 




## Ground pound: Filtering and cleaning the data

Let's read the data into R. JSON is short for "Javascript Object
Notation", and it's basically the equivalent of a `list()` in R. Hence,
[jsonlite](https://rdrr.io/pkg/jsonlite/man/read_json.html) provides a large, deeply nesting list to us.


```r
library(tidyverse)

# a helper function to download the data from github
# in case you want to play along
path_blog_data <- function(x) {
  file.path(
    "https://raw.githubusercontent.com",
    "tjmahr/tjmahr.github.io/master/_R/data",
    x
  )
}

json_runs <- path_blog_data("2022-05-23-nsmbw-runs.json") |> 
  jsonlite::read_json()
```

The plot on the statistics page came with a dropdown menu for different
kinds of records to display, so this JSON object has a sublist for each
dropdown menu choice. What we want is the first sublist (full game runs)
then its first sublist (with a `label` of `"Any% - Physical"`) then its
`"data"`. 


```r
# Dropdown menu choices
str(json_runs, max.level = 1)
#> List of 10
#>  $ 0   :List of 7
#>  $ 6789:List of 18
#>  $ 6805:List of 18
#>  $ 6815:List of 18
#>  $ 6826:List of 19
#>  $ 6841:List of 18
#>  $ 6846:List of 20
#>  $ 6859:List of 19
#>  $ 6868:List of 22
#>  $ 6882:List of 18

# Full game run histories
str(json_runs[[1]], max.level = 2)
#> List of 7
#>  $ :List of 7
#>   ..$ label                    : chr "Any% - Physical"
#>   ..$ data                     :List of 30
#>   ..$ borderColor              : chr "#EE4444"
#>   ..$ pointBorderColor         : chr "#EE4444"
#>   ..$ pointHoverBackgroundColor: chr "#EE4444"
#>   ..$ hidden                   : logi FALSE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "Cannonless - Physical"
#>   ..$ data                     :List of 25
#>   ..$ borderColor              : chr "#EF8241"
#>   ..$ pointBorderColor         : chr "#EF8241"
#>   ..$ pointHoverBackgroundColor: chr "#EF8241"
#>   ..$ hidden                   : logi FALSE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "100% - Physical"
#>   ..$ data                     :List of 17
#>   ..$ borderColor              : chr "#F0C03E"
#>   ..$ pointBorderColor         : chr "#F0C03E"
#>   ..$ pointHoverBackgroundColor: chr "#F0C03E"
#>   ..$ hidden                   : logi FALSE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "Any% No W5 - Physical"
#>   ..$ data                     :List of 22
#>   ..$ borderColor              : chr "#8AC951"
#>   ..$ pointBorderColor         : chr "#8AC951"
#>   ..$ pointHoverBackgroundColor: chr "#8AC951"
#>   ..$ hidden                   : logi TRUE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "Low% - Physical"
#>   ..$ data                     :List of 18
#>   ..$ borderColor              : chr "#09B876"
#>   ..$ pointBorderColor         : chr "#09B876"
#>   ..$ pointHoverBackgroundColor: chr "#09B876"
#>   ..$ hidden                   : logi TRUE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "Any% Multiplayer - Physical"
#>   ..$ data                     :List of 11
#>   ..$ borderColor              : chr "#44BBEE"
#>   ..$ pointBorderColor         : chr "#44BBEE"
#>   ..$ pointHoverBackgroundColor: chr "#44BBEE"
#>   ..$ hidden                   : logi TRUE
#>   ..$ steppedLine              : logi TRUE
#>  $ :List of 7
#>   ..$ label                    : chr "All Regular Exits - Physical"
#>   ..$ data                     :List of 7
#>   ..$ borderColor              : chr "#6666EE"
#>   ..$ pointBorderColor         : chr "#6666EE"
#>   ..$ pointHoverBackgroundColor: chr "#6666EE"
#>   ..$ hidden                   : logi TRUE
#>   ..$ steppedLine              : logi TRUE

# Just want the data field from the first one
json_any_percent <- json_runs[[1]][[1]][["data"]]
```

Here are the first two points. We have not-so-obviously encoded date
(`x`), the run length in seconds (`y`) and the `player`. We are going to
convert each of these lists into a dataframe and bind them together.


```r
json_any_percent |> head(2) |> str()
#> List of 2
#>  $ :List of 4
#>   ..$ x      : int 1306670400
#>   ..$ y      : int 1616
#>   ..$ players:List of 1
#>   .. ..$ : chr "RaikerZ"
#>   ..$ link   : chr "/nsmbw/run/2216987"
#>  $ :List of 4
#>   ..$ x      : int 1325246400
#>   ..$ y      : int 1549
#>   ..$ players:List of 1
#>   .. ..$ : chr "RaikerZ"
#>   ..$ link   : chr "/nsmbw/run/2216995"

data <- json_any_percent |> 
  lapply(
    # turn one list into a dataframe
    function(x) { 
      tibble(
        date = x$x, 
        run_time_s = x$y, 
        player = x$players[[1]]
      )
    }
  ) |> 
  bind_rows()

data
#> # A tibble: 30 × 3
#>          date run_time_s player       
#>         <int>      <dbl> <chr>        
#>  1 1306670400       1616 RaikerZ      
#>  2 1325246400       1549 RaikerZ      
#>  3 1332763200       1531 RaikerZ      
#>  4 1349870400       1527 RaikerZ      
#>  5 1457179200       1526 GreenUprooter
#>  6 1461585600       1523 Auchgard     
#>  7 1461672000       1522 Auchgard     
#>  8 1461758400       1519 Auchgard     
#>  9 1470744000       1514 Auchgard     
#> 10 1471521600       1512 Auchgard     
#> # … with 20 more rows
```

Lastly, we need to do something about those dates. When you see a
date-time represented by a single large number, it's probably a
[POSIX](https://rdrr.io/r/base/as.POSIXlt.html) date representing the date-time as the number of
seconds since some origin date-time (see also [Unix
Time](https://en.wikipedia.org/wiki/Unix_time)). Using the default Unix
origin time seems to give the correct date conversion:


```r
data <- data |> 
  mutate(
    date_posix = as.POSIXct(date, tz = "UTC", origin = "1970-01-01")
  ) 

data
#> # A tibble: 30 × 4
#>          date run_time_s player        date_posix         
#>         <int>      <dbl> <chr>         <dttm>             
#>  1 1306670400       1616 RaikerZ       2011-05-29 12:00:00
#>  2 1325246400       1549 RaikerZ       2011-12-30 12:00:00
#>  3 1332763200       1531 RaikerZ       2012-03-26 12:00:00
#>  4 1349870400       1527 RaikerZ       2012-10-10 12:00:00
#>  5 1457179200       1526 GreenUprooter 2016-03-05 12:00:00
#>  6 1461585600       1523 Auchgard      2016-04-25 12:00:00
#>  7 1461672000       1522 Auchgard      2016-04-26 12:00:00
#>  8 1461758400       1519 Auchgard      2016-04-27 12:00:00
#>  9 1470744000       1514 Auchgard      2016-08-09 12:00:00
#> 10 1471521600       1512 Auchgard      2016-08-18 12:00:00
#> # … with 20 more rows
```

## Triple jump: Plotting

First, let's get the data on the panel. I could spend an endless amount
of time tweaking or customizing a plot's theme, so I do the styling
last. Otherwise, styling would fill up all of the time I've set aside to
work on the plot.

We want to draw a point for each particular record-setting event, and we
want to draw a line that connects all of the points.
[`geom_step()`](https://ggplot2.tidyverse.org/reference/geom_path.html) draws a line plot but it can move
straight up/down or straight left/right---no diagonal lines---so it's
what we want. We also want to the color of these geometries to change
with the record holder (`player`).


```r
ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step() +
  geom_point()
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/plot-oops-1.png" title="A step plot with one line per player. It is not what we want." alt="A step plot with one line per player. It is not what we want." width="80%" style="display: block; margin: auto;" />

Oops! It assumed that we wanted to connected the dots separate for each color. We set the `group` aesthetic to a constant value so there is only one line drawn.


```r
ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step(aes(group = 1)) +
  geom_point()
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/plot-grouped-correctly-1.png" title="A step plot showing the world record progression. There is a single line and it changes color whenever a new record-holder takes over." alt="A step plot showing the world record progression. There is a single line and it changes color whenever a new record-holder takes over." width="80%" style="display: block; margin: auto;" />

Making the Summoning Salt version is just a matter of theming at this
point. We use [`theme_void()`](https://ggplot2.tidyverse.org/reference/ggtheme.html) to completely wipe out
the current theme, and we hide the color legend.


```r
ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step(aes(group = 1)) +
  geom_point() + 
  theme_void() +
  guides(color = "none")
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/void-plot-1-1.png" title="A step plot showing the world record progression." alt="A step plot showing the world record progression." width="80%" style="display: block; margin: auto;" />

Next, we are going to use the showtext package to obtain an 8-bit font:


```r
library(showtext)
#> Loading required package: sysfonts
#> Loading required package: showtextdb
font_add_google("Press Start 2P")
showtext_auto(TRUE)
```

The void font provides nothing, so we have to specify the main colors,
the axis lines, and the plotting margin. We also crank up the chroma
values for intense colors on the black background.


```r

ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step(aes(group = 1)) +
  geom_point() +
  guides(color = "none") + 
  scale_color_discrete(c = 255) +
  labs(title = "World Record Timeline") +
  theme_void(base_size = 20, base_family = "Press Start 2P") +
  theme(
    plot.title = element_text(color = "white", hjust = .5), 
    plot.background = element_rect(fill = "black"),
    axis.line = element_line(
      color = "white", 
      size = 1, 
      # more 8-bit looking lines
      lineend = "square"
    ), 
    plot.margin = margin(12, 12, 12, 12, "pt")
  ) 
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/void-plot-2-1.png" title="A step plot showing the world record progression. There is a black background now and an 8-bit looking font." alt="A step plot showing the world record progression. There is a black background now and an 8-bit looking font." width="80%" style="display: block; margin: auto;" />

To keep overlapping points from looking like blobs, we can use a filled
point. We see the outline of the points to black and the fill to the
player color. Then we have to make sure that guide for the fill doesn't appear and that fill and color have the same color scale.



```r
ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step(aes(group = 1)) +
  geom_point() +
  geom_point(
    aes(fill = player),
    shape = 21,
    color = "black", 
    size = 2
  ) +
  # no legend for fill
  guides(color = "none", fill = "none") + 
  # fill and color get same scale
  scale_color_discrete(c = 255, aesthetics = c("color", "fill")) +
  labs(title = "World Record Timeline") +
  theme_void(base_size = 20, base_family = "Press Start 2P") +
  theme(
    plot.title = element_text(color = "white", hjust = .5), 
    plot.background = element_rect(fill = "black"),
    axis.line = element_line(
      color = "white", 
      size = 1, 
      lineend = "square"
    ), 
    plot.margin = margin(12, 12, 12, 12, "pt")
  ) 
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/void-plot-3-1.png" title="A step plot showing the world record progression. The points have been restyled to have a black outline." alt="A step plot showing the world record progression. The points have been restyled to have a black outline." width="80%" style="display: block; margin: auto;" />



Finally, let's make another version of this figure. How might we make an
accessible of this information (of who held a record and when), assuming
we have a static image? A legend with players/colors is a nonstarter.
One idea would be to label the point with a line whenever there is a new
record holder.


```r
showtext_auto(FALSE)

data <- data |>
  # record whenever the title holder changes an "era"
  mutate(
    player2 = ifelse(
      player == "[gb/eng]FadeVanity", 
      "FadeVanity", 
      player
    ),
    change = player != lag(player) | is.na(lag(player)),
    era = cumsum(change)
  ) 

# I am going to hardcode some position adjustments for the labels
offsets <- c(1, 2, 1, 5, -1, 5, 4, 3, 2, 1, -4, -3, -2, -1)

data_lab <- data |> 
  group_by(era) |> 
  filter(run_time_s == max(run_time_s)) |> 
  ungroup() |> 
  mutate(offset = offsets)

ggplot(data) + 
  aes(x = date_posix, y = run_time_s, color = player) + 
  geom_step(aes(group = 1), size = 1) +
  geom_text(
    aes(
      label = player2,
      y = run_time_s  + 30 * offset 
    ),
    hjust = 0,
    size = 4,
    data = data_lab
  ) +
  geom_segment(
    aes(
      yend = run_time_s  + 30 * offset * .95, 
      xend = date_posix
    ),     
    data = data_lab, 
    linetype = "dashed"
  ) + 
  geom_point(size = 3) +
  # yes, I'm adding forty million seconds to the last datetime
  expand_limits(x = max(data$date_posix) + 4e7) +
  guides(color = "none") +
  scale_x_datetime(
    NULL,
    date_breaks = "2 years", 
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    "World record",
    breaks = 21:27 * 60,
    labels = function(x) sprintf("%d:%02.f", x %/% 60, x %% 60)
  ) + 
  theme_minimal(base_size = 14) +
  theme(plot.margin = margin(12, 12, 12, 12, "pt"))
```

<img src="/figs/drafts/2022-05-24-summoning-salt-plot/informative-plot-1.png" title="A step plot showing the world record progression. The name of the player is next to their point whenever the record changes." alt="A step plot showing the world record progression. The name of the player is next to their point whenever the record changes." width="80%" style="display: block; margin: auto;" />




