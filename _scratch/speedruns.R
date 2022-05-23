
j <- jsonlite::read_json("runs.json")
j[[1]] |>
  str()
d <- j[[1]][[1]][["data"]] |>
  lapply(as.data.frame) |>
  lapply(stats::setNames, c("date", "time", "user", "link")) |>
  dplyr::bind_rows()
d
library(tidyverse)
library(ggplot2)
d <- d |>
  mutate(
    date = as.POSIXct(d$date, "UTC", origin = "1970-01-01"),
    minutes = time %/% 60,
    seconds = time %% 60,
    time2 = sprintf("%d:%06.3f", minutes, seconds)
  )

lubridate:::format.Duration
lubridate::pret
ggplot(d) +
  aes(x = date, y = time) +
  geom_step(aes(color = user, group = 1), direction = "hv") +
  geom_point() +
  theme_void()
