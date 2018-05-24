
library(dplyr)
library(ggplot2)
df <- carData::OBrienKaiser

df_long <- df %>%
  as_tibble() %>%
  tibble::rowid_to_column("id") %>%
  tidyr::gather("time", "value", -id, -treatment, -gender) %>%
  tidyr::separate(time, c("phase", "time")) %>%
  mutate(phase = factor(phase, c("pre", "post", "fup")))


ggplot(df_long) +
  aes(x = interaction(time, phase), y = value, color = treatment) +
  geom_line(aes(group = interaction(id, phase))) +
  geom_point()

#
