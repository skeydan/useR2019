library(tidyverse)
library(hexSticker)

chains <- matrix(rnorm(1000), ncol = 4)

x <- seq(0, 1, .01)
y <- 3 * dbeta(x, 1.9, 2.8)
dist <- data.frame(x = x, y = y)

df <- as_tibble(chains,
                .name_repair = ~ c("chain_1", "chain_2", "chain_3", "chain_4")) %>%
  add_column(sample = (1:250)/250) %>%
  gather(key = "chain", value = "value", -sample)

p <- df %>%
  ggplot(aes(x = sample, y = value, color = chain)) +
  geom_line(size = 0.5) +
  geom_line(data = dist, aes(x = x, y = y), color = "#e0ffff") +
  theme_void() +
  theme_transparent() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("#7cb2d1", "#e6b7c1", "#593780", "#1f006e"))

plot(p)

sticker(
  p,
  package = "tfprobability",
  s_x = 1,
  s_y = 1.1,
  s_width = 1.3,
  s_height = 1,
  p_x = 1,
  p_y = 0.55,
  p_size = 12,
  h_fill = "#2ebdbd",
  h_color = "#e0ffff",
  filename = "poster/tfprobability.png"
)
