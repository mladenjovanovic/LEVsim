library(hexSticker)
library(tidyverse)
library(cowplot)

L0 <- 200
V0 <- 2

intercept <- V0
slope <- -V0 / L0

plot_df <- tibble(
  x = c(0, L0),
  y = c(V0, 0)
)

one_RM_df <- tibble(
  v1RM = 0.4,
  `1RM` = L0 + v1RM * 1 / slope
)

gg <- ggplot(plot_df, aes(x = x, y = y)) +
  theme_cowplot(12) +
  geom_line(color = "#5DA5DA", size = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, L0 * 1.05), breaks = L0, labels = "L0") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, V0 * 1.05), breaks = V0, label = "V0") +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text=element_text(size=32))

gg <- gg +
  geom_segment(
    data = one_RM_df,
    aes(x = 0, xend = `1RM`, y = v1RM, yend = v1RM),
    alpha = 0.5,
    linetype = "dotted",
    size = 0.5
  ) +
  geom_segment(
    data = one_RM_df,
    aes(x = `1RM`, xend = `1RM`, y = 0, yend = v1RM),
    alpha = 0.5,
    linetype = "dotted",
    size = 0.5
  ) +
  geom_point(data = one_RM_df, aes(x = `1RM`, y = v1RM), size = 1, shape = 21, fill = "white") +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, V0 * 1.05),
    breaks = c(one_RM_df$v1RM, V0),
    label = c("v1RM", "V0")
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    limits = c(0, L0 * 1.05),
    breaks = c(one_RM_df$`1RM`, L0),
    label = c("1RM", "L0")
  )

s <- sticker(gg,
             package = "LEVsim",
             p_size = 40, p_color = "black", p_y = 0.5, dpi = 600,
             s_width = 1.5, s_x = 1, s_y = 1.1, s_height = 1.1,
             h_fill = "white",
             h_color = "#5DA5DA",
             filename = "man/figures/logo.png"
)
