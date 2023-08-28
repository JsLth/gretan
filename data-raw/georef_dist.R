if (!require(pacman)) {
  install.packages("pacman")
}

pacman::p_load(ggplot2, ggridges, dplyr, sf)

load("R/sysdata.rda")

stacked <- bind_rows(
  "NUTS-0" = st_drop_geometry(srv_nuts0),
  "NUTS-1" = st_drop_geometry(srv_nuts1),
  "NUTS-2" = st_drop_geometry(srv_nuts2),
  "INSPIRE" = st_drop_geometry(srv_grid),
  .id = "level"
)

stacked$level <- factor(stacked$level, levels = rev(unique(stacked$level)))

ggplot(stacked) +
  geom_density_ridges2(aes(x = sample, y = level, fill = level)) +
  theme_ridges() +
  theme(legend.position = "none") +
  xlab("Sample size") +
  ylab("Geographical level")

ggsave("data-raw/level_dist.png", bg = "white")


summarise(
  stacked,
  Level = unique(level),
  Mean = mean(sample, na.rm = TRUE),
  SD = sd(sample, na.rm = TRUE),
  P25 = quantile(sample, probs = 0.25, na.rm = TRUE),
  Median = median(sample, na.rm = TRUE),
  P75 = quantile(sample, probs = 0.75, na.rm = TRUE),
  .by = "level"
) %>%
  select(-level) %>%
  print()