# Summary quality characteristics
qualCol <-  list(
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 1500), c(1500, 3000), c(3000, Inf)),
    "name" = 'MACS2__total_peaks'
  ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 1.3), c(1.3, 2), c(2, Inf)),
    "name" = 'rlfs_pval'
  ),
  # tibble(
  #   "descript" = rep(c('low', 'high'), 3),
  #   "type" = rep(c("error", "warning", "ok"), each = 2),
  #   'vals' = c(c(-Inf, 5), c(5, 15), c(15, Inf)),
  #   "name" = "rlfs_max"
  # ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 60), c(60, 80), c(80, 100)),
    "name" = "pct_aligned"
  ),
  tibble(
    "descript" = rep(c('low', 'high'), 3),
    "type" = rep(c("error", "warning", "ok"), each = 2),
    'vals' = c(c(0, 80), c(80, 90), c(90, 100)),
    "name" = "percent_passing"
  )
) %>% 
  bind_rows() %>%
  dplyr::select(-descript) %>%
  group_by(name, type) %>%
  summarise(vals = list(vals)) %>%
  pivot_wider(id_cols = name, names_from = type, values_from = vals)

qualColors <- tibble(
  'name' = c("error", "warning", "ok"),
  'color' = c("red", "orange", "green")
)


