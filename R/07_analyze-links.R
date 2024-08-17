library(dplyr)

direcct_links_analysis <- readr::read_csv(here::here("data", "processed", "direcct-links-analysis.csv"))

# Visualize links ---------------------------------------------------------

source(here::here("R", "figures-setup.R"))

# Transform links into list column of intersection sets
trials_links <-
  direcct_links_analysis |>
  select(id,
         has_reg_pub_link,
         has_trn_secondary_id,
         has_trn_abstract,
         has_trn_ft
  ) |>
  rename(
    "TRN in full-text" = has_trn_ft,
    "TRN in abstract" = has_trn_abstract,
    "TRN in PubMed metadata" = has_trn_secondary_id,
    "Publication in registration" = has_reg_pub_link
  ) |>
  tidyr::pivot_longer(cols = -id, names_to = "link") |>
  filter(value == TRUE) |>
  group_by(id) |>
  mutate(links = list(link)) |>
  ungroup() |>
  select(-value, -link) |>
  distinct()

# Prepare trials without links
# Create dummy links list column
trials_no_links <-
  direcct_links_analysis |>
  filter(
    !has_reg_pub_link &
      !has_trn_secondary_id &
      !has_trn_abstract &
      !has_trn_ft
  ) |>
  select(id) |>
  mutate(links = list(NULL))

plot_upset_links_reg_pub <-
  bind_rows(trials_links, trials_no_links) |>
  ggplot(aes(x = links)) +
  geom_bar() +
  geom_text(
    stat = 'count',
    aes(label = scales::percent(after_stat(count)/nrow(direcct_links_analysis), accuracy = 0.1)),
    vjust = -.5,
    size = 3.5) +
  scale_y_continuous(
    label = scales::label_percent(scale = 100/nrow(direcct_links_analysis), accuracy = 1),
    breaks = scales::breaks_width(nrow(direcct_links_analysis)/20),
    expand = expansion(mult = c(0, .05))
  ) +
  ggupset::scale_x_upset() +
  ylab("Percentage of trials") +
  xlab(NULL) +
  ggupset::theme_combmatrix(
    # combmatrix.label.make_space = FALSE,
    combmatrix.panel.line.size = 0,
    combmatrix.label.text = element_text(family = "Roboto", size = 11)
  ) +
  
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    axis.title.y = element_text(size = 11)
  )

# # Move y axis label closer to plot
# # Thanks to https://stackoverflow.com/questions/68593982
# plot_upset_links_reg_pub_registry +
#   theme(axis.title.y=element_blank()) +
#   annotate(geom = "text", x = -0.2, y = 6500, label = "count", angle = 90, size=4) +
#   coord_cartesian(xlim = c(1, 8), clip = "off")

# `ggupset` doesn't currently allow to change label order
# https://github.com/const-ae/ggupset/issues/20

ggsave(
  fs::path(dir_figures, "plot-upset-link-type.pdf"),
  plot_upset_links_reg_pub,
  scale = 1.25,
  width = 7,
  height = 5
  # scale = 2
)

ggsave(
  fs::path(dir_figures, "plot-upset-link-type.svg"),
  plot_upset_links_reg_pub,
  scale = 1.25,
  width = 7,
  height = 5,
  dpi = 600
)
