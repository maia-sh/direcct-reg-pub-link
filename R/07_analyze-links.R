library(dplyr)

direcct_links_analysis <- readr::read_csv(here::here("data", "processed", "direcct-links-analysis.csv"))

# Visualize links ---------------------------------------------------------

source(here::here("R", "figures-setup.R"))

# Specify order of link practice labels
upset_levels <- c("TRN in full-text", "TRN in abstract", "TRN in PubMed metadata", "Publication in registration")

# Change label order
# https://github.com/const-ae/ggupset/issues/20
change_label_order <- function(df) {
  df |>
    mutate(single_label = factor(single_label, levels = rev(upset_levels))) |>
    ggplot(aes(x = at, y = single_label)) +
    geom_rect(aes(fill = index %% 2 == 0), ymin = df$index - 0.5,
              ymax = df$index + 0.5, xmin = 0, xmax = 1) +
    geom_point(aes(color = observed), size = 3) +
    geom_line(data = function(dat) dat[dat$observed,,drop=FALSE],
              aes(group = labels), linewidth = 0) +
    ylab("") + xlab("") +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_fill_manual(values= c(`TRUE` = "white", `FALSE` = "#F7F7F7")) +
    scale_color_manual(values= c(`TRUE` = "black", `FALSE` = "#E0E0E0")) +
    guides(color = "none", fill = "none") +
    theme(
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.line = element_blank(),
      panel.border = element_blank()
    )
}

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
    combmatrix.panel.line.size = 0,
    combmatrix.label.text = element_text(family = "Roboto", size = 11)
  ) +
  
  theme(
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.position.inside = c(.85, .9),
    axis.title.y = element_text(size = 11, vjust = -40)
  ) +
  
  ggupset::axis_combmatrix(
    sep = ",",
    override_plotting_function = change_label_order
  )


# Save links plots --------------------------------------------------------
# 
# ggsave(
#   fs::path(dir_figures, "plot-upset-link-type.pdf"),
#   plot_upset_links_reg_pub,
#   scale = 1.25,
#   width = 7,
#   height = 5
# )
# 
# ggsave(
#   fs::path(dir_figures, "plot-upset-link-type.svg"),
#   plot_upset_links_reg_pub,
#   scale = 1.25,
#   width = 7,
#   height = 5,
#   dpi = 600
# )
