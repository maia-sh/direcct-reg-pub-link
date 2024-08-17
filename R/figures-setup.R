library(ggplot2)

# Prepare directory for figures (plots and tables)
dir_figures <- fs::dir_create(here::here("figures"))

# Prepare fonts
sysfonts::font_add_google("Roboto", "Roboto")
sysfonts::font_add_google("Roboto Mono", "Roboto Mono")
showtext::showtext_auto()

# Prepare colors
lightgray <- "gray75"
  darkgray <- "gray35"
    registry_colors <- c("DRKS" = darkgray, "ClinicalTrials.gov" = lightgray)

    # Set theme
    theme_set(theme_light(base_family = "Roboto"))

    theme_update(
      text = element_text(family = "Roboto"),
      axis.text = element_text(family = "Roboto Mono",size = 8)
    )
