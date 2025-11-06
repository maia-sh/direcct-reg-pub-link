library(ggplot2)

# Prepare directory for figures (plots and tables)
# dir_figures <- fs::dir_create(here::here("figures"))

# Add font from system if available, or Google Fonts otherwise
add_font <- function(font) {
  
  font_file <-
    sysfonts::font_files() |>
    dplyr::filter(family == font, face == "Regular") |>
    dplyr::slice_head(n=1) |>
    dplyr::pull(file)
  
  if (length(font_file) == 1) {
    sysfonts::font_add(font, font_file)
  } else {
    cli::cli_inform("Downloading font `{font}`")
    sysfonts::font_add_google(font, font)
  }
}

# Prepare fonts
add_font("Roboto")
add_font("Roboto Mono")
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
