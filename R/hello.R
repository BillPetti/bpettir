require(devtools)

theme_bp_grey <- function(base_size = 12, base_family = "Helvetica") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(face = "bold", size = 12),
      axis.title = element_text(face = "bold", size = 15),
      legend.title = element_text(face = "bold", size = 14),
      legend.text = element_text(face = "bold", size = 14),
      plot.background = element_rect(fill="#F0F0F0"),
      #panel.background = element_rect(fill="#F0F0F0", color=NA),
      panel.grid.major = element_line(colour="#FFFFFF",size=.75)
    )
}

usethis::use_data(theme_bp_grey, internal = FALSE, overwrite = TRUE)

tab_palette <- c("#006BA4", "#5F9ED1", "#A2CEEC", "#FFBC79", "#FF800E", "#C85200", "#595959")

usethis::use_data(tab_palette, internal = FALSE, overwrite = TRUE)

