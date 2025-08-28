
tag_theme <- theme(
  plot.tag.position = c(0.01, 1.3),   # x in [0,1], y just above panel
  plot.tag = element_text(size = 8, face = "bold", margin = margin(0,0,0,0))
)

# Row 1 tags
fish_solar_bar_tag        <- fish_solar_bar        + labs(tag = "(A)") + tag_theme
cephalopods_solar_bar_tag <- cephalopods_solar_bar + labs(tag = "(B)") + tag_theme

# Row 3 tags
fish_lunar_bar_tag        <- fish_lunar_bar        + labs(tag = "(C)") + tag_theme
cephalopods_lunar_bar_tag <- cephalopods_lunar_bar + labs(tag = "(D)") + tag_theme

left_top  <- plot_spacer() +
  inset_element(
    fish_solar_bar_tag,
    left = 0.028, bottom = -0.55, right = 0.886, top = 0.5,  
    align_to = "plot", on_top = TRUE, clip = TRUE
  )

right_top <- plot_spacer() +
  inset_element(
    cephalopods_solar_bar_tag,
    left = 0.007, bottom = -0.55, right = 1, top = 0.5,  
    align_to = "plot", on_top = TRUE, clip = TRUE
  )

left_mid <- plot_spacer() +
  inset_element(
    fish_lunar_bar_tag,
    left = 0.029, bottom = -0.55, right = 0.884, top = 0.5,  
    align_to = "plot", on_top = TRUE, clip = TRUE
  )

right_mid <- plot_spacer() +
  inset_element(
    cephalopods_lunar_bar_tag,
    left = 0.008, bottom = -0.55, right = 1, top = 0.5,  
    align_to = "plot", on_top = TRUE, clip = TRUE
  )


row1 <- left_top | right_top
row_2 <- (fish_heatmap_solar | cephalopods_heatmap_solar)
row_3 <- left_mid | right_mid
row_4 <- (fish_heatmap_lunar | cephalopods_heatmap_lunar)

combined <- (row1 / row_2 / row_3 / row_4) +
  plot_layout(ncol = 1,                       # one column of rows
              heights = c(0.1, 0.40, 0.1, 0.40))+  # one weight per row 
  plot_annotation(theme = theme(plot.margin = margin(0, 0, 0, 0)))

ggsave(
  filename = "~/Desktop/combined_temporal_plots_27_08.tiff",
  plot = combined,
  width = 190,                      # mm
  height = 150,                # ≈ 160 mm
  units = "mm",
  dpi = 500,
  compression = "lzw"
)



