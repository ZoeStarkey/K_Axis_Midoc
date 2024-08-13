##### PLOT OF TAXA TOTAL ######
load("~/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_DF/K4S_DA_DF/km_df_environmental_variables.Rda")  

km_df <- km_df %>%
  filter(!is.na(depth) & depth != "0-1000m")

label_midoc_stn <- function(x) {
  sub("MIDOC", "", x)
}


km_df <- km_df %>% 
  mutate(tax.grp = na_if(tax.grp, "NA")) %>%
  filter(!is.na(tax.grp))


#remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps", "mixed/other invertebrates")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

library(RColorBrewer)

# Define a color-blind friendly palette
cb_palette <- brewer.pal(8, "Paired")
cb_palette <- c("#2e4057", "#4A92C6", "#FFC000",  "#ff7c43", "#C41E3A")

stacked_bar_plot <-ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  ) +
  scale_x_discrete(labels = label_midoc_stn) +
  scale_fill_manual(values = cb_palette) +
  labs(x = "Station", y = expression(paste("Biomass (g m"^"-3",")")), fill = "Taxonomic Group")




output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Stacked")
output_filename <- "K4S_Plot_A1_Bar_Chart.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = stacked_bar_plot, width =10, height = 8, dpi = 300, bg = "white")




## Define the custom color palette

custom_palette <- c("#2e4057", "#4A92C6", "#FFC000",  "#ff7c43", "#C41E3A")

custom_palette <- c("#1F4E79", "#4A92C6", "#FFC000", "#7B3C5D","#E68A4F") 

# Create the stacked bar plot
stacked_bar_plot_depth <- ggplot(km_df, aes(x = midoc.stn, y = bm_g_m3, fill = tax.grp)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid.major.x = element_blank(),
    legend.position = "right",
    axis.line.x = element_line(colour = NA, size = 0.5),
    axis.ticks.x = element_line(colour = "grey80", size = 0.5),
    axis.ticks.length = unit(3, "pt"),
    axis.title.x = element_text(margin = margin(t = 10))  # Increase space above x-axis label
  ) +
  scale_x_discrete(labels = label_midoc_stn, 
                   expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  facet_grid(rows = var("depth")) +
  facet_wrap(~factor(depth, levels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m"),
                     labels = c("0-200m", "200-400m", "400-600m", "600-800m", "800-1000m")), ncol = 1) +
  scale_fill_manual(values = custom_palette) +
  labs(x = "Midoc Station", y = expression(paste("Biomass (g m"^"-3",")")), fill = "Taxonomic Group")

# Display the plot
print(stacked_bar_plot)



output_directory <-  paste0("/Users/", usr,"/Desktop/Honours/Data_Analysis/K_axis_midoc/K4S_key_scripts/K4S_DA_Aims/K4S_DA_A1/K4S_Plot_A1/K4S_Plot_A1_Stacked")
output_filename <- "K4S_Plot_A1_Bar_Chart_Depth.png"
full_output_path <- file.path(output_directory, output_filename)



# Save the plot
ggsave(filename = full_output_path, plot = stacked_bar_plot_depth, width =10, height = 8, dpi = 300, bg = "white")


