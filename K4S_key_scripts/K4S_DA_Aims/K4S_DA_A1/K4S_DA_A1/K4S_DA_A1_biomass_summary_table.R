library(dplyr)
library(tidyr)


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

# Get unique values
# unique_taxa <- unique(as.character(km_df$tax.grp))
# 
# # Capitalize first letter
# capitalized_taxa <- sapply(unique_taxa, function(x) {
#   paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
# })
# 
# km_df$tax.grp <- factor(km_df$tax.grp, 
#                         levels = unique_taxa, 
#                         labels = capitalized_taxa)
#remove gelatinous 
exclude_taxa <- c("cnidarians", "salps", "mixed/other gelatinous", "mixed krill and salps","mixed/other invertebrates")
km_df <-  km_df[!km_df$tax.grp %in% exclude_taxa, ]

#capitalised taxon
km_df$tax.grp <- tools::toTitleCase(km_df$tax.grp)


biomass_table <- km_df %>%
  group_by(midoc.stn, tax.grp) %>%
  summarize(biomass = sum(bm_g_m3, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = tax.grp, values_from = biomass, values_fill = 0) %>%
  mutate(
    Total_Biomass = rowSums(select(., -midoc.stn), na.rm = TRUE),
    Percentage_Fish = Fish / Total_Biomass * 100,
    Percentage_Cephalopod = Cephalopods / Total_Biomass * 100,
    Percentage_Krill = Krill / Total_Biomass * 100,
    `Percentage_Mixed_Other_Crustaceans` = `Mixed/Other Crustaceans` / Total_Biomass * 100,
    `Percentage_Mixed Fish and Invertebrates` = `Mixed Fish and Invertebrates` / Total_Biomass * 100

  ) %>%
  select(
    midoc.stn,
    Total_Biomass,
    Percentage_Fish,
    Percentage_Cephalopod,
    Percentage_Krill,
    `Percentage_Mixed_Other_Crustaceans`,
    `Percentage_Mixed Fish and Invertebrates`
    
  ) %>%
  mutate(across(starts_with("Percentage"), ~round(., 2))) %>%
  mutate(Total_Biomass = round(Total_Biomass, 3)) %>%
  arrange(midoc.stn)

# Display the table
print(biomass_table)

#download the biomass table as a csv
write.csv(biomass_table, "~/Desktop/biomass_table.csv", row.names = FALSE)
