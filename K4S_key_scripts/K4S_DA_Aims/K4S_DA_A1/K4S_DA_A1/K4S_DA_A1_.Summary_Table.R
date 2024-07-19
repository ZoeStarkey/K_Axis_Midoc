

library(dplyr)

# Assuming km_df has columns: 'tax.grp' and 'bm_g_m3'
# Define the groups explicitly
specific_groups <- c("fish", "cephalopods", "salps", "cnidarians", "krill")

# Create a new column to categorize "everything else"
km_df <- km_df %>%
  mutate(tax.grp.modified = ifelse(tax.grp %in% specific_groups, tax.grp, "everything else"))

# Summarize data
summary_table <- km_df %>%
  group_by(tax.grp.modified) %>%
  summarize(
    `Number of entries` = n(),
    `Summed biomass` = sum(bm_g_m3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    `Percentage of total` = `Summed biomass` / sum(`Summed biomass`) * 100
  )

# Add a total row
total_row <- summary_table %>%
  summarize(
    tax.grp.modified = "Total",
    `Number of entries` = sum(`Number of entries`),
    `Summed biomass` = sum(`Summed biomass`),
    `Percentage of total` = 100
  )

# Combine the summary table with the total row
summary_table <- bind_rows(summary_table, total_row)

# Rename the tax.grp.modified column to Group for the final table
summary_table <- summary_table %>%
  rename(Group = tax.grp.modified)

# Print the summary table
print(summary_table)



#checking with ceph biomass 
cephalopods_biomass <- km_df %>%
  filter(tax.grp == "cephalopods") %>%
  summarize(Summed_Biomass = sum(bm_g_m3, na.rm = TRUE))

# Print the result
print(cephalopods_biomass)





# Find all unique names in the tax.grp column
unique_tax_groups <- km_df %>%
  distinct(tax.grp) %>%
  arrange(tax.grp)

# Print the unique tax groups
print(unique_tax_groups)





#All GROUPS


library(dplyr)

# Define the specific tax.groups you want to include
specified_tax_groups <- c(
  "cephalopods", "cnidarians", "fish", "krill",
  "mixed fish and invertebrates", "mixed krill and salps",
  "mixed/other crustaceans", "mixed/other gelatinous",
  "mixed/other invertebrates", "salps"
)


# Summarize data
summary_table <- km_df %>%
  group_by(tax.grp.modified) %>%
  summarize(
    `Number of entries` = n(),
    `Summed biomass` = sum(bm_g_m3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    `Percentage of total` = `Summed biomass` / sum(`Summed biomass`) * 100
  )

# Add a total row
total_row <- summary_table %>%
  summarize(
    tax.grp.modified = "Total",
    `Number of entries` = sum(`Number of entries`),
    `Summed biomass` = sum(`Summed biomass`),
    `Percentage of total` = 100
  )

# Combine the summary table with the total row
summary_table <- bind_rows(summary_table, total_row)

# Rename the tax.grp.modified column to Group for the final table
summary_table <- summary_table %>%
  rename(Group = tax.grp.modified)

# Print the summary table
print(summary_table)




#GROUPING SOME TAXA 


# Define the specific tax.groups and the groupings you want
specified_tax_groups <- c(
  "cephalopods", "fish",
  "mixed fish and invertebrates", "mixed krill and salps",
  "mixed/other invertebrates", "salps"
)

# Filter out NA values and create new groups
km_df <- km_df %>%
  filter(!is.na(tax.grp)) %>%
  mutate(tax.grp.modified = case_when(
    tax.grp %in% c("krill", "mixed/other crustaceans") ~ "krill + mixed/other crustaceans",
    tax.grp %in% c("cnidarians", "mixed/other gelatinous") ~ "cnidarians + mixed/other gelatinous",
    tax.grp %in% specified_tax_groups ~ tax.grp,
    TRUE ~ "NA"
  ))

# Summarize data
summary_table <- km_df %>%
  group_by(tax.grp.modified) %>%
  summarize(
    `Number of entries` = n(),
    `Summed biomass` = sum(bm_g_m3, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    `Percentage of total` = `Summed biomass` / sum(`Summed biomass`) * 100
  )

# Add a total row
total_row <- summary_table %>%
  summarize(
    tax.grp.modified = "Total",
    `Number of entries` = sum(`Number of entries`),
    `Summed biomass` = sum(`Summed biomass`),
    `Percentage of total` = 100
  )

# Combine the summary table with the total row
summary_table <- bind_rows(summary_table, total_row)

# Rename the tax.grp.modified column to Group for the final table
summary_table <- summary_table %>%
  rename(Group = tax.grp.modified)

# Print the summary table
print(summary_table)


desktop_path <- "~/Desktop/summary_table_3.csv"
write_csv(summary_table, desktop_path)


