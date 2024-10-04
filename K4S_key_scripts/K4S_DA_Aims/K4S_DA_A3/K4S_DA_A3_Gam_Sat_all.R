
#TEST 3
# Fit the GAM model
fish_additive_all_vars <- gam(log(bm_sum_fish) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list <- draw(fish_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
# modify_geom_point <- function(plot, new_color) {
#   plot$layers[[which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))]]$aes_params$colour <- new_color
#   return(plot)
# }

modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}
# Customize each plot individually
fish_SST <- modify_geom_point(p_list[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  ) 
fish_CUR <- modify_geom_point(p_list[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title = element_text(color = "grey30")
  )
fish_CHLA <- modify_geom_point(p_list[[3]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )
fish_TSM <- modify_geom_point(p_list[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
(fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM)


###CEPHALOPODS
# Define the new model
ceph_additive_all_vars <- gam(log(bm_sum_ceph) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list_ceph <- draw(ceph_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}

# Customize each plot individually for cephalopod model
ceph_SST <- modify_geom_point(p_list_ceph[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

ceph_CUR <- modify_geom_point(p_list_ceph[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title = element_text(color = "grey30")
  )

ceph_CHLA <- modify_geom_point(p_list_ceph[[3]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

ceph_TSM <- modify_geom_point(p_list_ceph[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
(fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM) | (ceph_SST / ceph_CUR / ceph_CHLA / ceph_TSM)

#################KRILL####################
# Define the new model for krill
krill_additive_all_vars <- gam(log(bm_sum_krill) ~ s(SST) + s(CUR) + s(CHLA) + s(TSM), data = km_bm_sum)

# Extract the ggplot objects from draw()
p_list_krill <- draw(krill_additive_all_vars, residuals = TRUE, return_objects = TRUE)

# Function to modify existing geom_point layer
modify_geom_point <- function(plot, new_color, new_size = 2) {
  point_layer_index <- which(sapply(plot$layers, function(x) inherits(x$geom, "GeomPoint")))
  # Update both the color and size of the points
  plot$layers[[point_layer_index]]$aes_params$colour <- new_color
  plot$layers[[point_layer_index]]$aes_params$size <- new_size
  
  return(plot)
}

# Customize each plot individually for krill model
krill_SST <- modify_geom_point(p_list_krill[[1]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

krill_CUR <- modify_geom_point(p_list_krill[[2]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text = element_text(color = "grey30"),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.title = element_text(color = "grey30")
  )

krill_CHLA <- modify_geom_point(p_list_krill[[3]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

krill_TSM <- modify_geom_point(p_list_krill[[4]], "grey40") + 
  theme_minimal() +
  labs(title = NULL) +  # Remove the title
  xlab(NULL) +
  theme(
    panel.background = element_rect(fill = "grey90", color = NA),  # Dark grey for the plot area
    panel.grid.major = element_line(color = "white", size = 0.5),
    panel.grid.minor = element_line(color = "white", size = 0.25),
    axis.text.x = element_text(size = 14, color = "black"),  # Increase x-axis text size
    axis.text.y = element_text(size = 14, color = "black"), 
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey30")
  )

# Combine the plots back into a grid layout
satellite_vars_fish_ceph_krill <- (fish_SST/ fish_CUR/ fish_CHLA/ fish_TSM) | (ceph_SST / ceph_CUR / ceph_CHLA / ceph_TSM) | (krill_SST / krill_CUR / krill_CHLA / krill_TSM)


