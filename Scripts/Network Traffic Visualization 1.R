### Network Traffic Analysis Visualization Script ###

library(igraph)
library(ggplot2)  # For plotting
#install.packages("ggraph") # For network graph creation
library(ggraph)    # For network graph visualization
library(dplyr)    # For data manipulation


# Network Analysis Visualization -----------------------------------------------

## Filter down df for Visualizations

#rm(df_all_trackers_XL_dType1)  # Clean up 
# Create new df with only tracker entries
df_all_trackers_XL_dType1 <- merged_data_all_more_info %>% # (True/False Positives and False Negatives)
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>%
  select(AppName, domain, DomainOwnerName, hits)#, domainType, TrackerBlackListXL)

table(df_all_trackers_XL_dType1$domainType, df_all_trackers_XL_dType1$TrackerBlackListXL)

# Number of unique domains in df
length(unique(df_all_trackers_XL_dType1$domain))  # 901 unique domains

# Number of unique apps in df
length(unique(df_all_trackers_XL_dType1$AppName))  # 150 unique apps

# Number of unique DomainOwnerName in df
length(unique(df_all_trackers_XL_dType1$DomainOwnerName))  # 90 unique domain owners

#save.csv(df_all_trackers_XL_dType1, "Output/Tables/df_all_trackers_XL_dType1.csv", row.names = FALSE)


## Filtering down to Top24 Apps --------------------------------------------

#rm(df_all_trackers_XL_dType1_Top24)  # Clean up
df_all_trackers_XL_dType1_Top24 <- df_all_trackers_XL_dType1 %>%
  group_by(AppName) %>%
  filter(n() >= 54) %>%
  ungroup()

# Show unique apps in filtered df
length(unique(df_all_trackers_XL_dType1_Top24$AppName))  # 24 unique apps

# Show apps in df
unique(df_all_trackers_XL_dType1_Top24$AppName)

# Number of unique domains in filtered df
length(unique(df_all_trackers_XL_dType1_Top24$domain)) # 701 unique domains

# Number of unique DomainOwnerName in filtered df
length(unique(df_all_trackers_XL_dType1_Top24$DomainOwnerName)) # 75 unique domain owners

# Show occurrence of each app in filtered df
table(df_all_trackers_XL_dType1_Top24$AppName)

# Show occurrence of each domain owner in filtered df
table(df_all_trackers_XL_dType1_Top24$DomainOwnerName)


## Top24 Apps no unique domains --------------------------------------------

rm(Top24apps_no_unique_domains)  # Clean up
# Filter for domains that occur at least 3 times across all apps
Top24apps_no_unique_domains <- df_all_trackers_XL_dType1_Top24 %>%
  group_by(domain) %>%
  #filter(n() >= 5) %>%
  filter(n() >= 3) %>%
  ungroup()

# Show unique domains in filtered df
length(unique(Top24apps_no_unique_domains$domain)) # 297 unique domains

table(Top24apps_no_unique_domains$domain)

# Show number of unique rows
nrow(Top24apps_no_unique_domains)  # 2156 rows

# Merge and summarise rows with same AppName and domain
#Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
 # group_by(AppName, domain, DomainOwnerName) %>%
  #summarise(hits = sum(hits), .groups = 'drop')

# Filter for hits >= 2
Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
  filter(hits >= 2)

table(Top24apps_no_unique_domains$DomainOwnerName)

# Further filter for DomainOwnerName with at least 5 occurrences
Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
  group_by(DomainOwnerName) %>%
  filter(n() >= 5) %>%
  ungroup()

# Show unique domains in filtered df
length(unique(Top24apps_no_unique_domains$domain)) # 251 unique domains
length(unique(Top24apps_no_unique_domains$AppName))  # 24 unique apps
length(unique(Top24apps_no_unique_domains$DomainOwnerName))  # 26 unique domain owners

# Show number of unique rows
nrow(Top24apps_no_unique_domains)  # 1354 rows


## Top24 Apps smaller df --------------------------------------------

rm(Top24apps_no_unique_domains)  # Clean up
# Filter for domains that occur at least 5 times across all apps
Top24apps_no_unique_domains <- df_all_trackers_XL_dType1_Top24 %>%
  group_by(domain) %>%
  filter(n() >= 5) %>%
  #filter(n() >= 3) %>%
  ungroup()

# Show unique domains in filtered df
length(unique(Top24apps_no_unique_domains$domain)) #  unique domains

table(Top24apps_no_unique_domains$domain)

# Show number of unique rows
nrow(Top24apps_no_unique_domains)  #  rows

# Merge and summarise rows with same AppName and domain
Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
  group_by(AppName, domain, DomainOwnerName) %>%
  summarise(hits = sum(hits), .groups = 'drop')

# Filter for hits >= 3
#Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
 # filter(hits >= 3)

table(Top24apps_no_unique_domains$DomainOwnerName)

# Further filter for DomainOwnerName with at least 2 occurrences
Top24apps_no_unique_domains <- Top24apps_no_unique_domains %>%
  group_by(DomainOwnerName) %>%
  filter(n() >= 2) %>%
  ungroup()

# Show unique domains in filtered df
length(unique(Top24apps_no_unique_domains$domain)) # 160 unique domains
length(unique(Top24apps_no_unique_domains$AppName))  # 24 unique apps
length(unique(Top24apps_no_unique_domains$DomainOwnerName))  # 25 unique domain owners

# Show number of unique rows
nrow(Top24apps_no_unique_domains)  # 458 rows


# Top 10 Apps even smaller df ---------------------------------------------

# aggregating hits to find the most active apps and domains
# filtering to top 10 apps and top 50 domains to prevent visual clutter ("hairball" graph)
top_apps <- df %>%
  group_by(AppName) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  slice_max(order_by = total_hits, n = 10) %>%
  pull(AppName)

df_filtered <- df %>%
  filter(AppName %in% top_apps)

top_domains <- df_filtered %>%
  group_by(domain) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  slice_max(order_by = total_hits, n = 50) %>%
  pull(domain)

df_filtered <- df_filtered %>%
  filter(domain %in% top_domains)


## Final Filtered df for Visualization --------------------------------------

rm(filtered_df)  # Clean up
# Reorder & Rename columns
filtered_df <- Top24apps_no_unique_domains %>%
  select(AppName, domain, hits, DomainOwnerName) %>%
  # rename AppName to bundleID
  rename(bundleID = AppName)

length(unique(filtered_df$bundleID))  #  unique apps
length(unique(filtered_df$domain))    #  unique domains
length(unique(filtered_df$DomainOwnerName))  #  unique domain owners


## Tibble Data -------------------------------------------------------------

library(tibble)
is_tibble(df_all_trackers_XL_dType1)  # True
is_tibble(df_all_trackers_XL_dType1_Top24)  # True
is_tibble(Top24apps_no_unique_domains)  # True
is_tibble(filtered_df)  # True


# Concentric Network Visualization ----------------------------------------

# Loading necessary libraries
library(igraph)

# Clean Up for refinement
rm(g4)  
rm(unique_bundle_ids)  
rm(layout2)  
rm(vertex.size.values)  
rm(vertex.label.values)  
rm(layout_concentric2)  
rm(weight_threshold)  


# Framework for Concentric Network Visual --------------------------------------

# Filtering out rows where 'DomainOwnerName' is 'Other'
#filtered_df <- df #subset(df, DomainOwnerName != "Other")

# Creating an empty graph
g4 <- make_empty_graph(directed = FALSE)

# Adding the 'User' node
g4 <- add_vertices(g4, 1, name = "User", color = "black", layer = 0)

# Adding 'bundleID' nodes and connecting to 'User' with weight 1
unique_bundle_ids <- unique(filtered_df$bundleID)
for (bundle in unique_bundle_ids) {
  if (!(bundle %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = bundle, color = "red", layer = 1)
  }
  g4 <- add_edges(g4, c("User", bundle), weight = 1)
}

# Adding 'domain' nodes and connecting to 'bundleID' nodes using 'hits' as weights
for (i in 1:nrow(filtered_df)) {
  domain <- filtered_df$domain[i]
  bundle <- filtered_df$bundleID[i]
  hits <- filtered_df$hits[i]
  if (!(domain %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = domain, color = "orange", layer = 2)
  }
  g4 <- add_edges(g4, c(bundle, domain), weight = hits)
}

# Adding 'DomainOwnerName' nodes and connecting to 'domain' nodes with weight 1
for (i in 1:nrow(filtered_df)) {
  domain <- filtered_df$domain[i]
  domain_owner <- filtered_df$DomainOwnerName[i]
  hits <- filtered_df$hits[i]
  if (!(domain_owner %in% V(g4)$name)) {
    g4 <- add_vertices(g4, 1, name = domain_owner, color = "yellow", layer = 3)
  }
  g4 <- add_edges(g4, c(domain, domain_owner), weight = hits)#2) #3) #5) #1) #hits)
}


# 0. Basic Concentric Network ---------------------------------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("white")
  } else if (name %in% unique_bundle_ids) {
    return("red")
  } else if (name %in% filtered_df$domain) {
    return("orange")
  } else {
    return("yellow")
  }
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/Plots/concentric_network_adjusted0.png", width = 800, height = 800)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = V(g4)$name, 
     vertex.label.color = "black", 
     vertex.size = 20,
     edge.width = E(g4)$weight, 
     layout = layout2, 
     main = "Concentric Network Visualization")
dev.off()

# 1. Normalized edges --------------------------------------

# Adjust vertex sizes and labels
vertex.size.values <- ifelse(V(g4)$layer == 0, 25, ifelse(V(g4)$layer == 1, 15, ifelse(V(g4)$layer == 2, 10, ifelse(V(g4)$layer == 3, 15, 8))))
vertex.label.values <- ifelse(V(g4)$layer == 0 | V(g4)$layer == 1 | V(g4)$layer == 3, V(g4)$name, NA)

# Plotting the graph using concentric circles with 'User' at the center
layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 2.5  # Increase spacing between layers
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/Plots/concentric_network_adjusted1.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black", 
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 5,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Normalized Edges")
dev.off()

# 2. Edges are further adjusted + Plot Visual Refinement ---------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightskyblue1")
  } else if (name %in% unique_bundle_ids) {
    return("tomato1")
  } else if (name %in% filtered_df$domain) {
    return("orange")
  } else {
    return("gold")
  }
})

# Adjust vertex sizes and labels
vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 12, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- ifelse(V(g4)$layer == 0 | V(g4)$layer == 1 | V(g4)$layer == 3, V(g4)$name, NA)
#vertex.label.size.values <- ifelse(V(g4)$layer == 0, 3, ifelse(V(g4)$layer == 1, 1, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 3, 1))))

# Plotting the graph using concentric circles with 'User' at the center
layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 2.5  # Increase spacing between layers
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

# Save the plot to a file
png("Output/Plots/concentric_network_adjusted2.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     #vertex.label.size = vertex.label.size.values,
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Adjusted Concentric Network Visualization")
dev.off()


# 3. Plot displaying domain with more than 3 ---------------------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightblue1")
  } else if (name %in% unique_bundle_ids) {
    return("tomato1")
  } else if (name %in% filtered_df$domain) {
    return("snow2")
  } else {
    return("gold")
  }
})

vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 12, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (V(g4)$layer[V(g4)$name == name] == 2) {
    domain_count <- sum(filtered_df$domain == name)
    #if (domain_count >= 3) {
    #if (domain_count >= 5) {
    #if (domain_count >= 6) {  
    if (domain_count >= 7) {  
    #if (domain_count >= 10) {  
          
      return(name)
    }
    return(NA)
  }
  return(name)
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/Plots/concentric_network_adjusted3.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     #vertex.label.size = vertex.label.size.values,
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Domains with 3 or more occurrences")
dev.off()

# 4. Plot displaying domains with high weights -------------------------------

# Extracting node attributes
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightblue1")
  } else if (name %in% unique_bundle_ids) {
    return("tomato1")
  } else if (name %in% filtered_df$domain) {
    return("snow2")
  } else {
    return("gold")
  }
})

vertex.size.values <- ifelse(V(g4)$layer == 0, 20, ifelse(V(g4)$layer == 1, 13.5, ifelse(V(g4)$layer == 2, 1, ifelse(V(g4)$layer == 3, 25, 8))))
vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (V(g4)$layer[V(g4)$name == name] == 2) {
    domain_count <- sum(filtered_df$domain == name)
    if (domain_count >= 1) {
      return(name)
    }
    return(NA)
  }
  return(name)
})

#weight_threshold <- 20  # Set the threshold for minimum edge weight
#weight_threshold <- 25  # Set the threshold for minimum edge weight
#weight_threshold <- 30  # Set the threshold for minimum edge weight
weight_threshold <- 50  # Set the threshold for minimum edge weight

vertex.label.values <- sapply(V(g4)$name, function(name) {
  if (name %in% filtered_df$domain) {
    total_weight <- sum(E(g4)[.from(name)]$weight)
    if (total_weight < weight_threshold) {
      return(NA)
    }
  }
  return(name)
})

layout_concentric2 <- function(g4) {
  layers <- split(V(g4), V(g4)$layer)
  positions <- list()
  positions[["User"]] <- c(0, 0)  # Place 'User' at the center
  for (i in seq_along(layers)) {
    if (i == 1) next  # Skip layer 0 since 'User' is already placed
    layer_size <- length(layers[[i]])
    radius <- i * 1.5
    angles <- seq(0, 2 * pi, length.out = layer_size + 1)[-1]
    for (j in seq_along(layers[[i]])) {
      positions[[layers[[i]][j]$name]] <- c(radius * cos(angles[j]), radius * sin(angles[j]))
    }
  }
  return(do.call(rbind, positions))
}

layout2 <- layout_concentric2(g4)

png("Output/Plots/concentric_network_adjusted4.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     vertex.size = vertex.size.values, 
     edge.width = E(g4)$weight / max(E(g4)$weight) * 20,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Domains with High Weights")
dev.off()


# Plots without overlaps --------------------------------------------------

#vertex.label.dist <- ifelse(V(g4)$layer == 2, 1.5, # pushes labels away from nodes
 #                           ifelse(V(g4)$layer == 1, 0,
  #                                 0))     
#vertex.label.degree <- ifelse(V(g4)$layer == 2,  pi/4, # fixed direction
 #                             ifelse(V(g4)$layer == 1, 0,
  #                                   0))    

ggraph(g4, layout = "manual", x = layout2[,1], y = layout2[,2]) +
  #geom_edge_link(aes(width = weight/1000)) +
  geom_edge_arc(aes(alpha = ..index..), strength = 0.1, color = "grey50", show.legend = FALSE) +
  #scale_edge_width_continuous(range = c(0.1, 10) ) + #, guide = "none") +
  geom_node_point(aes(color = color)) +#, size = size)) +
  geom_node_text(aes(label = name), repel = TRUE)


# 5. Plot with less pixels for higher resolution? -------------------------

png("Output/Plots/concentric_network_adjusted5.png", width = 1500, height = 1500)
plot(g4, 
     vertex.color = V(g4)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     vertex.label.cex = 1.2,  # Increase the font size for vertex labels
     #vertex.label.dist = vertex.label.dist,
     #vertex.label.degree = vertex.label.degree,
     vertex.size = vertex.size.values,  # Keep vertex sizes as defined earlier
     edge.width = E(g4)$weight / max(E(g4)$weight) * 30, #1, #40, #20,  # Normalize edge width
     layout = layout2, 
     main = "Concentric Network Visualization with Domains with High Weights",
     main.cex = 5)
dev.off()


# Tracker Heatmap ---------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr) # Optional, but good practice

# 1. Get the top 15 domains from the custom list (broader)
# We filter, group by domain, sum hits, sort descending, and take the top 15 names
top_domains <- df %>%
  filter(TrackerBlackListXL == TRUE) %>%
  group_by(domain) %>%
  summarise(total_hits = sum(hits)) %>%
  arrange(desc(total_hits)) %>%
  slice_head(n = 15) %>%
  pull(domain)

# 2. Filter original df for these domains
df_top <- df %>%
  filter(domain %in% top_domains)

# 3. Get top 15 apps contributing to these
# Group by AppName, sum hits, sort descending, take top 15 names
top_apps <- df_top %>%
  group_by(AppName) %>%
  summarise(total_hits = sum(hits)) %>%
  arrange(desc(total_hits)) %>%
  slice_head(n = 15) %>%
  pull(AppName)

# Filter the dataset to only include these top apps
df_top <- df_top %>%
  filter(AppName %in% top_apps)

# 4. Prepare data for plotting
# Unlike Python's seaborn which needs a pivot table (matrix), 
# ggplot2 prefers "long" format data. We aggregate hits by AppName and domain here.
plot_data <- df_top %>%
  group_by(AppName, domain) %>%
  summarise(hits = sum(hits), .groups = 'drop')

# 5. Plot
ggplot(plot_data, aes(x = domain, y = AppName, fill = hits)) +
  geom_tile(color = "white") +                               # Create the heatmap tiles
  geom_text(aes(label = hits), color = "black", size = 3) +  # Add numbers (annot=True)
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +  # Use similar Yellow-Orange-Red palette
  labs(title = "Top 15 Apps vs Top 15 Suspected Tracker Domains (Hits)",
       x = "Domain",
       y = "App Name") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),       # Rotate x-axis labels
    panel.grid = element_blank()                             # Remove grid lines for cleaner look
  )

# Save the plot
ggsave("tracker_heatmap.png", width = 12, height = 8)


# App-Tracker Network Analysis --------------------------------------------

library(igraph)
library(ggraph)

# creating edge list for network graph
edge_list <- df %>%
  filter(TrackerBlackListXL == TRUE) %>%
  select(apps, domain, hits_sum) %>%
  unnest(apps = strsplit(apps, ", "))

# creating graph object
tracker_network <- graph_from_data_frame(edge_list, directed = TRUE)

# plotting network
ggraph(tracker_network, layout = 'fr') +
  geom_edge_link(aes(alpha = hits_sum), arrow = arrow(length = unit(2, 'mm'))) +
  geom_node_point(aes(size = degree(tracker_network)), color = "steelblue") +
  geom_node_text(aes(label = name), size = 3, repel = TRUE) +
  theme_void() +
  labs(title = "App-Tracker Network Relationships")


# Branch-TikTok-Spotify Diagram -------------------------------------------

# Install package if needed
if (!require("igraph")) install.packages("igraph")

library(igraph)

# 1. Create the data frame of connections (From -> To)
relations <- data.frame(
  from = c("USER",      "TikTok AD", "Branch",   "Spotify",     "Spotify",   "USER",    "Branch"),
  to   = c("TikTok AD", "Branch",    "Spotify",  "Branch",      "USER",      "Spotify", "TikTok AD"),
  label = c("clicks",   "triggers",   "opens",   "Reports back","SHOWS SONG","Interacts", "")
)


#relations <- data.frame(
 # from = c("Branch",   "Spotify", "USER",      "TikTok AD",     "Spotify",   "USER",    "Branch"),
#  to   = c("Spotify",  "Branch",  "TikTok AD", "Branch",          "USER",      "Spotify", "TikTok AD"),
#  label = c("opens",   "Reports back","clicks",   "triggers",   "SHOWS SONG","Interacts", "")
#)

# 2. Create the graph object
g <- graph_from_data_frame(relations, directed=TRUE)

# 3. specific Layout (so it looks like a circle/cycle)
l <- layout_in_circle(g)

# 4. Plotting
plot(g,
     layout = l,
     edge.label = relations$label,           # Add the text on lines
     edge.label.cex = 0.8,                   # Size of text on lines
     edge.arrow.size = 0.5,                  # Size of arrowheads
     vertex.color = c("lightblue", "white", "white", "#1DB954"), # Colors for nodes
     vertex.size = 30,                       # Size of bubbles
     vertex.label.color = "black",           # Text color inside bubbles
     vertex.label.cex = 0.9,                 # Text size inside bubbles
     main = "User Acquisition Flow"          # Title
)


# ### Fin du script ### ---------------------------------------------------
### Fin du script ###