### Network Traffic Analysis Visualization Script 2 ###

library(igraph)
library(ggplot2)  # For plotting
#install.packages("ggraph") # For network graph creation
library(ggraph)    # For network graph visualization
library(dplyr)    # For data manipulation


# 1. Enhanced Concentric Network Visualization Top24 Apps --------------------

# Clean Up previous variables
rm(g4, g4_filtered, layout_improved, layout_concentric_improved,
   vertex.size.values, vertex.label.values, layout2, layout_concentric2, unique_bundle_ids,
   weight_threshold)


# 1.1 Framework for Concentric Network Visual ---------------------------------

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
  g4 <- add_edges(g4, c(domain, domain_owner), weight = hits)#10) #2) #1)
}



# 1.2 Enhanced Features -------------------------------------------------------

# 1. Calculate traffic metrics for bundleID nodes
bundle_traffic <- sapply(unique_bundle_ids, function(b) {
  sum(filtered_df$hits[filtered_df$bundleID == b])
})

# Calculate traffic metrics for domain Owner nodes
domainOwner_traffic <- sapply(unique(filtered_df$DomainOwnerName), function(owner) {
  sum(filtered_df$hits[filtered_df$DomainOwnerName == owner])
})

# 2. Create color palette for bundleID based on traffic
library(RColorBrewer)
color_palette_apps <- colorRampPalette(c("red1", "darkred"))(5)
color_palette_dOwners <- colorRampPalette(c("darkgoldenrod2", "gold"))(5)

# 3. Enhanced color coding with traffic-based coloring for apps
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightblue1")
  } else if (name %in% unique_bundle_ids) {
    traffic <- bundle_traffic[name]
    quantile_rank <- cut(traffic, 
                         breaks = quantile(bundle_traffic, probs = 0:5/5),
                         labels = FALSE, include.lowest = TRUE)
    return(color_palette_apps[quantile_rank])
  } else if (name %in% filtered_df$domain) {
    return("snow2")
  #} else {
    #return("gold")
  } else if (name %in% filtered_df$DomainOwnerName) {
    traffic <- domainOwner_traffic[name]
    quantile_rank <- cut(traffic, 
                         breaks = quantile(domainOwner_traffic, probs = 0:5/5),
                         labels = FALSE, include.lowest = TRUE)
    return(color_palette_dOwners[quantile_rank])
  }
})

# 4. Calculate domain weights for filtering
domain_weights <- sapply(V(g4)$name[V(g4)$layer == 2], function(domain) {
  sum(E(g4)[.to(domain)]$weight, na.rm = TRUE)
})

# 5. Set weight threshold for domain visibility
#weight_threshold <- 40  # Adjust this value as needed
weight_threshold <- 60
#weight_threshold <- 80

# 6. Enhanced vertex sizes with better scaling
vertex.size.values <- ifelse(V(g4)$layer == 0, 20,           # User
                             ifelse(V(g4)$layer == 1, 13.5,         # Apps
                                    ifelse(V(g4)$layer == 2, 0,#1,            # Domains (small)
                                           ifelse(V(g4)$layer == 3, 25, 8))))     # Domain Owners


# 7. Smart label display based on weight threshold and layer
vertex.label.values <- sapply(V(g4)$name, function(name) {
  vertex_layer <- V(g4)$layer[V(g4)$name == name]
  
  # Always show User
  if (name == "User") return(name)
  
  # Always show bundleID (apps)
  if (vertex_layer == 1) return(name)
  
  # Show domains only if they meet weight threshold
  if (vertex_layer == 2) {
    total_weight <- sum(E(g4)[.to(name)]$weight, na.rm = TRUE)
    if (total_weight >= weight_threshold) {
      return(name)
    }
    return(NA)
  }
  
  # Always show domain owners
  if (vertex_layer == 3) return(name)
  
  return(name)
})


# 8. Label positioning - outer ring labels pushed outward
vertex.label.dist <- ifelse(V(g4)$layer == 3, 0,#1.8,   # Outer ring: labels outside
                            ifelse(V(g4)$layer == 2, 0.5,     # Domains: on node
                                   ifelse(V(g4)$layer == 1, 0, 0))) # Apps and User: on node

vertex.label.degree <- ifelse(V(g4)$layer == 2,  pi/4, # fixed direction
                              ifelse(V(g4)$layer == 1, 0,
                                     0))   
# 9. Label sizes by layer importance
vertex.label.cex <- ifelse(V(g4)$layer == 0, 1.5,#2.0,    # User: largest
                           ifelse(V(g4)$layer == 1, 0.8,#1,#1.4,    # Apps: large
                                  ifelse(V(g4)$layer == 2, 0.8,    # Domains: small
                                         0.9)))                     # Owners: medium-large

# 10. Font styling - bold for important nodes
vertex.label.font <- ifelse(V(g4)$layer == 0, 1,#2,     # User: bold
                            ifelse(V(g4)$layer == 1, 1,#2,     # Apps: bold
                                   1))                       # Others: normal

# 10b. Vertex Frame Color of Domain Nodes white
vertex.frame.color <- ifelse(V(g4)$layer == 0, adjustcolor("black", alpha.f = 0.5),  # User
                             ifelse(V(g4)$layer == 1, adjustcolor("black", alpha.f = 0.5),  # Apps
                                    #ifelse(V(g4)$layer == 2, adjustcolor("black", alpha.f = 0.2),  # Domain nodes get transparent frame
                                    ifelse(V(g4)$layer == 2, adjustcolor("white", alpha.f = 0.1),  # Domain nodes get transparent frame
                                           adjustcolor("black", alpha.f = 0.5))))  # Owners


# 11. Hierarchical edge filtering
edge_list <- as_edgelist(g4)
edge_weights <- E(g4)$weight
keep_edges <- rep(FALSE, length(edge_weights))

for (i in 1:nrow(edge_list)) {
  from_name <- edge_list[i, 1]
  to_name <- edge_list[i, 2]
  
  from_layer <- V(g4)$layer[V(g4)$name == from_name]
  to_layer <- V(g4)$layer[V(g4)$name == to_name]
  
  # User to bundleID: keep all
  if (from_layer == 0 && to_layer == 1) {
    keep_edges[i] <- TRUE
  }
  # bundleID to domain: keep top 80% by weight
  else if (from_layer == 1 && to_layer == 2) {
    threshold <- quantile(edge_weights, 0.2, na.rm = TRUE)
    keep_edges[i] <- edge_weights[i] >= threshold
  }
  # domain to owner: keep only if domain label is visible
  else if (from_layer == 2 && to_layer == 3) {
    #threshold <- quantile(edge_weights, 0.2, na.rm = TRUE)
    #keep_edges[i] <- edge_weights[i] >= threshold
    keep_edges[i] <- TRUE
    #domain <- from_name
    #domain_idx <- which(V(g4)$name == domain)
    #keep_edges[i] <- !is.na(vertex.label.values[domain_idx])
  }
  else {
    keep_edges[i] <- TRUE
  }
}

# Create filtered graph
g4_filtered <- delete_edges(g4, E(g4)[!keep_edges])


# 12. Edge transparency based on weight
edge.alpha <- E(g4_filtered)$weight / max(E(g4_filtered)$weight, na.rm = TRUE)
edge.alpha <- pmax(0.05, pmin(edge.alpha, 0.4))  # Clamp between 0.05 and 0.4
#edge.alpha <- pmax(0.05, pmin(edge.alpha, 0.2))  # Clamp between 0.05 and 0.4

#edge.color.values <- adjustcolor("gray30", alpha.f = edge.alpha)
# Apply adjustcolor to each alpha value individually
edge.color.values <- sapply(edge.alpha, function(alpha) {
  adjustcolor("gray30", alpha.f = alpha)
})


# 13. Improved concentric layout with better spacing
layout_concentric_improved <- function(g) {
  layers <- split(V(g), V(g)$layer)
  positions <- matrix(0, nrow = vcount(g), ncol = 2)
  rownames(positions) <- V(g)$name
  
  # Center - User
  positions["User", ] <- c(0, 0)
  
  # Define radii with increased spacing between layers
  #radii <- c(0, 2, 3.5, 5.5)
  radii <- c(0, 3.5, 6, 9)
  for (layer_num in 1:3) {
    layer_vertices <- layers[[layer_num + 1]]
    n <- length(layer_vertices)
    
    if (n > 0) {
      # Add offset to prevent perfect alignment
      angle_offset <- pi / (2 * n)
      angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] + angle_offset
      
      for (j in 1:n) {
        vertex_name <- layer_vertices[j]$name
        positions[vertex_name, ] <- radii[layer_num + 1] * c(cos(angles[j]), sin(angles[j]))
      }
    }
  }
  
  return(positions)
}

# Generate improved layout
layout_improved <- layout_concentric_improved(g4_filtered)

# 14. Create enhanced visualization
png("Output/Plots/concentric_network_enhanced.png", width = 2000, height = 2000, res = 150)

par(mar = c(1, 1, 3, 1))

plot(g4_filtered, 
     vertex.color = V(g4_filtered)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     vertex.label.cex = vertex.label.cex,
     #vertex.label.dist = vertex.label.dist,
     #vertex.label.degree = vertex.label.degree,
     vertex.label.font = vertex.label.font,
     vertex.size = vertex.size.values,
     vertex.frame.color = vertex.frame.color,
     edge.width = E(g4_filtered)$weight / max(E(g4_filtered)$weight, na.rm = TRUE) * 20, #30,
     #edge.color = edge.color.values,
     edge.curved = 0.1,  # Slight curve to reduce overlap
     layout = layout_improved, 
     main = "Concentric Network: User → Apps → Domains → Owners")

# 15. Add informative legend
legend("bottomright", 
       legend = c("User", "Mobile Apps (by traffic)", "Domains", "Domain Owners"),
       fill = c("lightblue1", "tomato1", "snow2", "gold"),
       cex = 1.2,
       title = "Node Types",
       border = "black",
       bty = "n")

# Add traffic indicator for apps
legend("topleft",
       legend = c("Highest Traffic", "", "", "", "Lowest Traffic"),
       fill = rev(color_palette_apps),
       cex = 1.0,
       title = "App Traffic Level",
       border = "black",
       bty = "n")

# Add traffic indicator for domain owners
legend("topright",
       legend = c("Highest Traffic", "", "", "", "Lowest Traffic"),
       fill = rev(color_palette_dOwners),
       cex = 1.0,
       title = "Domain Owner Traffic Level",
       border = "black",
       bty = "n")

dev.off()


# Top 10 Apps even smaller df ---------------------------------------------

rm(filtered_df)  # Clean up

# aggregating hits to find the most active apps and domains
# filtering to top 10 apps and top 50 domains to prevent visual clutter ("hairball" graph)
top10_apps <- df_all_trackers_XL_dType1 %>%
  group_by(AppName) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  slice_max(order_by = total_hits, n = 10) %>%
  pull(AppName)

filtered_df <- df_all_trackers_XL_dType1 %>%
  filter(AppName %in% top10_apps)

top50_domains <- filtered_df %>%
  group_by(domain) %>%
  summarise(total_hits = sum(hits, na.rm = TRUE)) %>%
  slice_max(order_by = total_hits, n = 50) %>%
  pull(domain)

filtered_df <- filtered_df %>%
  filter(domain %in% top50_domains)

filtered_df <- filtered_df %>%
  select(AppName, domain, hits, DomainOwnerName) %>%
  # rename AppName to bundleID
  rename(bundleID = AppName)


# 2. Enhanced Concentric Network Visualization Top10 Apps -----------------

# Clean Up previous variables
rm(g4, g4_filtered, layout_improved, layout_concentric_improved,
   vertex.size.values, vertex.label.values, layout2, layout_concentric2, unique_bundle_ids,
   weight_threshold)


# 2.1 Framework for Concentric Network Visual ---------------------------------

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
  g4 <- add_edges(g4, c(domain, domain_owner), weight = hits)#10) #2) #1)
}


# 2.2 Enhanced Features -------------------------------------------------------

# 1. Calculate traffic metrics for bundleID nodes
bundle_traffic <- sapply(unique_bundle_ids, function(b) {
  sum(filtered_df$hits[filtered_df$bundleID == b])
})

# Calculate traffic metrics for domain Owner nodes
domainOwner_traffic <- sapply(unique(filtered_df$DomainOwnerName), function(owner) {
  sum(filtered_df$hits[filtered_df$DomainOwnerName == owner])
})

# 2. Create color palette for bundleID based on traffic
library(RColorBrewer)
color_palette_apps <- colorRampPalette(c("red1", "darkred"))(5)
color_palette_dOwners <- colorRampPalette(c("darkgoldenrod2", "gold"))(5)

# 3. Enhanced color coding with traffic-based coloring for apps
V(g4)$color <- sapply(V(g4)$name, function(name) {
  if (name == "User") {
    return("lightblue1")
  } else if (name %in% unique_bundle_ids) {
    traffic <- bundle_traffic[name]
    quantile_rank <- cut(traffic, 
                         breaks = quantile(bundle_traffic, probs = 0:5/5),
                         labels = FALSE, include.lowest = TRUE)
    return(color_palette_apps[quantile_rank])
  } else if (name %in% filtered_df$domain) {
    return("snow2")
    #} else {
    #return("gold")
  } else if (name %in% filtered_df$DomainOwnerName) {
    traffic <- domainOwner_traffic[name]
    quantile_rank <- cut(traffic, 
                         breaks = quantile(domainOwner_traffic, probs = 0:5/5),
                         labels = FALSE, include.lowest = TRUE)
    return(color_palette_dOwners[quantile_rank])
  }
})

# 4. Calculate domain weights for filtering
domain_weights <- sapply(V(g4)$name[V(g4)$layer == 2], function(domain) {
  sum(E(g4)[.to(domain)]$weight, na.rm = TRUE)
})

# 5. Set weight threshold for domain visibility
#weight_threshold <- 15  # Adjust this value as needed
weight_threshold <- 20

# 6. Enhanced vertex sizes with better scaling
vertex.size.values <- ifelse(V(g4)$layer == 0, 20,           # User
                             ifelse(V(g4)$layer == 1, 13.5,         # Apps
                                    ifelse(V(g4)$layer == 2, 1,#1,            # Domains (small)
                                           ifelse(V(g4)$layer == 3, 25, 8))))     # Domain Owners


# 7. Smart label display based on weight threshold and layer
vertex.label.values <- sapply(V(g4)$name, function(name) {
  vertex_layer <- V(g4)$layer[V(g4)$name == name]
  
  # Always show User
  if (name == "User") return(name)
  
  # Always show bundleID (apps)
  if (vertex_layer == 1) return(name)
  
  # Show domains only if they meet weight threshold
  if (vertex_layer == 2) {
    total_weight <- sum(E(g4)[.to(name)]$weight, na.rm = TRUE)
    if (total_weight >= weight_threshold) {
      return(name)
    }
    return(NA)
  }
  
  # Always show domain owners
  if (vertex_layer == 3) return(name)
  
  return(name)
})


# 8. Label positioning - outer ring labels pushed outward
vertex.label.dist <- ifelse(V(g4)$layer == 3, 0,#1.8,   # Outer ring: labels outside
                            ifelse(V(g4)$layer == 2, 0.5,     # Domains: on node
                                   ifelse(V(g4)$layer == 1, 0, 0))) # Apps and User: on node

vertex.label.degree <- ifelse(V(g4)$layer == 2,  pi/4, # fixed direction
                              ifelse(V(g4)$layer == 1, 0,
                                     0))    

# 9. Label sizes by layer importance
vertex.label.cex <- ifelse(V(g4)$layer == 0, 1.5,#2.0,    # User: largest
                           ifelse(V(g4)$layer == 1, 0.8,#1,#1.4,    # Apps: large
                                  ifelse(V(g4)$layer == 2, 0.8,    # Domains: small
                                         0.9)))                     # Owners: medium-large

# 10. Font styling - bold for important nodes
vertex.label.font <- ifelse(V(g4)$layer == 0, 1,#2,     # User: bold
                            ifelse(V(g4)$layer == 1, 1,#2,     # Apps: bold
                                   1))                       # Others: normal

# 10b. Vertex Frame Color of Domain Nodes white
vertex.frame.color <- ifelse(V(g4)$layer == 0, adjustcolor("black", alpha.f = 0.5),  # User
                             ifelse(V(g4)$layer == 1, adjustcolor("black", alpha.f = 0.5),  # Apps
                                    #ifelse(V(g4)$layer == 2, adjustcolor("white", alpha.f = 0.0),  # Domain nodes get transparent frame
                                    ifelse(V(g4)$layer == 2, adjustcolor("black", alpha.f = 0.3),  # Domain nodes 
                                           adjustcolor("black", alpha.f = 0.5))))  # Owners


# 11. Hierarchical edge filtering
edge_list <- as_edgelist(g4)
edge_weights <- E(g4)$weight
keep_edges <- rep(FALSE, length(edge_weights))

for (i in 1:nrow(edge_list)) {
  from_name <- edge_list[i, 1]
  to_name <- edge_list[i, 2]
  
  from_layer <- V(g4)$layer[V(g4)$name == from_name]
  to_layer <- V(g4)$layer[V(g4)$name == to_name]
  
  # User to bundleID: keep all
  if (from_layer == 0 && to_layer == 1) {
    keep_edges[i] <- TRUE
  }
  # bundleID to domain: keep top 80% by weight
  else if (from_layer == 1 && to_layer == 2) {
    #threshold <- quantile(edge_weights, 0.2, na.rm = TRUE)
    #keep_edges[i] <- edge_weights[i] >= threshold
    keep_edges[i] <- TRUE
  }
  # domain to owner: keep only if domain label is visible
  else if (from_layer == 2 && to_layer == 3) {
    #threshold <- quantile(edge_weights, 0.2, na.rm = TRUE)
    #keep_edges[i] <- edge_weights[i] >= threshold
    keep_edges[i] <- TRUE
    #domain <- from_name
    #domain_idx <- which(V(g4)$name == domain)
    #keep_edges[i] <- !is.na(vertex.label.values[domain_idx])
  }
  else {
    keep_edges[i] <- TRUE
  }
}

# Create filtered graph
g4_filtered <- delete_edges(g4, E(g4)[!keep_edges])


# 12. Edge transparency based on weight
edge.alpha <- E(g4_filtered)$weight / max(E(g4_filtered)$weight, na.rm = TRUE)
#edge.alpha <- pmax(0.05, pmin(edge.alpha, 0.4))  # Clamp between 0.05 and 0.4
edge.alpha <- pmax(0.01, pmin(edge.alpha, 0.4))  # Clamp between 0.05 and 0.4

#edge.color.values <- adjustcolor("gray30", alpha.f = edge.alpha)
# Apply adjustcolor to each alpha value individually
edge.color.values <- sapply(edge.alpha, function(alpha) {
  adjustcolor("gray30", alpha.f = alpha)
})


# 13. Improved concentric layout with better spacing
layout_concentric_improved <- function(g) {
  layers <- split(V(g), V(g)$layer)
  positions <- matrix(0, nrow = vcount(g), ncol = 2)
  rownames(positions) <- V(g)$name
  
  # Center - User
  positions["User", ] <- c(0, 0)
  
  # Define radii with increased spacing between layers
  #radii <- c(0, 2, 3.5, 5.5)
  radii <- c(0, 3.5, 5, 7.5)
  #radii <- c(0, 2, 4.5, 6.5)
  for (layer_num in 1:3) {
    layer_vertices <- layers[[layer_num + 1]]
    n <- length(layer_vertices)
    
    if (n > 0) {
      # Add offset to prevent perfect alignment
      angle_offset <- pi / (2 * n)
      angles <- seq(0, 2 * pi, length.out = n + 1)[-(n + 1)] + angle_offset
      
      for (j in 1:n) {
        vertex_name <- layer_vertices[j]$name
        positions[vertex_name, ] <- radii[layer_num + 1] * c(cos(angles[j]), sin(angles[j]))
      }
    }
  }
  
  return(positions)
}

# Generate improved layout
layout_improved <- layout_concentric_improved(g4_filtered)

# 14. Create enhanced visualization
png("Output/Plots/concentric_network_enhanced_Top10.png", width = 2000, height = 2000, res = 150)

par(mar = c(1, 1, 3, 1))

plot(g4_filtered, 
     vertex.color = V(g4_filtered)$color, 
     vertex.label = vertex.label.values, 
     vertex.label.color = "black",
     vertex.label.cex = vertex.label.cex,
     vertex.label.dist = vertex.label.dist,
     #vertex.label.degree = vertex.label.degree,
     vertex.label.font = vertex.label.font,
     vertex.size = vertex.size.values,
     vertex.frame.color = vertex.frame.color,
     edge.width = E(g4_filtered)$weight / max(E(g4_filtered)$weight, na.rm = TRUE) * 10,#20,
     #edge.color = edge.color.values,
     edge.curved = 0.1,  # Slight curve to reduce overlap
     layout = layout_improved, 
     main = "Concentric Network: User → Apps → Domains → Owners")

# 15. Add informative legend
legend("bottomright", 
       legend = c("User", "Mobile Apps (by traffic)", "Domains", "Domain Owners"),
       fill = c("lightblue1", "tomato1", "snow2", "gold"),
       cex = 1.2,
       title = "Node Types",
       border = "black",
       bty = "n")

# Add traffic indicator for apps
legend("topleft",
       legend = c("Highest Traffic", "", "", "", "Lowest Traffic"),
       fill = rev(color_palette_apps),
       cex = 1.0,
       title = "App Traffic Level",
       border = "black",
       bty = "n")

# Add traffic indicator for domain owners
legend("topright",
       legend = c("Highest Traffic", "", "", "", "Lowest Traffic"),
       fill = rev(color_palette_dOwners),
       cex = 1.0,
       title = "Domain Owner Traffic Level",
       border = "black",
       bty = "n")

dev.off()


# Optional: Interactive visualization (requires networkD3 package) --------

# Uncomment to generate interactive HTML version
install.packages("networkD3")  # Uncomment if not installed
library(networkD3)
library(htmlwidgets)
 
# Convert to networkD3 format
d3_data <- igraph_to_networkD3(g4_filtered, group = V(g4_filtered)$layer)
 
# Add names to nodes
d3_data$nodes$group <- V(g4_filtered)$layer
# 
# Create interactive network
network <- forceNetwork(Links = d3_data$links, 
                        Nodes = d3_data$nodes,
                        Source = 'source', 
                        Target = 'target',
                        NodeID = 'name',
                        Group = 'group',
                        Value = 'value',
                        opacity = 0.9,
                        zoom = TRUE,
                        fontSize = 14,
                        fontFamily = "Arial",
                        charge = -100,
                        linkDistance = 50,
                        legend = TRUE)
 
saveWidget(network, "Output/Plots/concentric_network_interactive.html")

# Print summary statistics
cat("\n=== Network Summary ===\n")
cat("Total nodes:", vcount(g4_filtered), "\n")
cat("Total edges:", ecount(g4_filtered), "\n")
cat("Nodes by layer:\n")
cat("  User:", sum(V(g4_filtered)$layer == 0), "\n")
cat("  Apps:", sum(V(g4_filtered)$layer == 1), "\n")
cat("  Domains (visible):", sum(V(g4_filtered)$layer == 2 & !is.na(vertex.label.values)), "\n")
cat("  Domain Owners:", sum(V(g4_filtered)$layer == 3), "\n")
cat("Weight threshold applied:", weight_threshold, "\n")


# ### Fin du script ### ---------------------------------------------------
### Fin du script ###