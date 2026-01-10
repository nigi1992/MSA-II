### Network Traffic Analysis Visualization Script ###
# Network Analysis Visualization -----------------------------------------------

# Filter down df for Visualizations

## Create new df with only tracker entries
df_all_trackers_XL_dType1 <- merged_data_all_more_info %>% # (True/False Positives and False Negatives)
  filter(TrackerBlackListXL == TRUE | domainType == 1) %>%
  select(AppName, domain, DomainOwnerName, hits, domainType, TrackerBlackListXL)

table(df_all_trackers_XL_dType1$domainType, df_all_trackers_XL_dType1$TrackerBlackListXL)

# Number of unique domains in df
length(unique(df_all_trackers_XL_dType1$domain))  # 901 unique domains

# Number of unique apps in df
length(unique(df_all_trackers_XL_dType1$AppName))  # 150 unique apps

# Number of unique DomainOwnerName in df
length(unique(df_all_trackers_XL_dType1$DomainOwnerName))  # 90

# Next step: Filter down to small enough number of apps and domains for visualization!!!!


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