### Data Preparation Script for Analysis ###

# 1. Loading the libraries ----------------------------------------------------------

# installing packages
install.packages("ndjson")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
library(ndjson) # For reading ndjson files
library(dplyr)
library(ggplot2)
library(tidyverse) # For data manipulation


# 2. Reading the data -----------------------------------------------------------

# Defining File Paths
file_path <- '/Users/nicolaswaser/New-project-GitHub-first/R/MSA II/Input Data'

## Loading the NDJSON files into R

# Distinction between Cross-Tracking ON and OFF and Unknown
df1_ct_off <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-04_T09_57_24_CT-OFF.ndjson"))

df2_ct_off <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-10_T00_01_39_CT-OFF.ndjson"))

df3_ct_off <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-16_T23_08_32_CT-OFF.ndjson"))

df4_ct_on <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-23_T23_50_52_CT-On.ndjson"))

df5_ct_on <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-03-03_T09_07_58_CT-On.ndjson"))

df6_ct_on <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-03-09_T22_53_00_CT-On.ndjson"))

df7_ct_on <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-03-16_T23_43_22_CT-On.ndjson"))

df8_ct_off <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-03-23_T22_51_37_CT-OFF.ndjson"))

df9_ct_off <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-03-30_T23_47_36_CT-OFF.ndjson"))

df10_ct_unknown <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-04-08_T10_30_17_CT-Unknown.ndjson"))

df11_ct_unknown <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-04-21_T16_55_54_CT-Unknown.ndjson"))
                                         

# Inspecting the data structure to get an overview of all variables
str(df1_ct_off)
head(df1_ct_off)
class(df1_ct_off)

str(df3_ct_off)
str(df11_ct_unknown)

# Printing column names of all data frames
colnames(df1_ct_off) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df2_ct_off) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df3_ct_off)
colnames(df4_ct_on)
colnames(df5_ct_on) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df6_ct_on) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df7_ct_on) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df8_ct_off) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df9_ct_off) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df10_ct_unknown) # 2 Missing columns: "broadcaster.identifier", "broadcaster.identifierType"
colnames(df11_ct_unknown) # 3 Missing columns: "outOfProcess", "broadcaster.identifier", "broadcaster.identifierType"


# 3. Tibble data -------------------------------------------------------------

# Turning data frames into tibbles for better readability 
library(tibble)


#is_tibble(data) # FALSE
#data_tibble <- as_tibble(data)
#is_tibble(data_tibble)
#head(data_tibble)
#class(data_tibble)

is_tibble(df1_ct_off) # FALSE
df1_ct_off_tibble <- as_tibble(df1_ct_off)
is_tibble(df1_ct_off_tibble) # TRUE
head(df1_ct_off_tibble)
class(df1_ct_off_tibble)

# function to convert all data frames to tibbles
convert_to_tibble <- function(df) {
  if (!is_tibble(df)) {
    return(as_tibble(df))
  } else {
    return(df)
  }
}

# Applying the function to all data frames
df1_ct_off_tibble <- convert_to_tibble(df1_ct_off)
df2_ct_off_tibble <- convert_to_tibble(df2_ct_off)
df3_ct_off_tibble <- convert_to_tibble(df3_ct_off)
df4_ct_on_tibble <- convert_to_tibble(df4_ct_on)
df5_ct_on_tibble <- convert_to_tibble(df5_ct_on)
df6_ct_on_tibble <- convert_to_tibble(df6_ct_on)
df7_ct_on_tibble <- convert_to_tibble(df7_ct_on)
df8_ct_off_tibble <- convert_to_tibble(df8_ct_off)
df9_ct_off_tibble <- convert_to_tibble(df9_ct_off)
df10_ct_unknown_tibble <- convert_to_tibble(df10_ct_unknown)
df11_ct_unknown_tibble <- convert_to_tibble(df11_ct_unknown)

head(df1_ct_off_tibble)
head(df3_ct_off_tibble)
head(df11_ct_unknown_tibble)


# 4. Unnest Data -------------------------------------------------------------

# Unnest data
#install.packages("tidyr")
library(tidyr)

#column_names <- names(data_tibble)
#print(column_names)
#data_unnested <- unnest(data_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", "category", 
 #                                             "identifier", "kind", "timeStamp", "type", "outOfProcess", "bundleID", "context", 
  #                                            "contextVerificationType", "domain", "domainClassification", "domainOwner", "domainType", 
   #                                           "firstTimeStamp", "hits", "initiatedType"))
#head(data_unnested)
#class(data_unnested)
#summary(data_unnested)
#glimpse(data_unnested)

names(df1_ct_off_tibble)
df1_ct_off_unnested <- unnest(df1_ct_off_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
head(df1_ct_off_unnested)
class(df1_ct_off_unnested)
summary(df1_ct_off_unnested)
glimpse(df1_ct_off_unnested)

# Unnesting all data frames
names(df2_ct_off_tibble)
df2_ct_off_unnested <- unnest(df2_ct_off_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df3_ct_off_tibble)
df3_ct_off_unnested <- unnest(df3_ct_off_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "broadcaster.identifier", "broadcaster.identifierType",
                                                            "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df4_ct_on_tibble)
df4_ct_on_unnested <- unnest(df4_ct_on_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "broadcaster.identifier", "broadcaster.identifierType", "outOfProcess",
                                                            "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df5_ct_on_tibble)
df5_ct_on_unnested <- unnest(df5_ct_on_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df6_ct_on_tibble)
df6_ct_on_unnested <- unnest(df6_ct_on_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df7_ct_on_tibble)
df7_ct_on_unnested <- unnest(df7_ct_on_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df8_ct_off_tibble)
df8_ct_off_unnested <- unnest(df8_ct_off_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df9_ct_off_tibble)
df9_ct_off_unnested <- unnest(df9_ct_off_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df10_ct_unknown_tibble)
df10_ct_unknown_unnested <- unnest(df10_ct_unknown_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))
names(df11_ct_unknown_tibble)
df11_ct_unknown_unnested <- unnest(df11_ct_unknown_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                            "category", "identifier", "kind", "timeStamp", "type", 
                                                            "bundleID", "context", "contextVerificationType", 
                                                            "domain", "domainClassification", "domainOwner", "domainType", 
                                                            "firstTimeStamp", "hits", "initiatedType"))



# 5. Selecting only rows and columns relevant to "networkActivity" ----------------------------------------

library(dplyr)
# Selecting only the relevant columns for analysis that concern "networkActivity"
df1_ct_off_relevant <- df1_ct_off_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df2_ct_off_relevant <- df2_ct_off_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df3_ct_off_relevant <- df3_ct_off_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df4_ct_on_relevant <- df4_ct_on_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df5_ct_on_relevant <- df5_ct_on_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df6_ct_on_relevant <- df6_ct_on_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df7_ct_on_relevant <- df7_ct_on_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df8_ct_off_relevant <- df8_ct_off_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df9_ct_off_relevant <- df9_ct_off_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df10_ct_unknown_relevant <- df10_ct_unknown_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

df11_ct_unknown_relevant <- df11_ct_unknown_unnested %>%
  filter(type == "networkActivity") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)


# 6. Getting an overview by showing some rankings ----------------------------

# specified domain accessed during network activity
# CT OFF
df1_ct_off_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df2_ct_off_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df3_ct_off_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# CT ON
df4_ct_on_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df5_ct_on_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df6_ct_on_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df7_ct_on_relevant %>%
  group_by(domain) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)


# application responsible for network activity
# CT OFF
df1_ct_off_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df2_ct_off_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df3_ct_off_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df4_ct_on_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df5_ct_on_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df6_ct_on_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df7_ct_on_relevant %>%
  group_by(bundleID) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)


# domain owner
# CT OFF
df1_ct_off_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df2_ct_off_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df3_ct_off_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

# CT ON
df4_ct_on_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df5_ct_on_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df6_ct_on_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)

df7_ct_on_relevant %>%
  group_by(domainOwner) %>%
  summarise(total_accesses = n()) %>%
  arrange(desc(total_accesses)) %>%
  print(n=20)


# 7. Merging data frames -----------------------------------------------------

# Merging all relevant data frames into one
merged_data_all <- bind_rows(
  df1_ct_off_relevant,
  df2_ct_off_relevant,
  df3_ct_off_relevant,
  df4_ct_on_relevant,
  df5_ct_on_relevant,
  df6_ct_on_relevant,
  df7_ct_on_relevant,
  df8_ct_off_relevant,
  df9_ct_off_relevant,
  df10_ct_unknown_relevant,
  df11_ct_unknown_relevant
)

# 2 separate dfs with distinction between Cross-Tracking ON and OFF

1646 + 1879 + 2057

2827 + 1255 + 996 + 862

merged_data_ct_off <- bind_rows(
  df1_ct_off_relevant,
  df2_ct_off_relevant,
  df3_ct_off_relevant,
  #df8_ct_off_relevant,
  #df9_ct_off_relevant
)

merged_data_ct_on <- bind_rows(
  df4_ct_on_relevant,
  df5_ct_on_relevant,
  df6_ct_on_relevant,
  df7_ct_on_relevant
)

#merged_data_ct_unknown <- bind_rows(
  #df10_ct_unknown_relevant,
  #df11_ct_unknown_relevant
#)


## rm df to free up memory
rm(df1_ct_off, df2_ct_off, df3_ct_off, df4_ct_on, df5_ct_on, df6_ct_on, df7_ct_on,
   df8_ct_off, df9_ct_off, df10_ct_unknown, df11_ct_unknown,
   df1_ct_off_tibble, df2_ct_off_tibble, df3_ct_off_tibble, df4_ct_on_tibble, df5_ct_on_tibble,
   df6_ct_on_tibble, df7_ct_on_tibble, df8_ct_off_tibble, df9_ct_off_tibble,
   df10_ct_unknown_tibble, df11_ct_unknown_tibble)

rm(category_data)


# 8. Extra Df's for data subject access requests ----------------------------------

library(dplyr)
# Merging all relevant data frames into one
merged_data_all_0 <- bind_rows(
  df1_ct_off_relevant,
  df2_ct_off_relevant,
  df3_ct_off_relevant,
  df4_ct_on_relevant,
  df5_ct_on_relevant,
  df6_ct_on_relevant,
  df7_ct_on_relevant,
  df8_ct_off_relevant,
  df9_ct_off_relevant,
  df10_ct_unknown_relevant,
  df11_ct_unknown_relevant
)

# filter for spotify only
merged_data_spotify <- merged_data_all_0 %>%
  filter(bundleID == "com.spotify.client")

merged_data_spotify_info <- merged_data_all_more_info %>%
  filter(bundleID == "com.spotify.client") %>%
  select(DomainOwnerName, AppName, domain, domainType, TrackerBlackList, TrackerBlackListXL, 
         firstTimeStamp, timeStamp, hits, initiatedType, domainClassification)
# save as .csv file
write.csv(merged_data_spotify, "Output/Tables/merged_data_spotify.csv", row.names = TRUE)
rm(merged_data_spotify)
rm(merged_data_spotify_info)

# filter for strava only
merged_data_strava <- merged_data_all_0 %>%
  filter(bundleID == "com.strava.stravaride")
write.csv(merged_data_strava, "Output/Tables/merged_data_strava.csv", row.names = TRUE)
rm(merged_data_strava)

# filter for ricardo only
merged_data_ricardo <- merged_data_all_0 %>%
  filter(bundleID == "swiss.ricardo.iphone")
write.csv(merged_data_ricardo, "Output/Tables/merged_data_ricardo.csv", row.names = TRUE)
rm(merged_data_ricardo)

# filter for tutti only
merged_data_tutti <- merged_data_all_0 %>%
  filter(bundleID == "ch.tutti.iphone")
write.csv(merged_data_tutti, "Output/Tables/merged_data_tutti.csv", row.names = TRUE)
rm(merged_data_tutti)

# filter for reddit only
merged_data_reddit <- merged_data_all_0 %>%
  filter(bundleID == "com.reddit.Reddit")
write.csv(merged_data_reddit, "Output/Tables/merged_data_reddit.csv", row.names = TRUE)
rm(merged_data_reddit)

## filter for domains that contain "branch" in their name
library(stringr)
merged_data_branch_io <- merged_data_all_0 %>%
  filter(str_detect(domain, "branch"))
write.csv(merged_data_branch_io, "Output/Tables/merged_data_branch_io.csv", row.names = TRUE)
rm(merged_data_branch_io)


# 9. Extra Dfs for Spotify Analysis Sep - Nov 2025 ---------------------------------------

library(ndjson) # For reading ndjson files
library(dplyr)
library(tidyverse) 

# Defining File Path
file_path_spotify <- '/Users/nicolaswaser/New-project-GitHub-first/R/MSA II/Input Data/Spotify Extra Dfs'

## Loading the NDJSON files into R

df_spotify1 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-09-07T22_31_11.ndjson"))

df_spotify2 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-09-15T00_38_37.ndjson"))

df_spotify3 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-09-21T23_46_45.ndjson"))

df_spotify4 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-09-28T23_19_35.ndjson"))

df_spotify5 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-10-05T23_03_32.ndjson"))

df_spotify6 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-10-12T22_59_05.ndjson"))

df_spotify7 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-10-19T22_39_39.ndjson"))

df_spotify8 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-10-26T22_50_08.ndjson"))

df_spotify9 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-11-02T23_13_18.ndjson"))

df_spotify10 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-11-09T23_04_53.ndjson"))

df_spotify11 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-11-17T01_49_13.ndjson"))

df_spotify12 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-11-23T22_40_56.ndjson"))

df_spotify13 <- ndjson::stream_in(paste0(file_path_spotify, "/App_Privacy_Report_v4_2025-11-30T23_06_59.ndjson"))


## Merging Spotify data frames into one -------------------------------------

df_spotify_merged_all <- bind_rows(
  df_spotify1,
  df_spotify2,
  df_spotify3,
  df_spotify4,
  df_spotify5,
  df_spotify6,
  df_spotify7,
  df_spotify8,
  df_spotify9,
  df_spotify10,
  df_spotify11,
  df_spotify12,
  df_spotify13
)

rm(df_spotify1, df_spotify2, df_spotify3, df_spotify4, df_spotify5,
   df_spotify6, df_spotify7, df_spotify8, df_spotify9, df_spotify10,
   df_spotify11, df_spotify12, df_spotify13)

## convert to tibble
is_tibble(df_spotify_merged_all) # FALSE
df_spotify_merged_all_tibble <- convert_to_tibble(df_spotify_merged_all)
is_tibble(df_spotify_merged_all_tibble) # TRUE

## unnest
library(tidyr)
names(df_spotify_merged_all_tibble)
df_spotify_merged_all_unnest <- unnest(df_spotify_merged_all_tibble, cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                                          "category", "identifier", "kind", "timeStamp", "type", 
                                                          "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                                          "domain", "domainClassification", "domainOwner", "domainType", 
                                                          "firstTimeStamp", "hits", "initiatedType"))

## selecting relevant columns
# Selecting only the relevant columns for analysis that concern "networkActivity"
df_spotify_merged_all_relevant <- df_spotify_merged_all_unnest %>%
  #filter() %>%
  filter(type == "networkActivity") %>%
  filter(bundleID == "com.spotify.client") %>%
  select(firstTimeStamp, timeStamp, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)
#rm(df_spotify_merged_all_relevant) 
rm(df_spotify_merged_all, df_spotify_merged_all_tibble)#, df_spotify_merged_all_unnest)

#save as .csv file
write.csv(df_spotify_merged_all_relevant, "Output/Tables/df_spotify_merged_all_Sep_Nov_25.csv", row.names = TRUE)


# 10. Df for 25.2.25 ------------------------------------

df_spotify1 <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-10_T00_01_39_CT-OFF.ndjson"))

library(tibble)
is_tibble(df_spotify1) # FALSE
df_spotify1_tibble <- convert_to_tibble(df_spotify1)
is_tibble(df_spotify1_tibble) # TRUE

## unnest
library(tidyr)
names(df_spotify1_tibble)
df_spotify1_unnest <- unnest(df_spotify1_tibble, 
                             cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                      "category", "identifier", "kind", "timeStamp", "type", 
                                      "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                      "domain", "domainClassification", "domainOwner", "domainType", 
                                      "firstTimeStamp", "hits", "initiatedType"))

## selecting relevant columns
library(dplyr)
# Selecting only the relevant columns for analysis that concern "networkActivity"
df_spotify1_relevant <- df_spotify1_unnest %>%
  #filter(type == "networkActivity") %>%
  filter(bundleID == "com.spotify.client") %>%
  select(firstTimeStamp, timeStamp, type, hits,
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

rm(df_spotify1, df_spotify1_tibble, df_spotify1_unnest) 

write.csv(df_spotify1_relevant, "Output/Tables/df_spotify1_Feb_25.csv", row.names = TRUE)
#rm(df_spotify1_relevant)


# 11. Df for 7.2.25 -----------------------------------------------

df_spotify1 <- ndjson::stream_in(paste0(file_path, "/App_Privacy_Report_v4_2025-02-10_T00_01_39_CT-OFF.ndjson"))

library(tibble)
is_tibble(df_spotify1) # FALSE
df_spotify1_tibble <- convert_to_tibble(df_spotify1)
is_tibble(df_spotify1_tibble) # TRUE

## unnest
library(tidyr)
names(df_spotify1_tibble)
df_spotify1_unnest <- unnest(df_spotify1_tibble, 
                             cols = c("accessCount", "accessor.identifier", "accessor.identifierType", 
                                      "category", "identifier", "kind", "timeStamp", "type", 
                                      "outOfProcess", "bundleID", "context", "contextVerificationType", 
                                      "domain", "domainClassification", "domainOwner", "domainType", 
                                      "firstTimeStamp", "hits", "initiatedType"))

## selecting relevant columns
library(dplyr)
# Selecting only the relevant columns for analysis that concern "networkActivity"
# Date/Time:           2025-02-07 07:13:03.0953 +0100
# Launch Time:         2025-02-07 07:12:57.0705 +0100
df_spotify1_timestamp <- df_spotify1_unnest %>%
  # filter time stamp 2025-02-07 06:12 - 08:14
  filter(timeStamp >= "2025-02-07T06:12:00.000+0100" & timeStamp <= "2025-02-07T08:14:00.000+0100") %>%
  #filter(type == "networkActivity") %>%
  #filter(bundleID == "com.spotify.client") %>%
  select(firstTimeStamp, timeStamp, type, hits, accessor.identifier, category, kind, 
         bundleID, domain, domainOwner, domainType, domainClassification,
         initiatedType)

rm(df_spotify1, df_spotify1_tibble, df_spotify1_unnest) 

write.csv(df_spotify1_timestamp, "Output/Tables/df_spotify1_Feb_25_6-8.csv", row.names = TRUE)
rm(df_spotify1_timestamp)


### Fin du script ---------------------------------------------------
### Fin du script ###