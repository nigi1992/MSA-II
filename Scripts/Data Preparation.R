### Data Preparation Script for Analysis ###


# Loading the libraries ----------------------------------------------------------

# installing packages
install.packages("ndjson")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyverse")
library(ndjson) # For reading ndjson files
library(dplyr)
library(ggplot2)
library(tidyverse) # For data manipulation

# Reading the data -----------------------------------------------------------

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


# Tibble data -------------------------------------------------------------

# Turning data frames into tibbles for better readability 
library(tibble)


#is_tibble(data) # FALSE
#data_tibble <- as_tibble(data)
#is_tibble(data_tibble)
#head(data_tibble)
#class(data_tibble)



# Unnest Data -------------------------------------------------------------

# Unnest data
install.packages("tidyr")
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

