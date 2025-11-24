### Code Leftovers ###

## Upload EasyList EasyPrivacy ------------------------

library(httr)
library(readr)

# URL of the blacklist
easylist_easyprivacy_url <- "https://easylist.to/easylist/easyprivacy.txt"

# download the file as plain text
easylist_easyprivacy_txt <- content(GET(easylist_easyprivacy_url), as = "text")

# convert into vector of domains
easylist_easyprivacy_domains <- read_lines(I(easylist_easyprivacy_txt))

# make into a data frame
easylist_easyprivacy_df <- data.frame(domain = easylist_easyprivacy_domains, stringsAsFactors = FALSE)


## Data Cleaning EasyList-----------------------------------------------------------
library(dplyr)
# Clean the domains (remove comments, wildcards, etc.)
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df %>%
  slice(-1:-18) %>% # remove first 17 lines (header info)
  
  filter(!str_starts(domain, "!")) %>%  # remove comments
  filter(!str_detect(domain, "\\*\\*\\*")) %>%  # remove comments
  filter(!str_starts(domain, "\\@\\@")) %>%  # remove exceptions 
  filter(!str_detect(domain, "\\*")) %>% # remove wildcards
  #filter(!str_starts(domain, "/")) %>%  # remove rules starting with /
  filter(!str_detect(domain, ";")) %>%  # remove anything with ;
  #filter(!str_detect(domain, "?")) %>%  # remove anything with ?
  #filter(!str_detect(domain, "=")) %>%  # remove anything with =
  
  mutate(domain = str_remove_all(domain, "^\\/\\/")) %>%     # remove //
  mutate(domain = str_remove_all(domain, "^\\/")) %>%     # remove /
  mutate(domain = str_remove_all(domain, "^:\\|\\|")) %>% # remove leading :||
  mutate(domain = str_remove_all(domain, "^\\:")) %>%        # remove :
  mutate(domain = str_remove_all(domain, "^\\|\\|")) %>% # remove leading ||
  mutate(domain = str_remove_all(domain, "^\\|")) %>%    # remove leading |
  
  mutate(domain = str_remove_all(domain, "^\\.")) %>%        # remove .
  mutate(domain = str_remove_all(domain, "^\\_\\_")) %>%     # remove _ _
  mutate(domain = str_remove_all(domain, "^\\_")) %>%     # remove _
  mutate(domain = str_remove_all(domain, "^\\?")) %>%     # remove ?
  mutate(domain = str_remove_all(domain, "^\\%")) %>%     # remove %
  mutate(domain = str_remove_all(domain, "^\\&")) %>%        # remove &
  mutate(domain = str_remove_all(domain, "^\\&\\&")) %>%       # remove &&
  mutate(domain = str_remove_all(domain, "^\\/\\/")) %>%     # remove //
  
  mutate(domain = str_remove(domain, "^www\\.")) %>%     # remove leading www.
  mutate(domain = str_remove(domain, "^http\\:\\/\\/")) %>%     # remove leading http://
  mutate(domain = str_remove(domain, "^https\\:\\/\\/")) %>% # remove leading https://
  
  mutate(domain = str_remove_all(domain, "^\\^")) %>%     # remove ^
  mutate(domain = str_remove_all(domain, "^\\&")) %>%     # remove &
  
  mutate(domain = str_remove_all(domain, "\\^$")) %>%    # remove trailing ^
  mutate(domain = str_remove_all(domain, "\\^\\$third-party")) %>%    # remove trailing ^$ third-party
  mutate(domain = str_remove_all(domain, ",domain.*$")) %>%    # remove ,domain ..
  mutate(domain = str_remove_all(domain, ",xmlhttprequest.*$")) %>%    # remove ,xmlhttprequest ..
  mutate(domain = str_remove_all(domain, "/.*$")) %>%     # remove anything after /
  mutate(domain = str_remove_all(domain, "#+.*$")) %>%     # remove anything after #
  mutate(domain = str_remove_all(domain, "\\?.*$")) %>%     # remove anything after ?
  mutate(domain = str_remove_all(domain, "\\_.*$")) %>%     # remove anything after _
  mutate(domain = str_remove_all(domain, "\\=.*$")) %>%     # remove anything after =
  mutate(domain = str_remove_all(domain, "\\$.*$")) %>%     # remove anything after $
  mutate(domain = str_remove_all(domain, "\\~.*$")) %>%     # remove anything after ~
  mutate(domain = str_remove_all(domain, "\\%.*$")) %>%     # remove anything after %
  mutate(domain = str_remove_all(domain, "\\^$")) %>%    # remove trailing ^
  mutate(domain = str_remove_all(domain, "\\.$")) %>%    # remove trailing .
  mutate(domain = str_to_lower(domain)) #%>%               # lowercase
#filter(!str_starts(domain, "[")) %>%  # remove section headers

# remove empty rows
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df_filtered %>%
  filter(domain != "")

# remove duplicates
easylist_easyprivacy_df_filtered <- easylist_easyprivacy_df_filtered %>%
  distinct(domain, .keep_all = TRUE)


rm(easylist_easyprivacy__domains, easylist_easyprivacy_txt, easylist_easyprivacy_url)

## Web-Parser for Disconnect.me JSON-file -------------------------------------------
#library(jsonlite)

#disconnect_url <- "https://raw.githubusercontent.com/disconnectme/disconnect-tracking-protection/master/services.json"
#disconnect_json <- fromJSON(disconnect_url)

# assuming JSON is a simple array of domains
#disconnect_json <- data.frame(domain = unlist(disconnect_json), stringsAsFactors = FALSE)

# keep only the domain column
#disconnect_json <- disconnect_json %>%
#slice(-1:-1) %>% # remove first line (header info)
#select(domain) %>%
#mutate(domain = clean_domains(domain)) %>%
#filter(domain != "")

rm(disconnect_json)
rm(disconnect_url)

## example code for reading in blacklists and url --------------------------

## upload:
# reading a plain text blacklist
#blacklist <- readLines("tracker_blacklist.txt")

# convert to a data frame
#blacklist_df <- data.frame(domain = blacklist, stringsAsFactors = FALSE)

#library(readr)

# reading a csv file with a column 'domain'
#blacklist_df <- read_csv("tracker_blacklist.csv")

#library(jsonlite)

# reading json file
#blacklist_json <- fromJSON("tracker_blacklist.json")

# assume JSON has a vector/list of domains
#blacklist_df <- data.frame(domain = blacklist_json, stringsAsFactors = FALSE)

#library(dplyr)
#library(stringr)

## Data Cleaning
# lowercase all domains and remove leading "www."
#blacklist_df <- blacklist_df %>%
# mutate(domain = str_to_lower(domain),
#       domain = str_remove(domain, "^www\\."))

#networkActivity_df <- networkActivity_df %>%
#  mutate(domain = str_to_lower(domain),
#        domain = str_remove(domain, "^www\\."))

## Cross-referencing
# find all network activity entries that match blacklist
#matched_activity <- networkActivity_df %>%
# semi_join(blacklist_df, by = "domain")

# check the results
#print(matched_activity, n = 50)

# Or: add a tracker flag to the network activity data
#networkActivity_df <- networkActivity_df %>%
# mutate(is_tracker = domain %in% blacklist_df$domain)

## Summary
#tracker_summary <- networkActivity_df %>%
# filter(is_tracker) %>%
#group_by(domain) %>%
#summarise(total_hits = n(), .groups = "drop") %>%
#arrange(desc(total_hits))

#print(tracker_summary, n = 20)


## Example Scrapping ---------------------------------------------------------------

library(httr)
library(readr)
# txt
# URL of the blacklist
blacklist_url <- "https://example.com/tracker_blacklist.txt"

# download the file as plain text
blacklist_txt <- content(GET(blacklist_url), as = "text")

# convert into vector of domains
blacklist_domains <- read_lines(I(blacklist_txt))

# make into a data frame
blacklist_df <- data.frame(domain = blacklist_domains, stringsAsFactors = FALSE)

## csv
library(readr)

blacklist_url <- "https://example.com/tracker_blacklist.csv"
blacklist_df <- read_csv(blacklist_url)

## json
library(jsonlite)

blacklist_url <- "https://example.com/tracker_blacklist.json"
blacklist_json <- fromJSON(blacklist_url)

# assuming JSON is a simple array of domains
blacklist_df <- data.frame(domain = unlist(blacklist_json), stringsAsFactors = FALSE)

## embedded in html
library(rvest)
library(dplyr)

blacklist_url <- "https://example.com/blacklist-page"

# read page
page <- read_html(blacklist_url)

# extract table or text nodes
blacklist_df <- page %>%
  html_nodes("table") %>%   # adjust selector if necessary
  html_table() %>%
  .[[1]] %>%
  rename(domain = 1)        # assume first column has domains

## mutliple lists
# assuming multiple lists in different data frames: bl1, bl2, bl3
combined_blacklist <- bind_rows(bl1, bl2, bl3) %>%
  distinct(domain, .keep_all = TRUE)

# automation
library(purrr)
library(dplyr)
library(readr)

urls <- c(
  "https://example.com/tracker_blacklist.txt",
  "https://example.com/tracker_blacklist.csv"
)

# function to read text or csv automatically
read_blacklist <- function(url) {
  if (grepl("\\.csv$", url)) {
    df <- read_csv(url)
  } else {
    txt <- read_lines(url)
    df <- data.frame(domain = txt, stringsAsFactors = FALSE)
  }
  return(df)
}

combined_blacklist <- map_df(urls, read_blacklist) %>%
  distinct(domain, .keep_all = TRUE)




# ### Fin du script ### ---------------------------------------------------
### Fin du script ###