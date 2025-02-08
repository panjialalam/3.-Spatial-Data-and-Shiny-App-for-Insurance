###############################################################
## Title: Web Scrap
## Author: Panji Al 'Alam
## Email: panjialalam@outlook.com
################################################################

##--------------------------------------------------------------
## Section 1: Setting up working directory
##--------------------------------------------------------------
rm(list = ls())

# Loading R Packages
RPackages <- c("tidyverse", "rvest", "stringr", "lubridate", 
               "readr", "purrr", "httr", "tidytext", "textdata", "sentimentr", 
               "igraph", "ggraph", "SnowballC", "udpipe")
lapply(RPackages, library, character.only = TRUE)

# Setting working directory
paths <- ("/Users/panjialalam/Documents/GitHub/3.-Spatial-Data-and-Shiny-App-for-Insurance/1. Scripts")
setwd(paths)

##--------------------------------------------------------------
## Section 1: Web Scrap 
##--------------------------------------------------------------
# This part is to pull the raw text data from website

# ------------(SETTING UP WEBSITE)------------
# Website base
article_data <- list()
# Checking number of pages
for (npage in seq(from = 1, to = 100, by = 1)){
  page_url <- "https://www.whitehouse.gov/briefing-room/statements-releases/page/"
  url_web <- paste0(page_url,npage,"/")
  page <- read_html(url_web)
  
  # ------------(SCRAPING ARTICLES)------------
  # Scraping Articles Title
  art_title <- page |> html_nodes(".news-item__title") |> html_text(trim = T)
  # Article's Date
  art_date <- page |> 
    html_elements(".posted-on.entry-date.published.updated") |> 
    html_text(trim = TRUE)
  # Article's Link
  article_link <- page |> html_nodes(".news-item__title-container") |> html_nodes("a") |> 
    html_attr("href") |> sapply(trimws)
  # Storing all elements into a list
  article_data[[npage]] <- list(
    article_title = art_title, 
    article_date = art_date, 
    article_link = article_link
  )
  # ------------(WARNING MESSAGE)------------
  print(paste0("Still working on page ", npage))
}

# Binding dataset
data_WH <- bind_rows(article_data)

##--------------------------------------------------------------
## Section 2: Filtering Title
##--------------------------------------------------------------
# Listing word to detect
word_to_detect <- c("health insurance", "health care", "health coverage")
word_pattern <- paste(word_to_detect, collapse = "|")

# Filtering dataset
HCCover_dta <- data_WH |>
  filter(str_detect(article_title, regex (word_pattern, ignore_case = TRUE)))

##--------------------------------------------------------------
## Section 3: Scraping Content
##--------------------------------------------------------------
# On this section, we'll scrap the content of each website that we have list above. We'll only focus on 

# ------------(SCRAPING CONTENT)------------
page_links <- HCCover_dta$article_link
article_page <- list()

# This part is to scrape the content
for(link in 1: length(page_links)){
  article_read <- read_html(page_links[link])
  # Extracting content
  article_contents <- article_read |>
    html_elements("p") |> html_text(trim = T) |> paste(collapse = " ")
  article_page[[link]] <- list(
    content = article_contents
  )
  print(paste0("Working on Link number ", link))
}

# Binding dataset
content_dta <- bind_rows(article_page)

##--------------------------------------------------------------
## Section 4: Combining dataset
##--------------------------------------------------------------
# On this part, we'll combine two dataset, HCCover_dta and content_dta. We'll also do some formatting. 
WH_fulldata <- cbind(HCCover_dta, content_dta)

# Cleaning Dataset
WH_full_dta <- WH_fulldata |>
  mutate(content = as.character(content),
         content = str_replace_all(content, pattern = "[\n\t]", 
                                   replacement = ""),
         content = str_replace(
           content,
           "The White House1600 Pennsylvania Ave NWWashington, DC 20500",
           ""),
         dates = mdy(article_date))

# Writing csv for the text data
write_csv(WH_full_dta, "White_House_Statements.csv")
