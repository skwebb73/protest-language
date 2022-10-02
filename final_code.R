###Protest Language in the News
###For Data in Linguistics course at UNC-Chapel Hill
###By: Susie Webb and Claire Bradley 

### Load in packages
library(xml2)
library(rvest)
library(tidyverse)
library(stringr)

### Read in links - The Oregonian 
links_oregonian <- read.csv("oregonian.csv") #loaded the list of links to the webpages
links_oregonian_v <- c(links_oregonian$Links)

### Scrape text - The Oregonian 
oregonian <- c() # created empty vector
for(i in links_oregonian_v){ # for loop to run through each of the links, pull text and store it
  r <- read_html(i) %>%
    html_node("div.article__story") %>%
    html_text()
  oregonian <- append(oregonian, r)
}

scraped_text_oregonion <- as.data.frame(links_oregonian_v, oregonian) # create data frame with all the 
# links and their text side by side 

### Read in links - The Oklahoman 
links_oklahoman <- read.csv("oklahoman.csv") #loaded the list of links to the webpages
links_oklahoman_v <- c(links_oklahoman$Links)

### Scrape text - The Oklahoman 
oklahoman <- c() # created empty vector
for(i in links_oklahoman_v){ # for loop to run through each of the links, pull text and store it
  r <- read_html(i) %>%
    html_node("body > div.gnt_cw_w > main > article > div.gnt_ar_b") %>%
    html_text()
  oklahoman <- append(oklahoman, r)
}

scraped_text_oklahoman <- as.data.frame(links_oklahoman_v, oklahoman) # create data frame with all the 
# links and their text side by side

### Format text - The Oregonian
oregonian_text <- paste(oregonian, collapse = " ") # combine text from all articles into one vector
oregonian_text <- tolower(oregonian_text) # make everything lower case

### Format text - The Oklahoman 
oklahoman_text <- paste(oklahoman, collapse = " ") # combine text from all articles into one vector
oklahoman_text <- tolower(oklahoman_text) # make everything lower case

### Analyze text

# create regular expressions to search text
regex_nonviolent <- "\\b(protest((or)|(s)|(ors))?|demonstrat((or)|(ors)|(ion)|(ions))?|march((er)|(ers))?|vigils?|peaceful)\\b"
regex_violent <- "\\b(angry|violent|riot((er)|(ers)|(s))?|instigat((or)|(ors))?|loot((er)|(ers)|(s))?|extremists?|mob((ster)|(sters)|(s))?|radicals?|far-left|far-right)\\b"

### Perform Two-Sample Proportions test
prop.test(c(str_count(oklahoman_text,regex_violent), str_count(oregonian_text,regex_violent)), 
          c((str_count(oklahoman_text,regex_violent) + str_count(oklahoman_text,regex_nonviolent)), (str_count(oregonian_text,regex_violent) + str_count(oregonian_text,regex_nonviolent))), 
          alternative = "greater")





