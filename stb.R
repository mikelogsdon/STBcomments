library(rvest)
library(dplyr)
library(ggplot2)
# setwd("/storage/homes/michael/Seattle/")

# url <- "http://seattletransitblog.com/2016/04/27/metro-and-sdot-cut-a-deal-on-the-se-seattle-restructure/"
readPage <- function(url) {
  html <- read_html(url)

  html_nodes(html, ".ping-list") %>% html_text()
  
  # Grab comment text
  comments <- html_nodes(html, "#comments .comment-content") %>%
    html_text()
  comments <- gsub("\\t", " ", comments)
  comments <- gsub("\\n", " ", comments)
  comments <- gsub("^( *)", "", comments)
  comments <- gsub("( *)$", "", comments)
  
  # Grab authors
  authors <- html_nodes(html, "#comments .comment-author") %>% 
    html_text()
  authors <- gsub("\\t", "", authors)
  authors <- gsub("\\n", "", authors)
  authors <- gsub("^(.+) says", "\\1", authors)
  authors
  
  # Grab timestamps
  meta <- html_nodes(html, "#comments .comment-meta") %>% 
    html_text()
#   meta <- gsub("am", "AM", meta)
#   meta <- gsub("pm", "PM", meta)
  timestamp <- strptime(gsub("at ", "", meta), "%B %d, %Y %I:%M %p")
  
  # Grab Post Title
  title <- html_nodes(html, ".entry-title") %>% 
    html_text()
  
  # Grab Post Author
  author <- html_nodes(html, ".entry-author-name") %>% 
    html_text()
  
  df <- data.frame("post" = title,
             "author" = author,
             "commenter" = authors,
             "timestamp" = timestamp,
             "text" = comments)

  df
}

# year <- "2016"
# month <- "04"
getUrls <- function(year, month) {
  year <- as.character(year)
  month <- as.character(month)
  if(nchar(month) == 1) {
    month <- paste("0", month, sep = "")
  }
  url <- paste("http://seattletransitblog.com", year, month, sep = "/")
  html <- try(read_html(url))
  if(inherits(html, "try-error")) {
    return(NULL)
  }
#   selector_name <- ".content"
  urls <- html_nodes(html, "a") %>%
    html_attr("href")
  urls <- urls[grep(paste(year, month, sep = "/"), urls)]
  urls <- unique(urls)
  urls
}

years <- 2011:lubridate::year(lubridate::today())
months <- 1:12
allUrls <- unlist(lapply(years, function(y) {
  lapply(months, function(m) {
    print(paste("Now reading", y, m))
    getUrls(y, m)
  })
}))
writeLines(con = "allUrls.txt", allUrls)
allUrls <- readLines("allUrls.txt")

allComments <- do.call('rbind', lapply(allUrls, function(url) {
  Sys.sleep(1)
  print(paste("Now trying to read url", url))
  comments <- try(readPage(url))
  if(inherits(comments, "try-error")) {
    return(NULL)
  } else {
    return(comments)
  }
}))
allComments$text <- as.character(allComments$text)
# Remove those dumb, weird apostrophes
allComments$text2 <- NA
for(i in 1:nrow(allComments)) {
  abc <- unlist(strsplit(allComments$text[i], split = ""))
  utfs <- sapply(abc, utf8ToInt)
  utfs[utfs==8217] <- 39
  chars <- sapply(utfs, intToUtf8)
  allComments$text2[i] <- paste(chars, collapse = "")
}

save(file = "allSTB.rda", allComments)
load(file = "allSTB.rda")
# 
# allwords <- unlist(strsplit(allComments$text, split = " "))
# commenters <- sort(table(allComments$commenter), decreasing = TRUE)
# head(commenters, 30)
# 
# allComments$hour <- lubridate::hour(allComments$timestamp)
# byHour <- aggregate(text ~ hour, data = allComments, FUN = length)
# byHour$text <- byHour$text / sum(byHour$text)
# ggplot(byHour) + theme_bw() + 
#   geom_bar(aes(hour, text), stat = "identity") +
#   ggtitle("Seattle Transit Blog Comments by Hour of Day")
# 
# allComments$date <- as.Date(allComments$timestamp)
# byDate <- aggregate(text ~ date, data = allComments, FUN = length)
# ggplot(byDate) + theme_bw() + 
#   geom_point(aes(date, text), stat = "identity") +
#   ggtitle("Seattle Transit Blog Comments by Date") +
#   scale_y_log10()
# 
# 
# alltext <- paste(allComments$text, collapse = " ")
# alltext <- unlist(strsplit(alltext, split = "[\\.\\!\\?]", fixed = FALSE))
# alltext <- gsub("^( *)", "", alltext)
# # writeLines(con = "~/Documents/Fun/allSTBtext.txt", alltext)
# chatText <- paste(alltext[sample(1:length(alltext), 1 + rpois(1, lambda = 2))], ".", sep = "", collapse = " ")
# chatText
# 
# goldLines <- grep("gold", allComments$text, ignore.case = TRUE)
# goldenGardenLines <- grep("golden gardens", allComments$text, ignore.case = TRUE)
# goldLines <- goldLines[!(goldLines %in% goldenGardenLines)]
# length(goldLines)
# allComments$text[goldLines[10]]
# 
# growsOnTrees <- grep("donald trump", allComments$text, ignore.case = TRUE)
# length(growsOnTrees)
# allComments$text[growsOnTrees]
# sort(table(allComments$commenter[growsOnTrees]))
# 

# 
# urls <- c("http://seattletransitblog.com/2016/04/27/metro-and-sdot-cut-a-deal-on-the-se-seattle-restructure/",
#           "http://seattletransitblog.com/2016/04/26/draft-west-seattle-plan-a-good-start-lets-make-it-great/",
#           "http://seattletransitblog.com/2016/04/26/podcast-15-costs-per-rider/",
#           "http://seattletransitblog.com/2016/04/25/metro-releases-draft-long-range-plan/")
# comments <- unlist(sapply(urls, readPage))
# page <- paste(comments, collapse = " ")
# page <- unlist(strsplit(page, split = "[\\.\\!\\?]", fixed = FALSE))
# page <- gsub("^( *)", "", page)
# chatText <- page[sample(1:length(page), 1)]
# chatText
# 
# 
# 
# year <- "2016"
# month <- "03"
# 


# library(rvest)
# library(dplyr)
# html <- read_html("http://seattletransitblog.com/2016/04/26/podcast-15-costs-per-rider/")
# selector_name <- ".comment-content"
# comments <- html_nodes(html, selector_name) %>%
#   html_text()
# comments <- gsub("\\t", " ", comments)
# comments <- gsub("\\n", " ", comments)
# comments <- gsub("^( *)", "", comments)
# comments <- gsub("( *)$", "", comments)
# comments[2]