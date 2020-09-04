
# Scraping Wikipedia Tables
###########################

library(ggplot2)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(countrycode)

# Downloading website
url <- "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"
xpath <- "/html/body/div[3]/div[3]/div[5]/div[1]/table[2]"
page <- read_html(url)

# Extracting table
alltables <- html_nodes(page, "table")

table <- alltables[[2]] %>% 
    html_table(fill = T)

# Cleaning
table <- table[which(grepl("Totals", table$NOC)!=TRUE),]

table <- table %>% 
    separate(NOC,
             c("country","c_abbrev"),
             sep = -6)

table$c_abbrev <- gsub("[()]|(^\\s+)|(\\s+$)", "",table$c_abbrev)

# Reading links
link <- page %>%
html_nodes(xpath = "//th/a") %>%
html_attr("href")

country <- gsub("/wiki/|_at_the_2016_Summer_Olympics", "", link)
country <- gsub("_", " ", country)
links <- data.frame(country,link)
links <- links[which(grepl("Summer Olympic|Winter Olympic",links$country)!=TRUE),]
links$link <- gsub("/wiki/", "",links$link)

# # Merge
table <- merge(table,links,
                by = "country")

rm(alltables,page, url, xpath,country,links,link) # removing clutter

# Cleaning & translating country names
table <- table[which(table$country!="Independent Olympic Athletes"),]

table$country_de <- countrycode(table$country,'country.name','country.name.de')

# Data for graph functions
table$link <- paste0("https://en.wikipedia.org/wiki/",table$link)

table$onclick <- sprintf("window.open(\"%s%s\")","",table$link)


# Reshape for graph
table <- pivot_longer(table,
                       cols = c("Gold","Silver","Bronze"),
                       names_to = "medal",
                       values_to = "count")

table$medal <- factor(table$medal, labels = c("Gold","Silver","Bronze"))


# Graph
#######

tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"


p <- ggplot(table, aes(x=reorder(c_abbrev,-Total),
                  y=count, fill = medal)) +
    geom_bar_interactive(position="stack", stat="identity",
                         aes(tooltip = paste0("Land: ",country_de,"\n",
                                              "Gesamtzahl Medaillen: ",Total,"\n\n",
                                              "Für weitere Informationen bitte auf den Balken klicken."),
                             onclick = onclick)) +
    xlab("") +
    ylab("") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 125)) +
    theme_bw() +
    scale_fill_brewer(palette = "Reds") +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.x = element_text(size = 6,angle = 25,hjust = 1,vjust = 1))

girafe(ggobj = p,
       fonts=list(sans = "Arial"),
        options = list(
          opts_tooltip(offx = 10, offy = 10,css = tooltip_css),
          opts_toolbar(saveaspng = FALSE)))

