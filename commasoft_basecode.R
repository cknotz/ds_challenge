
# Scraping Wikipedia Tables
###########################

library(ggplot2)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(countrycode)


# Number of medals
##################

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
links <- links[which(grepl("Summer Olympic|Winter Olympic|Independent Olympic Athletes",links$country)!=TRUE),]
links$link <- gsub("/wiki/", "",links$link)

# # Merge
table <- merge(table,links,
                by = "country")

rm(alltables,page, url, xpath,country,links,link) # removing clutter

# adjust medal names
names(table)[names(table)=="Silver"] <- "Silber"


# Number of athletes
####################
url <- "https://en.wikipedia.org/wiki/2016_Summer_Olympics#Number_of_athletes_by_National_Olympic_Committee"

# Fetching page
athletes <- url %>% 
    read_html() %>% 
    html_nodes(xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[5]", 
               #css = "table.wikitable:nth-child(101)"
               ) %>% 
    #html_table(fill=T)
    html_text() 

athletes <- strsplit(athletes, '\n')[[1]] %>% 
    as.data.frame() %>% 
    filter(.!="Participating National Olympic Committees" & .!= "") %>% 
    rename("entry"= ".")

# Cleaning
athletes$no <- as.numeric(str_extract(athletes$entry,
              "(\\d+)"))
athletes$country <- gsub("\\d|[[:punct:]]",
                         "",
                         athletes$entry) %>% 
    trimws(whitespace = "[\\h\\v]")
athletes$entry <- NULL

rm(url) #remove clutter 

# Merge & reshape
#################
table <- merge(athletes,table,
               by="country",
               all.x = T)
rm(athletes)


# Cleaning & translating country names
table <- filter(table,
                country!="Refugee Olympic Team" & country!="Independent Olympic Athletes")

table$country_de <- countrycode(table$country,'country.name','country.name.de')
    # custom name changes
    table$country_de[which(table$country=="Virgin Islands")] <- "Virgin Islands"
    table$country_de[which(table$country_de=="Korea, Demokratische Volksrepublik")] <- "Nordkorea"
    table$country_de[which(table$country_de=="Korea, Republik von")] <- "Südkorea"
    table$country_de[which(table$country_de=="Russische Föderation")] <- "Russland"
    
table$c_abbrev <- countrycode(table$country,'country.name','iso3c')
    # custom names
    table$c_abbrev[which(table$country=="Kosovo")] <- "XK"
    table$c_abbrev[which(table$country=="Virgin Islands")] <- "VIR"

# Data for graph functions
table$link <- paste0("https://en.wikipedia.org/wiki/",table$link)

table$onclick <- sprintf("window.open(\"%s%s\")","",table$link)

# German links
table$link_de <- paste0("https://de.wikipedia.org/wiki/Olympische_Sommerspiele_2016/Teilnehmer_(",
                        table$country_de,
                        ")") %>%
    gsub(" ","_",.)

table$onclick_de <- sprintf("window.open(\"%s%s\")","",table$link_de)


# Reshape for graph
table <- pivot_longer(table,
                       cols = c("Gold","Silber","Bronze"),
                       names_to = "medal",
                       values_to = "count")

# Values for tooltip
tiptab <- pivot_wider(table[,c("country","medal","count")],
                      id_cols = c("country","medal"),
                      names_from = "medal",
                      values_from = "count")

# Merging
table <- merge(table,tiptab,by = "country")
    rm(tiptab)
    
# Factor for graph
table$medfac <- NA
table$medfac[table$medal=="Bronze"] <- 1
table$medfac[table$medal=="Silber"] <- 2
table$medfac[table$medal=="Gold"] <- 3
table$medfac <- factor(table$medfac,
                       levels = c(1,2,3),
                       labels = c("Bronze","Silber","Gold"))

# Save as backup
saveRDS(table,file = "www/backup.rds")

# Graph
#######
tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"


# Plot
p <- table %>% arrange(-table$Total,table$c_abbrev) %>% 
    slice_head(n=3*20) %>% 
    ggplot(aes(x=reorder(c_abbrev,Total),
                  y=count, fill = medfac)) +
    geom_bar_interactive(position="stack", stat="identity",color = "gray", size=.05,
                         aes(tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Gesamtzahl Medaillen: ",Total,"\n",
                                              "Gold: ",Gold,"\n",
                                              "Silber: ",Silber,"\n",
                                              "Bronze: ",Bronze,"\n\n",
                                              "Für weitere Informationen bitte auf den Balken klicken."),
                             onclick = onclick_de)) +
    coord_flip() +
    xlab("") +
    ylab("") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 125)) +
    scale_fill_manual(values = c("#d95f0e","#a6cee3","#fff7bc"),
                      guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 4),
          axis.text.x = element_text(size = 6),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          legend.key.size = unit(.75,"line"),
          legend.text = element_text(size = 6))

girafe(ggobj = p,
       fonts=list(sans = "Arial"),
        options = list(
          opts_tooltip(offx = 10, offy = 10,css = tooltip_css,use_cursor_pos = TRUE),
          opts_toolbar(saveaspng = FALSE)))


table %>% arrange(-no) %>% 
    slice_head(n=3*50) %>% 
    ggplot(aes(x=reorder(c_abbrev,no),y=no)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 4),
          axis.text.x = element_text(size = 6),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          legend.key.size = unit(.75,"line"),
          legend.text = element_text(size = 6))
