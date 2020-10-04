
# Scraping Wikipedia Tables
###########################

library(ggplot2)
library(rvest)
library(dplyr)
library(tidyverse)
library(ggiraph)
library(countrycode)
library(BayesianFirstAid)

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
    geom_bar_interactive(position="stack", stat="identity",color = "gray", size=.1,
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
    scale_fill_manual(values = c("#fee8c8","#fdbb84","#e34a33"),
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


p <- table %>% arrange(-no) %>% 
    slice_head(n=3*50) %>% 
    ggplot(aes(x=reorder(c_abbrev,no),y=no)) +
    geom_bar_interactive(stat = "identity", fill = "#e34a33",
                         aes(tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Anzahl Athleten: ",no,"\n\n",
                                              "Für weitere Informationen bitte auf den Balken klicken."),
                             onclick = onclick_de)) +
    ylab("Athleten") +
    xlab("") +
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

girafe(ggobj = p,
       fonts=list(sans = "Arial"),
        options = list(
          opts_tooltip(offx = 10, offy = 10,css = tooltip_css,use_cursor_pos = TRUE),
          opts_toolbar(saveaspng = FALSE)))


p <- table %>% 
    select(country_de,no,Total,onclick_de,c_abbrev,Bronze,Silber,Gold) %>% 
    filter(!is.na(Total)) %>% 
    unique() %>% 
    ggplot(aes(x=no,y=Silber)) +
    stat_smooth(color = "gray",alpha = .2,linetype = "dashed",size = .5) +
    geom_point_interactive(color = "#e34a33", alpha = .6,size = 3,
                           aes(data_id = country_de,
                               onclick = onclick_de,
                               tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Anzahl Athleten: ",no,"\n",
                                              names(table)[names(table) == "Silber"],": ",Silber,"\n\n",
                                              "Für weitere Informationen bitte hier klicken."))) +
    ylab("Gesamtzahl Medaillen") +
    xlab("Anzahl Athleten") +
    labs(caption = "Regression via LOESS smoother") +
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
          opts_toolbar(saveaspng = FALSE),
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "fill:red;")))



# Aufg 3 Bayesian simulation
############################

set.seed(42)

no_pos <- c(90,2)
no_eval <- c(100,2)

bayes.prop.test(no_pos,no_eval)

fit <- bayes.prop.test(no_pos,no_eval)

summary(fit)

s <- as.data.frame(fit)
s$diff <- s$theta1-s$theta2

  hdi_diff <- bayestestR::ci(s$diff, method="HDI",ci=0.95)
  
  hdi_anbA <- bayestestR::ci(s$theta1, method="HDI",ci=0.95)
  
  hdi_anbB <- bayestestR::ci(s$theta2, method="HDI",ci=0.95)

  mean(s$diff>0)
  mean(s$diff<0)  
  
  
  # ROPE
  mean(abs((s$theta1 - s$theta2)) < 0.025)

s %>% 
  select(theta1, theta2) %>% 
  pivot_longer(cols = everything(),
               values_to="theta",
               names_to = "type") %>% 
  ggplot(aes(x=theta, fill = type)) +
    geom_density(alpha = 0.25) +
    theme_bw() +
    theme(panel.grid = element_blank(),
      legend.position = "bottom")
    

ggplot(s,aes(x=theta1)) +
  geom_density(color="#585858", fill="#585858", alpha=.5) +
  geom_vline(xintercept = median(s$theta1), linetype="dashed", size=0.25) +
  geom_segment(aes(x=hdi_anbA$CI_low,xend=hdi_anbA$CI_high,y=0.075,yend=0.075), size = 0.25) +
  annotate(geom="text",x=hdi_anbA$CI_low,y=0.5,label=as.character(round(hdi_anbA$CI_low,2))) +
  annotate(geom="text",x=hdi_anbA$CI_high,y=0.5,label=as.character(round(hdi_anbA$CI_high,2))) +
  scale_y_continuous(expand = c(0,0),limits = c(0,16), breaks = seq(0,15,5)) + 
  xlab("Anteil pos. Bewertungen für Anbieter A") +
  ylab("Dichte") +
  theme_bw() +
    theme(panel.grid = element_blank()) 

ggplot(s,aes(x=theta2)) +
  geom_density(color="#585858", fill="#585858", alpha=.5) +
  geom_vline(xintercept = median(s$theta2), linetype="dashed", size=0.25) +
  geom_segment(aes(x=hdi_anbB$CI_low,xend=hdi_anbB$CI_high,y=0.075,yend=0.075), size = 0.25) +
  annotate(geom="text",x=hdi_anbB$CI_low,y=0.135,label=as.character(round(hdi_anbB$CI_low,2))) +
  annotate(geom="text",x=hdi_anbB$CI_high,y=0.135,label=as.character(round(hdi_anbB$CI_high,2))) +
  scale_y_continuous(expand = c(0,0),limits = c(0,2.95), breaks = seq(0,2.75,0.5)) + 
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  xlab("Anteil pos. Bewertungen für Anbieter B") +
  ylab("Dichte") +
  theme_bw() +
    theme(panel.grid = element_blank())

ggplot(s,aes(x=diff)) +
  geom_density(color="#c2224a", fill="#c2224a", alpha=.5) +
  geom_vline(xintercept = median(s$diff), linetype="dashed", size=0.25) +
  geom_segment(aes(x=hdi_diff$CI_low,xend=hdi_diff$CI_high,y=0.02,yend=0.02), size = 0.25) +
  annotate(geom="text",x=hdi_diff$CI_low,y=0.1,label=as.character(round(hdi_diff$CI_low,2))) +
  annotate(geom="text",x=hdi_diff$CI_high,y=0.1,label=as.character(round(hdi_diff$CI_high,2))) +
  scale_x_continuous(breaks = seq(-0.2,0.9,0.1), limits = c(-0.25,0.95)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,2.75), breaks = seq(0,2.5,0.5)) +  
  xlab("Differenz im Anteil pos. Bewertungen (Anb.A - Anb. B)") +
  ylab("Dichte") +
  theme_bw() +
    theme(panel.grid = element_blank())
