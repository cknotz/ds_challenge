
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



# Aufg 3 (Randomization test/Fisher exact test)
###############################################

(90/100)-(2/2)

(89/100)-(1/2)

(88/100)-(0/2)

fisher.test(rbind(c(90,2),c(10,0)), alternative="less")

# by hand:
one <- (factorial(90+2)*factorial(10+0)*factorial(90+10)*factorial(2+0)) /
    (factorial(102)*factorial(90)*factorial(2)*factorial(10)*factorial(0)) # observed

two <- (factorial(89+1)*factorial(11+1)*factorial(89+11)*factorial(1+1)) /
    (factorial(102)*factorial(89)*factorial(1)*factorial(11)*factorial(1)) # one more extreme

three <- (factorial(88+0)*factorial(12+2)*factorial(88+12)*factorial(0+2)) /
    (factorial(102)*factorial(88)*factorial(0)*factorial(12)*factorial(2)) # one more extreme

one+two+three # one sided, also two-sided (R sums probs of all tables with probs <= to observed; here: all!)

one # one-sided, other direction?

two+three

# Simulation I 
##############

# Generate data.frame
df <- data.frame(anb = c(replicate(100, "Anbieter A"),replicate(2, "Anbieter B")),
                 eval = c(c(replicate(90,1),replicate(10,0)),c(1,1)))

table(df$anb,df$eval)

# Storing proportions
sum <- df %>% 
  group_by(anb) %>% 
  summarize(positive = mean(eval == 1),
    sample_size = n())

phat_A <- sum$positive[1]
phat_B <- sum$positive[2]
n_A <- sum$sample_size[1]
n_B <- sum$sample_size[2]
obs_diff <- phat_A - phat_B

# Shuffling
set.seed(17)

shuffles <- mosaic::do(1000) *
    (df %>% 
         mutate(anb = mosaic::shuffle(anb)) %>% 
         group_by(anb) %>% 
         summarize(positive = mean(eval == 1)))

ggplot(shuffles, aes(x=anb,y=positive)) +
    geom_jitter(alpha=.2, width = 0.25, size = 2,color = "#c2224a") +
    scale_y_continuous(breaks = seq(0,1,.1)) +
    ylab("Anteil positive Bewertungen") +
    xlab("") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())


null_dist <- shuffles %>% 
    group_by(.index) %>% 
    summarize(diff = -diff(positive)) # negative of difference to obtain same sign as diff A-B

ggplot(null_dist, aes(x = diff)) +
  geom_histogram(color = "white", fill = "#c2224a") +
    scale_y_continuous(labels = function(x){x/1000}) +
    scale_x_continuous(breaks = seq(-0.2,1,.1)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())

sum(round(null_dist$diff,2)>0)/1000

# Compute p-value
pvalue <- null_dist %>%
  filter( (round(diff,2) > obs_diff) ) %>%
  nrow() / nrow(null_dist)
pvalue


# Simulation II
###############
set.seed(42)
anbA <- rbinom(n=10000,size=100,prob = 90/100)
anbB <- rbinom(n=10000,size=2,prob = 2/2)

sims <- as.data.frame(cbind(anbA/100,anbB/2)) %>% 
    pivot_longer(names_to = "anb",
                 values_to = "scores",
                 cols = everything())

ggplot(sims, aes(x=scores,fill = anb)) +
    geom_histogram(binwidth =.01,alpha=.8, aes(y=..density..), position = "identity") +
    scale_fill_manual(values = c("#222d33","#c2224a"),
                      labels = c("Anbieter A","Anbieter B")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    xlab("Anteil positive Bewertungen") +
    ylab("Dichte (%)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())

diffs <- anbA/100 - anbB/2

ggplot(as.data.frame(diffs), aes(x=diffs)) +
    stat_density(alpha = .3,bw=.01, fill = "#c2224a") +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Differenz in den Anteilen pos. Bewertungen") +
    ylab("Dichte (%)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())

sum(diffs>=0)/10000

