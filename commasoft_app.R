

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggiraph)

table <- readRDS("www/backup.rds")
names(table)[names(table)=="Total"] <- "Gesamt"

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Herausforderung", titleWidth = 300),
  dashboardSidebar(
      sidebarMenu(
          menuItem("Start", tabName = "start"),
          menuItem("Meine Lösung", tabName = "solu",
          menuSubItem("Aufgabe 1", tabName = "aufg1", selected = T), #, icon = icon("chart-bar", lib = "font-awesome")
          menuSubItem("Aufgabe 2", tabName = "aufg2"), #, icon = icon("amazon", lib = "font-awesome")
          menuSubItem("Aufgabe 3", tabName = "aufg3")) #, icon = icon("cogs", lib = "font-awesome")
  )),
  dashboardBody(
      shinyDashboardThemes(theme = "flat_red"),
      tags$style(type="text/css", "text {font-family: sans-serif}"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tabItems(
          tabItem(tabName = "start",
                  fluidRow(
                      box(width = 12, collapsible = F,solidHeader = T,
                          HTML("<p><strong>Herausforderungen stelle ich mich immer gerne.</strong></p>
                               <p>Zu meiner Lösung:
                               <ul>
                               <li>Die Daten zur Zahl der Athleten und der gewonnenen Medaillen bei den Olympischen 
                               Sommerspielen 2016 werden durch das Dashboard selbst von Wikipedia 'gescraped' 
                               (<a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics#Number_of_athletes_by_National_Olympic_Committee'>
                               Quelle für die Zahl der Athleten</a>; 
                               <a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table#Medal_table'>
                               Quelle für die Zahl der Medaillen</a>)</li>
                               <li>Bei den Antworten auf Frage 3 beziehe ich mich u.a. auf den Artikel zur Anwendung von Machine-Learning Algorithmen
                               im Bereich Predictive Maintenance von Carvalho et al. (<a target='_blank'
                               href='https://doi.org/10.1016/j.cie.2019.106024'>2019</a>).
                               </li>
                               </ul></p>")
                      )
                  )),
          tabItem(tabName = "aufg1",
              fluidRow(
                  tabBox(width = 12, id = "tab1", # title = "Athleten & Medaillen bei den Olympischen Sommerspielen 2016"
                         tabPanel("Athleten pro Land",
                                  fluidRow(width=NULL,align="center",
                                  sliderInput("n_athletes",
                                              label = "Anzahl Länder",
                                              min = 2,
                                              max = 207,
                                              value = 10,
                                              step = 1,
                                              ticks = F)),
                                  girafeOutput("athletes")),
                         tabPanel("Medaillen pro Land",
                                  fluidRow(width=NULL,align="center",
                                  sliderInput("n_medals",
                                              label = "Anzahl Länder",
                                              min = 2,
                                              max = 84,
                                              value = 10,
                                              step = 1,
                                              ticks = F)),
                                  girafeOutput("medals")),
                         tabPanel("Bringen mehr Athleten mehr Medaillen?",
                                  fluidRow(width=NULL,align="center",
                                  pickerInput("medal_select",
                                              label = "Resultat",
                                              choices = c("Gesamtzahl Medaillen" = "Gesamt",
                                                          "Goldmedaillen" = "Gold",
                                                          "Silbermedaillen" = "Silber",
                                                          "Bronzemedaillen" = "Bronze"))),
                                  girafeOutput("scatter"))
                     )
              )),
          tabItem(tabName = "aufg2",
              fluidRow(
                  box(width = 12, collapsible = F, solidHeader = T,
                      title = "Amazon-Kundenbewertungen")
              )),
          tabItem(tabName = "aufg3",
              fluidRow(
                  box(width = 12,collapsible = T, solidHeader = T,collapsed = T,
                      title = "Frage 1: "),
                  box(width = 12,collapsible = T,solidHeader = T,collapsed = T,
                      title = "Frage 2: "),
                  box(width = 12,collapsible = T,solidHeader = T,collapsed = T,
                      title = "Frage 3: ")
              ))
  ))
)

server <- function(input, output, session) {
tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"

##### Graph 1.1
###############
output$athletes <- renderGirafe({
  print(6-0.020*input$n_athletes)
  
p <- table %>% 
    group_by(country) %>% 
    slice(1) %>% 
    ungroup %>% 
    arrange(-no) %>%
    slice_head(n=input$n_athletes) %>%
    ggplot(aes(x=reorder(c_abbrev,no),y=no)) +
    geom_bar_interactive(stat = "identity", fill = "#c2224a",
                         aes(tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Anzahl Athleten: ",no,"\n\n",
                                              "Für weitere Informationen bitte auf den Balken klicken."),
                             onclick = onclick_de)) +
    ylab("Athleten") +
    xlab("") +
    coord_flip() +
    scale_y_continuous(expand = c(0, 0), limits = c(0,610)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 6-0.0225*input$n_athletes ), # adapt this
          axis.text.x = element_text(size = 8),
          axis.ticks.y = element_line(size = 0.1),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          legend.key.size = unit(.75,"line"),
          legend.text = element_text(size = 6))

girafe(ggobj = p,
       fonts=list(sans = "Arial"),
        options = list(
          opts_tooltip(offx = 10, offy = 10,css = tooltip_css,use_cursor_pos = TRUE),
          opts_toolbar(saveaspng = FALSE,position = "bottomright"),
          opts_zoom(max = 5)))
})

##### Graph 1.2
###############
output$medals <- renderGirafe({

p <- table %>% arrange(-table$Gesamt,table$c_abbrev) %>%
    slice_head(n=3*input$n_medals) %>%
    ggplot(aes(x=reorder(c_abbrev,Gesamt),
                  y=count, fill = medfac)) +
    geom_bar_interactive(position="stack", stat="identity",color = "gray", size=.1,
                         aes(tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Gesamtzahl Medaillen: ",Gesamt,"\n",
                                              "Gold: ",Gold,"\n",
                                              "Silber: ",Silber,"\n",
                                              "Bronze: ",Bronze,"\n\n",
                                              "Für weitere Informationen bitte auf den Balken klicken."),
                             onclick = onclick_de)) +
    coord_flip() +
    xlab("") +
    ylab("") +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 130)) +
    scale_fill_manual(values = c("#c2224a50","#c2224a95","#c2224a"),
                      guide = guide_legend(reverse = TRUE)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 6-0.0225*input$n_medals ),
          axis.text.x = element_text(size = 8),
          axis.ticks.y = element_line(size = 0.1),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          legend.key.size = unit(.75,"line"),
          legend.text = element_text(size = 6))

girafe(ggobj = p,
       fonts=list(sans = "Arial"),
        options = list(
          opts_tooltip(offx = 10, offy = 10,css = tooltip_css,use_cursor_pos = TRUE),
          opts_toolbar(saveaspng = FALSE),
          opts_zoom(max = 5)))
})

##### Graph 1.3
###############
output$scatter <- renderGirafe({

p <- table %>% 
    select(country_de,no,Gesamt,onclick_de,c_abbrev,Bronze,Silber,Gold) %>% 
    filter(!is.na(Gesamt)) %>% 
    unique() %>% 
    ggplot(aes(x=no,y=!!sym(input$medal_select))) +
    stat_smooth(color = "gray",alpha = .2,linetype = "dashed",size = .5) +
    geom_point_interactive(color = "#c2224a", alpha = .6,size = 3,
                           aes(data_id = country_de,
                               onclick = onclick_de,
                               tooltip = paste0("<strong>",country_de,"</strong>\n\n",
                                              "Anzahl Athleten: ",no,"\n",
                                              names(table)[names(table) == input$medal_select],": ",!!sym(input$medal_select),"\n\n",
                                              "Für weitere Informationen bitte auf den Punkt klicken."))) +
    ylab(names(table)[names(table) == input$medal_select]) +
    xlab("Anzahl Athleten") +
    labs(caption = "Regression via LOESS smoother") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.text.y = element_text(size = 8),
          axis.text.x = element_text(size = 8),
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
          opts_hover(css = "fill:red;"),
          opts_selection(type = "none")))
})

}

# Run the application 
shinyApp(ui = ui, server = server)
