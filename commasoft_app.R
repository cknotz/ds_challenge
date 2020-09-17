

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
  dashboardHeader(title = "Data Science Challenge", titleWidth = 300),
  dashboardSidebar(
      sidebarMenu(
          menuItem("Start", tabName = "start"),
          menuItem("Meine Lösung", tabName = "solu",
          menuSubItem("Aufgabe 1", tabName = "aufg1"), #, icon = icon("chart-bar", lib = "font-awesome")
          menuSubItem("Aufgabe 2", tabName = "aufg2", selected = T), #, icon = icon("amazon", lib = "font-awesome")
          menuSubItem("Aufgabe 3", tabName = "aufg3")) #, icon = icon("cogs", lib = "font-awesome")
  )),
  dashboardBody(
      shinyDashboardThemes(theme = "flat_red"),
      tags$style(type="text/css", "text {font-family: sans-serif}"),
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
      tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar {background: #c2224a;border-color: #c2224a;}")),
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
                                              min = 10,
                                              max = 207,
                                              value = 10,
                                              step = 1,
                                              ticks = F)),
                                  girafeOutput("athletes")),
                         tabPanel("Medaillen pro Land",
                                  fluidRow(width=NULL,align="center",
                                  sliderInput("n_medals",
                                              label = "Anzahl Länder",
                                              min = 10,
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
                      title = "Amazon-Kundenbewertungen",
                      column(width = 6,
                             plotOutput("sim")
                             ),
                      column(width = 6,
                             plotOutput("diff")),
                      column(width = 6,
                             h5("Anbieter A"),
                             sliderInput(inputId = "count_a",
                                         min = 2,
                                         max = 500,
                                         value = 100,
                                         step = 1,
                                         ticks = F,
                                         label = "Anzahl Bewertungen"),
                             sliderInput(inputId = "pos_a",
                                         min = 0,
                                         max = 100,
                                         step = 1,
                                         value = 90,
                                         ticks = F,
                                         label = "Prozent positiv")
                             ),
                      column(width = 6,
                             h5("Anbieter B"),
                             sliderInput(inputId = "count_b",
                                         min = 2,
                                         max = 500,
                                         value = 2,
                                         step = 1,
                                         ticks = F,
                                         label = "Anzahl Bewertungen"),
                             sliderInput(inputId = "pos_b",
                                         min = 2,
                                         max = 100,
                                         step = 5,
                                         value = 100,
                                         ticks = F,
                                         label = "Prozent positiv")
                             ),
                      HTML("<p>Die Aufgabe lässt sich auf zwei Weisen lösen. Eine Möglichkeit ist ein parametrischer Test,
                       (d.h. ein Test, der auf Annahmen über die Verteilung der Daten in der Grundgesamtheit beruht), die
                           zweite Möglichkeit ist ein Test mittels einer Simulation.</p>
                           <p>In ersterem Fall verwendet man den Exakten Test nach Fisher, mit welchen
                           Differenzen zwischen Stichprobenanteilen auch bei kleinen Stichprobengrößen (wie hier der Fall)
                           auf ihre Signifikanz hin getestet werden können (die Alternative bei größeren
                           Stichproben ist der klassische Signifikanztest für Stichprobenanteile basierend auf
                           der Standardnormalverteilung).</p>
                           <p>Der Exakte Test nach Fisher wird berechnet als die Wahrscheinlichkeit, die beobachtete oder
                           eine noch extreme Verteilung zu erhalten unter der Annahme, dass es zwischen der Wahl des Anbieters
                           und der Kundenzufriedenheit keinen Zusammenhang gibt. Formal wird dies berechnet als:</p>"),
                      withMathJax("$$p = \\frac{(a+b)!(c+d)!(a+c)!(b+d)!}{(a+b+c+d)!a!b!c!d!}$$")
                      )
              )),
          tabItem(tabName = "aufg3",
              fluidRow(
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 1: Was ist das primäre Ziel für den Einsatz von Predictive Maintenance?",
                      HTML("<p>Aller Wahrscheinlichkeit nach ist das Ziel, Wartungszeiten zu verkürzen 
                      (bzw. zu vermeiden, wo diese nicht nötig sind) und daher vorhandenes Equipment optimaler zu nutzen. Andere
                           Motive sind allerdings auch denkbar (bspw. Unfallschutz oder Qualitätssicherung). Je nach Ziel ändert sich ggf.
                           auch, welche Gütemaße bei der Bewertungen von Algorithmen angewandt werden müssen. Sollen Wartungen optimiert werden,
                           spielt die Präzision (die Vermeidung falscher positiver Diagnosen) eine zentrale Rolle; bei der Qualitätssicherung
                           ist die Vermeidung von falscher negativer Diagnosen (die Sensitivität) wichtiger.</p>")),
                  box(width = 12,collapsible = F, solidHeader = T,
                      title = "Frage 2: Wie werden die Maschinen momentan inspiziert?",
                      HTML("<p>Laut einer Umfrage von PWC verlassen sich viele Betrieben auf visuelle Inspektionen ihrer
                           Maschinen; relativ wendige wenden schon Instrumentendaten, ggf. auch in real-time, an. In ersterem
                           Fall müssten zunächst Messinstrumente installiert und systematisch Daten gesammelt werden.</p>")),
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 3: ")
              ))
  ))
)

server <- function(input, output, session) {
tooltip_css <- "background-color:gray;color:white;padding:10px;border-radius:5px;font-family: Lora, sans-serif;font-weight:lighter;font-size:12px;"

##### Graph 1.1
###############
output$athletes <- renderGirafe({

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
          opts_toolbar(saveaspng = FALSE,position = "topleft"),
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
          opts_toolbar(saveaspng = FALSE,position = "topleft"),
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
    #labs(caption = "Regression via LOESS smoother") +
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
          opts_toolbar(saveaspng = FALSE,position = "topleft"),
          opts_hover_inv(css = "opacity:0.1;"),
          opts_hover(css = "fill:red;"),
          opts_selection(type = "none")))
})

##### Graph 2.1
###############

output$sim <- renderPlot({
  set.seed(42)
  
  print((input$pos_a/100)*input$count_a)
  
  anbA <- rbinom(n=10000,size=input$count_a,prob = ((input$pos_a/100)*input$count_a)/input$count_a)
  anbB <- rbinom(n=10000,size =input$count_b,prob = ((input$pos_b/100)*input$count_b)/input$count_b)
  diffs <- anbA/input$count_a - anbB/input$count_b

  sims <- as.data.frame(cbind(anbA/input$count_a,anbB/input$count_b)) %>% 
    pivot_longer(names_to = "anb",
                 values_to = "scores",
                 cols = everything())
  
  
  ggplot(sims, aes(x=scores,fill = anb)) +
    geom_histogram(binwidth=.01, alpha=.8, aes(y=..density..)) +
    scale_fill_manual(values = c("#222d33","#c2224a"),
                      labels = c("Anbieter A","Anbieter B")) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, 101)) +
    xlab("Anteil positive Bewertungen") +
    ylab("Dichte (%)") +
    labs(caption = paste0("Anteil der sim. Fälle, in denen Anbieter A besser bewertet wird: ",sum(diffs>0)/10000)) +
    theme_bw() +
    theme(legend.position = "top",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          plot.caption = element_text(size = 8))
})

output$diff <- renderPlot({
  
  set.seed(42)
  anbA <- rbinom(n=10000,size=input$count_a,prob = ((input$pos_a/100)*input$count_a)/input$count_a)
  anbB <- rbinom(n=10000,size =input$count_b,prob = ((input$pos_b/100)*input$count_b)/input$count_b)
  diffs <- anbA/input$count_a - anbB/input$count_b
  sum(diffs>0)/10000
  print(sum(diffs>0)/10000)
  
  ggplot(as.data.frame(diffs), aes(x=diffs)) +
    stat_density(alpha = .3, fill = "#c2224a") +
    scale_y_continuous(expand = c(0, 0)) +
    xlab("Verteilung der Differenzen in den pos. Bewertungen") +
    ylab("Dichte (%)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())
})

}

# Run the application 
shinyApp(ui, server)
