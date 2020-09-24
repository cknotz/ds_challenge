

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggiraph)

table <- readRDS("www/backup.rds")
names(table)[names(table)=="Total"] <- "Gesamt"

# paste(readLines("www/tab1.html"), collapse="\n")
# paste(readLines("www/tab2.html"), collapse="\n")
# paste(readLines("www/tab3.html"), collapse="\n")
# paste(readLines("www/tab4.html"), collapse="\n")

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Challenge", titleWidth = 300),
  dashboardSidebar(
      sidebarMenu(
          menuItem("Start", tabName = "start", selected = T),
          menuItem("Meine Lösung", tabName = "solu",
          menuSubItem("Aufgabe 1", tabName = "aufg1"), #, icon = icon("chart-bar", lib = "font-awesome")
          menuSubItem("Aufgabe 2", tabName = "aufg2"), #, icon = icon("amazon", lib = "font-awesome")
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
                               <p>Meine Lösungen für die drei Aufgaben stelle ich Ihnen über dieses interaktive Dashboard vor.</p>
                               <p>Zu meinen Lösungen:
                               <ul>
                               <li>Die Daten zur Zahl der Athleten und der gewonnenen Medaillen bei den Olympischen 
                               Sommerspielen 2016 habe ich direkt von Wikipedia 'gescraped'
                               (<a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics#Number_of_athletes_by_National_Olympic_Committee'>Quelle für die Zahl der Athleten</a>; 
                               <a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table#Medal_table'>
                               Quelle für die Zahl der Medaillen</a>). Bei der Zahl der Athleten beschränke ich mich auf
                               die ersten 75 Länder aus 207, um das Schaubild lesbar zu halten.</li>
                               <li>Der Code, um die Daten von Wikipedia zu 'scrapen' sowie der Code für dieses Dashbord (inkl. der Simulation) sind auf
                               meinem <a href='' target='_blank'>Github Profil</a> hinterlegt.</li>
                               <li>Bei den Antworten auf Frage 3 beziehe ich mich u.a. auf zwei Studien zu Predictive Maintenance
                               Praktiken von PricewaterhouseCoopers (<a target='_blank href='https://www.pwc.nl/nl/assets/documents/pwc-predictive-maintenance-4-0.pdf'>2017</a>; <a target='_blank'
                               href='https://www.mainnovation.com/wp-content/uploads/tmp/6397245268d8d3711c88cda0b4585ab02e612f2e.pdf'>2018</a>).
                               </li>
                               </ul>
                               Mir hat die Challenge viel Spaß gemacht, und ich hoffe natürlich, dass Sie meine Antworten überzeugen.
                               Mein genaues Vorgehen und die Resultate bespreche ich natürlich sehr gerne näher mit Ihnen in einem persönlichen Gespräch.
                               </p>
                               <br>
                               <p>Freundliche Grüße</p>
                               <p>Carlo Knotz</p>
                               ")
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
                                              max = 75,
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
                  box(width = 12, collapsible = F, solidHeader = F,
                      title = "Amazon-Kundenbewertungen",
                      HTML(paste(readLines("www/aufg2_simtext1.html"), collapse="\n"))),
                      box(width=12, title = "Die Berechnung im Detail", collapsible = T, collapsed = T, solidHeader = F,
                      column(width = 12,
                             HTML("<p>Beim exakten Test nach Fisher wird die Wahrscheinlichkeit
                             berechnet, dass eine bestimmte Differenz in einem binären Merkmal (z.B. positiv/negativ) zwischen 
                             zwei Gruppen beobachtet wird, <i>falls es in Wahrheit keinen wirklichen Unterschied zwischen 
                             den zwei Gruppen gibt</i>. Anders ausgedrückt berechnet man die Wahrscheinlichkeit, dass die 
                             beobachteten Werte rein durch Zufall enstanden sein könnten und die Daten keinen tatsächlichen Unterschied 
                             widerspiegeln.</p>
                             
                             <p>Dazu überträgt man zunächst die beobachteten Werte in eine 2x2 Kreuztabelle, wie in 
                             Tabelle (1) dargestellt.</p>")
                             ),
                      column(width = 6,
                             HTML(readLines("www/tab2.html"))
                             ),
                      column(width = 6,
                             HTML(readLines("www/tab1.html")),
                             br(),
                      br()),
                      column(width = 12,
                             HTML("<p>Die Wahrscheinlichkeit, die in Tabelle (1) dargestellten Werte zu beobachten, lässt
                                  sich dann mit Hilfe der folgenden Formel direkt berechnen. Die Beobachtungen gehen dabei
                                  wie in Tabelle (2) gezeigt in die Berechnung ein:</p>"),
                             withMathJax("$$p = \\frac{(A+B)!(C+D)!(A+C)!(B+D)!}{N!A!B!C!D!}$$"),
                             HTML("<p>Für die beobachteten Werte ergibt die Berechnung:</p>"),
                             verbatimTextOutput("fish_1"),
                             HTML("<p><strong>Die Berechnung ergibt eine geschätzte Wahrscheinlichkeit von rund 80%, dass die beobachtenen
                                  Werte rein durch Zufall entstanden sind und eigentlich kein Unterschied in den Bewertungen
                                  von Anbieter A und B besteht.</strong></p>
                                  <p>In einem zweiten Schritt kann dann noch berechnet werden, mit welcher Wahrscheinlichkeit 
                                  man in den Daten theoretisch eine im Schnitt bessere Bewertung von Anbieter A beobachtet werden könnte.
                                  Bei ingesamt 102 Bewertungen, von sich denen 100 auf Anbieter A und 2 auf Anbieter B verteilen,
                                  ergeben sich zwei Szenarien. Diese sind in Tabellen (3) und (4) dargestellt.</p>"),
                      column(width = 6,
                             HTML(readLines("www/tab3.html"))
                             ),
                      column(width = 6,
                             HTML(readLines("www/tab4.html")),
                             br(),
                            br()),
                      column(width = 12,
                             HTML("<p>In Tabelle (3) wurden beiden Anbietern je eine positive Bewertung abgezogen und als
                                  negative hinzugerechnet; in Tabelle (4) wurde dies wiederholt. Da Anbieter B über nur zwei 
                                  Bewertungen verfügt, sind nur diese zwei Alternativen möglich ohne die Gesamtzahl der 
                                  Bewertungen zu verändern.</p>
                                  <p>Wendet man die obige Formel hier nochmals an ergeben sich folgende Wahrscheinlichkeiten:</p>")),
                      column(width=6,
                             HTML("<strong>Tabelle (3)</strong>"),
                             verbatimTextOutput("fish_2")),
                      column(width = 6,
                             HTML("<strong>Tabelle (4)</strong>"),
                             verbatimTextOutput("fish_3")),
                      column(width = 12,
                             HTML("<p>Die Berechnungen ergeben eine geschätzte Wahrscheinlichkeit von insgesamt rund 18%, bei
                                  100 Bewertungen für Anbieter A und 2 für Anbieter B die in Tabelle (3) gezeigte Verteilung
                                  zu beobachten. Die in Tabelle (4) gezeigte Verteilung, in der Anbieter B nur negativ 
                                  beurteilt wird, ist mit geschätzten 0% extrem unwahrscheinlich.</p>
                                  
                                  <p><strong>Die geschätzte Wahrscheinlichkeit, dass Anbieter A besser bewertet sein könnte, liegt damit
                                  nur bei knapp unter 20%.</strong></p>")
                             )
                      )),
                  box(width=12, collapsible = T, collapsed = T, solidHeader = T,
                      title = "Die Simulationsanalyse",
                      column(width = 12,
                      HTML(paste(readLines("www/sim_old.html"), collapse="\n"))),
                      actionBttn(
                        inputId = "runsim",
                        label = "Simulation starten",
                        style = "material-flat",
                        color = "danger",
                        size = "xs"),
                      br(),
                      br(),
                      column(width = 6, align = "center",
                      plotOutput("simvals")),
                      column(width = 6, align = "center",
                      plotOutput("simdiffs")),
                      column(width = 12,
                            br(),
                            br(),
                            uiOutput("simres1")),
                      column(width = 12, align = "center",
                             uiOutput("simres3")
                             ),
                      column(width = 12,
                             uiOutput("simres4")
                             )))),
          tabItem(tabName = "aufg3",
              fluidRow(
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 1: Was ist das primäre Ziel?",
                      HTML("<p>Oft ist das Ziel natürlich, Wartungen zu optimieren und dadurch vorhandenes Equipment 
                      effizienter zu nutzen. Andere Motive sind allerdings auch denkbar, bspw. Unfallschutz oder 
                      Qualitätssicherung. Je nach Ziel ändert sich ggf., welche Indikatoren bei der Bewertung von 
                      Algorithmen angewandt werden. Sollen Wartungen optimiert werden, spielt die Präzision 
                      (die Vermeidung falscher positiver Diagnosen, also von Fehlalarmen) eine zentrale Rolle; 
                      bei der Qualitätssicherung ist die Vermeidung falscher negativer Diagnosen (die Sensitivität) 
                      wichtiger.</p>")),
                  box(width = 12,collapsible = F, solidHeader = T,
                      title = "Frage 2: Welche IT Ressourcen werden momentan zur Organisation der Instandhaltung angewandt?",
                      HTML("<p>Viele Firmen verwenden laut der Studie von PWC weiterhin noch Microsoft Excel für die 
                           Datenbearbeitung rund um die Instandhaltung, andere schon Datenbanksoftware oder 
                           Statistiksoftware. Je nach dem bisherigen Stand der IT ergibt sich ggf. besonderer Aufwand 
                           bei der Datenaufbereitung. Außerdem liegt ggf. auch Bedarf an Training in der Datenbearbeitung 
                           oder Datenanalyse vor.</p>")),
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 3: Wie viele Fälle von reparaturbedürftigen Fehlfunktionen sind erfasst?",
                      HTML("<p>Um einen Algorithmus zur Vorhersage von Inspektions- und Reparaturbedarf trainieren zu 
                           können, braucht es eine hinreichend große Zahl an bisherigen Fehlfunktionen, die als 
                           Datengrundlage genutzt werden können. Sind diese nicht vorhanden (etwa, weil Equipment schon 
                           bei frühen Anzeichen abgeschaltet wird), muss ggf. auf Techniken wie Simulationen 
                           zurückgegriffen werden.</p>"))
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
                                              "Für weitere Informationen auf den Balken klicken bzw. 2x tippen."),
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
                                              "Für weitere Informationen auf den Balken klicken bzw. 2x tippen."),
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
                                              "Für weitere Informationen auf den Punkt klicken bzw. 2x tippen."))) +
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

##### Fisher test
################

output$fish_1 <- renderText({
  round((factorial(90+10)*factorial(2+0)*factorial(90+2)*factorial(10+0)) /
    (factorial(102)*factorial(90)*factorial(2)*factorial(10)*factorial(0)),3)
})

output$fish_2 <- renderText({
  round((factorial(91+9)*factorial(1+1)*factorial(91+1)*factorial(9+1)) /
    (factorial(102)*factorial(91)*factorial(1)*factorial(9)*factorial(1)),3)
})

output$fish_3 <- renderText({
  round((factorial(92+8)*factorial(0+2)*factorial(92+0)*factorial(8+2)) /
    (factorial(102)*factorial(92)*factorial(0)*factorial(8)*factorial(2)),3)
})

##### Graph 2.1
###############

observeEvent(input$runsim,{
  print("blork")
  
  showModal(modalDialog("Simulation läuft...", footer=NULL)) 
  
  # Generate dataset
  df <- data.frame(anb = c(replicate(100, "Anbieter A"),replicate(2, "Anbieter B")),
                 eval = c(c(replicate(90,1),replicate(10,0)),c(1,1)))
  
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
  
  set.seed(17)
  
  shuffles <- mosaic::do(1000) *
    (df %>% 
         mutate(anb = mosaic::shuffle(anb)) %>% 
         group_by(anb) %>% 
         summarize(positive = mean(eval == 1)))
  
  output$simvals <- renderPlot({
    ggplot(shuffles, aes(x=anb,y=positive)) +
    geom_jitter(alpha=.2, width = 0.25, size = 2,color = "#c2224a") +
    scale_y_continuous(breaks = seq(0,1,.1)) +
    ylab("Anteil positive Bewertungen") +
    xlab("") +
    labs(title = "(1)",
         caption = "Die Daten sind zur besseren Lesbarkeit leicht gestreut.") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank(),
          plot.caption = element_text(size = 7))
    
  })
  
  null_dist <- shuffles %>% 
    group_by(.index) %>% 
    summarize(diff = -diff(positive)) # negative of difference to obtain same sign as diff A-B
  
  output$simdiffs <- renderPlot({
  ggplot(null_dist, aes(x = diff)) +
    geom_histogram(color = "white", fill = "#c2224a") +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0,1000),
                       breaks = seq(0,1000,200)) +
    scale_x_continuous(breaks = seq(-0.2,1,.1)) +
    ylab("Häufigkeit") +
    xlab("Differenz im Anteil der positiven Bewertungen (Anb. A - Anb. B)") +
    labs(title = "(2)") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.major.x = element_line(color = "gray", size = .2),
          panel.grid.major.y = element_blank())
  
  })
  
  p <- sum(round(null_dist$diff<=0),2)/1000
  
  output$simres1 <- renderUI({
          HTML(paste(readLines("www/aufg2_simtext2.html"), collapse="\n"))
  })
  
  removeModal()
})

}

# Run the application 
shinyApp(ui, server)
