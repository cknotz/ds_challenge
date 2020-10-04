

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggiraph)
library(BayesianFirstAid)
library(bayestestR)

table <- readRDS("www/backup.rds")
names(table)[names(table)=="Total"] <- "Gesamt"

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
                      box(width = 12, collapsible = F,solidHeader = T, #<p><strong>Herausforderungen stelle ich mich immer gerne.</strong></p>
                          HTML("<p>Meine Lösungen für die drei Aufgaben stelle ich Ihnen über dieses interaktive Dashboard vor.</p>
                               <p>Zu meinen Lösungen:
                               <ul>
                               <li>Die Daten zur Zahl der Athleten und der gewonnenen Medaillen bei den Olympischen 
                               Sommerspielen 2016 habe ich direkt von Wikipedia eingelesen ('gescraped'; <a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics#Number_of_athletes_by_National_Olympic_Committee'>Quelle für die Zahl der Athleten</a>; 
                               <a target='_blank'
                               href='https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table#Medal_table'>
                               Quelle für die Zahl der Medaillen</a>). Bei der Zahl der Athleten beschränke ich mich auf
                               die ersten 75 Länder aus 207, um das Schaubild lesbar zu halten.</li>
                               <li>Der Code, um die Daten von Wikipedia einzulesen sowie der Code für dieses Dashbord (inkl. der bayesianischen Schätzung für Aufg. 2) sind auf
                               meinem <a href='https://github.com/cknotz/ds_challenge' target='_blank'>Github Profil</a> hinterlegt.</li>
                               <li>Bei den Antworten auf Frage 3 beziehe ich mich u.a. auf zwei Studien zu Predictive Maintenance
                               Praktiken von PricewaterhouseCoopers (<a target='_blank href='https://www.pwc.nl/nl/assets/documents/pwc-predictive-maintenance-4-0.pdf'>2017</a>; <a target='_blank'
                               href='https://www.mainnovation.com/wp-content/uploads/tmp/6397245268d8d3711c88cda0b4585ab02e612f2e.pdf'>2018</a>).
                               </li>
                               </ul>
                               Mir hat die Challenge viel Spaß gemacht und ich hoffe natürlich, dass Sie meine Antworten überzeugen.
                               Mein genaues Vorgehen und die Resultate bespreche ich sehr gerne näher mit Ihnen in einem persönlichen Gespräch.
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
                      HTML(readLines("www/bayes_intro.html")),
                      column(width=4,
                      actionBttn(
                        inputId = "runsim",
                        label = "Schätzung ausführen",
                        style = "material-flat",
                        color = "danger",
                        size = "xs"),
                      br(),
                      br()),
                      column(width = 12,
                            uiOutput("bayestext1"),
                      column(width = 6,
                             plotOutput("anbA")),
                      column(width = 6,
                             plotOutput("anbB"),
                             br()),
                            uiOutput("bayestext2")),
                      column(width=10,
                             plotOutput("diffplot"),
                             br()),
                      column(width = 12,
                             uiOutput("bayestext3")
                             )
                      ))),
          tabItem(tabName = "aufg3",
              fluidRow(
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 1: Was ist das primäre Ziel für den Einsatz von Predictive Maintenance Methoden?",
                      HTML("<p>Oft ist das Ziel natürlich, Wartungen zu optimieren und dadurch vorhandenes Equipment 
                      effizienter zu nutzen. Andere Motive sind allerdings auch denkbar, beispielsweise Unfallschutz oder 
                      Qualitätssicherung. Je nach Ziel ändert sich gegebenenfalls, welche Indikatoren beim Training von 
                      Algorithmen angewandt werden. Sollen Wartungen optimiert werden, spielt die Präzision 
                      (die Vermeidung falscher positiver Diagnosen, also von Fehlalarmen) eine zentrale Rolle; 
                      bei der Qualitätssicherung ist die Vermeidung falscher negativer Diagnosen (die Sensitivität) 
                      wichtiger.</p>")),
                  box(width = 12,collapsible = F, solidHeader = T,
                      title = "Frage 2: Wie werden die Daten zur Instandhaltung erfasst und gespeichert?",
                      HTML("<p>Viele Firmen verwenden laut der Studie von PWC weiterhin noch Microsoft Excel für die 
                           Datenbearbeitung rund um die Instandhaltung, andere dagegen schon Datenbanksoftware. Je weniger
                           systematisch die Daten erfasst und gespeichert werden, desto größer ist unter Umständen der Aufwand 
                           bei der Aufbereitung der Daten.</p>")),
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Frage 3: Wie viele Fälle von reparaturbedürftigen Fehlfunktionen sind erfasst?",
                      HTML("<p>Um einen Algorithmus zur Vorhersage von Inspektions- und Reparaturbedarf trainieren zu 
                           können, braucht es eine hinreichend große Zahl an bisherigen Fehlfunktionen, die als 
                           Datengrundlage genutzt werden können. Sind diese nicht vorhanden (etwa, weil Equipment schon 
                           bei frühen Anzeichen abgeschaltet wird), muss vielleicht auf Techniken wie Simulationen 
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

##### Graphs 2.1-3
##################

observeEvent(input$runsim,{
  
  set.seed(42)
  no_pos <- c(90,2)
  no_eval <- c(100,2)
  
  fit <- bayes.prop.test(no_pos,no_eval)
  
  s <- as.data.frame(fit)
  s$diff <- s$theta1-s$theta2
  hdi_diff <- bayestestR::ci(s$diff, method="HDI",ci=0.95)
  hdi_anbA <- bayestestR::ci(s$theta1, method="HDI",ci=0.95)
  hdi_anbB <- bayestestR::ci(s$theta2, method="HDI",ci=0.95)
  
  print(mean(s$diff>0))
  
  output$diffplot <- renderPlot({
    ggplot(s,aes(x=diff)) +
      geom_density(color="#c2224a", fill="#c2224a", alpha=.25) +
      geom_vline(xintercept = median(s$diff), linetype="dashed", size=0.25) +
      geom_segment(aes(x=hdi_diff$CI_low,xend=hdi_diff$CI_high,y=0.02,yend=0.02), size = 0.25) +
      annotate(geom="text",x=hdi_diff$CI_low,y=0.1,label=as.character(round(hdi_diff$CI_low,2))) +
      annotate(geom="text",x=hdi_diff$CI_high,y=0.1,label=as.character(round(hdi_diff$CI_high,2))) +
      annotate(geom = "text", x=median(s$diff)-0.025, y=1.5, label=as.character(round(median(s$diff),2))) +
      annotate(geom = "label", x= .7, y=2.25,  label = paste0("p(Diff. > 0) = ",as.character(round(mean(s$diff>0),3)))) +
      scale_x_continuous(breaks = seq(-0.2,0.9,0.1), limits = c(-0.25,0.95)) +
      scale_y_continuous(expand = c(0,0),limits = c(0,2.75), breaks = seq(0,2.5,0.5)) +  
      xlab("Differenz im Anteil pos. Bewertungen (Anb.A - Anb. B)") +
      ylab("Dichte") +
      labs(title = "(3)") +
      theme_bw() +
        theme(panel.grid = element_blank())
  })
  
  output$anbA <- renderPlot({
    ggplot(s,aes(x=theta1)) +
      geom_density(color="#585858", fill="#585858", alpha=.25) +
      geom_vline(xintercept = median(s$theta1), linetype="dashed", size=0.25) +
      geom_segment(aes(x=hdi_anbA$CI_low,xend=hdi_anbA$CI_high,y=0.075,yend=0.075), size = 0.25) +
      annotate(geom="text",x=hdi_anbA$CI_low,y=0.5,label=as.character(round(hdi_anbA$CI_low,2)),color="#c2224a") +
      annotate(geom="text",x=hdi_anbA$CI_high,y=0.5,label=as.character(round(hdi_anbA$CI_high,2)),color="#c2224a") +
      annotate(geom = "text", x=median(s$theta1)-0.05, y=7.5, label=as.character(round(median(s$theta1),2))) +
      scale_y_continuous(expand = c(0,0),limits = c(0,16), breaks = seq(0,15,5)) + 
      scale_x_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) +
      xlab("Anteil pos. Bewertungen für Anbieter A") +
      ylab("Dichte") +
      labs(title = "(1)") +
      theme_bw() +
        theme(panel.grid = element_blank()) 
  })
  
  output$anbB <- renderPlot({
    ggplot(s,aes(x=theta2)) +
      geom_density(color="#585858", fill="#585858", alpha=.5) +
      geom_vline(xintercept = median(s$theta2), linetype="dashed", size=0.25) +
      geom_segment(aes(x=hdi_anbB$CI_low,xend=hdi_anbB$CI_high,y=0.075,yend=0.075), size = 0.25) +
      annotate(geom="text",x=hdi_anbB$CI_low,y=0.5,label=as.character(round(hdi_anbB$CI_low,2)),color="#c2224a") +
      annotate(geom="text",x=hdi_anbB$CI_high,y=0.5,label=as.character(round(hdi_anbB$CI_high,2)),color="#c2224a") +
      annotate(geom = "text", x=median(s$theta2)-0.05, y=7.5, label=as.character(round(median(s$theta2),2))) +
      scale_y_continuous(expand = c(0,0),limits = c(0,16), breaks = seq(0,15,5)) +
      scale_x_continuous(breaks = seq(0,1,0.1)) +
      xlab("Anteil pos. Bewertungen für Anbieter B") +
      ylab("Dichte") +
      labs(title = "(2)") +
      theme_bw() +
        theme(panel.grid = element_blank())
  })
  
  output$bayestext1 <- renderUI({
    HTML(readLines("www/bayestext1.html"))
  })
  
  output$bayestext2 <- renderUI({
    HTML(readLines("www/bayestext2.html"))
  })
  
  output$bayestext3 <- renderUI({
    HTML(readLines("www/bayestext3.html"))
  })
  
})


}

# Run the application 
shinyApp(ui, server)
