

library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(ggiraph)

ui <- dashboardPage(
  dashboardHeader(title = "Data Science Herausforderung", titleWidth = 300),
  dashboardSidebar(
      sidebarMenu(
          menuItem("Start", tabName = "start"),
          menuItem("Meine Lösung", tabName = "solu",
          menuSubItem("Aufgabe 1", tabName = "aufg1"), #, icon = icon("chart-bar", lib = "font-awesome")
          menuSubItem("Aufgabe 2", tabName = "aufg2"), #, icon = icon("amazon", lib = "font-awesome")
          menuSubItem("Aufgabe 3", tabName = "aufg3")) #, icon = icon("cogs", lib = "font-awesome")
  )),
  dashboardBody(
      shinyDashboardThemes(theme = "flat_red"),
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
                  box(width = 12,collapsible = F,solidHeader = T,
                      title = "Athleten & Medaillen bei den Olympischen Sommerspielen 2016")
              )),
          tabItem(tabName = "aufg2",
              fluidRow(
                  box(width = 12, collapsible = F, solidHeader = T,
                      title = "Amazon-Kundenbewertungen")
              )),
          tabItem(tabName = "aufg3",
              fluidRow(
                  box(width = 12,collapsible = T, solidHeader = T,collapsed = T,
                      title = "Frage 1:"),
                  box(width = 12,collapsible = T,solidHeader = T,collapsed = T,
                      title = "Frage 2: "),
                  box(width = 12,collapsible = T,solidHeader = T,collapsed = T,
                      title = "Frage 3: ")
              ))
  ))
)

server <- function(input, output, session) {


}

# Run the application 
shinyApp(ui = ui, server = server)
