#!/usr/bin/Rscript --vanilla

library(dplyr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(RPostgreSQL)
library(leaflet)
library(DT)

setwd("/home/igor/geschichte/artikel/obisposdominicos/analisis")
source("./cargardatos.R")

ui <- dashboardPage( skin = "gree",
  dashboardHeader(title = "Obispos católicos"),
  dashboardSidebar(sidebarMenu(
      menuItem("General", tabName = "general", icon = icon("dashboard")),
      menuItem("Descriptiva", tabName = "descriptiva", icon = icon("dashboard")),
      menuItem("Mapas OP", tabName = "mapas_op", icon = icon("th"),
               menuItem("Duración", tabName = "m_ops", icon = icon("th")),
               menuItem("Mapas2", tabName = "mapas2", icon = icon("th")),
               menuItem("Mapas2", tabName = "mapas3", icon = icon("th"))),
      menuItem("Mapas (Otros)", tabName = "mapas_otros", icon = icon("th"),
               menuItem("Mapas2", tabName = "m_otros", icon = icon("th"))),
      menuItem("Tablas", tabName = "tablas", icon = icon("th"),
               menuItem("Dominicos", tabName = "t_ops", icon = icon("th")),
               menuItem("Órdenes religiosas", tabName = "t_org", icon = icon("th"))),
      menuItem("Edad Moderna", tabName = "edm", icon = icon("th"),
               menuItem("Órdenes religiosas", tabName = "t_org_edm", icon = icon("th")))
  )),

  # body content
  dashboardBody(
    tabItems(
      # First tab content
        tabItem(tabName = "general",
                fluidRow(
                    valueBox(total_b_sinseculares_conafil, "Obispos regulares(con afiliados)", icon = icon("thumbs-up", lib = "glyphicon")),
                    valueBox(total_b_sinseculares_sinafil, "Obispos regulares(sin afiliados)", icon = icon("thumbs-up", lib = "glyphicon"))
                    )),

        tabItem(tabName = "descriptiva",
                fluidRow(
                    box(plotOutput("plotduracion", height = 250)),

                    box(
                        title = "Controls",
                        sliderInput("sliderduracion", "Años:", 1200, 1800, value=c(1200, 1800))
                    )
                )),

        # Second tab content
        tabItem(tabName = "m_ops",
                box(title = "Duración de obispados", width = "100%",
                    leafletOutput("mymap", height = 600)
                    )),

        # tercero 
        tabItem(tabName = "t_org",
                box(title="Tabla", DT::dataTableOutput("mytable1")),
                box(title="Tabla", DT::dataTableOutput("mytable2")),
                box(title="Tabla", DT::dataTableOutput("mytable3"))
                ),
        
        # Edad Moderna  
        tabItem(tabName = "t_org_edm",
                selectInput("seleccionar_orden", label = h3("Seleccionar orden"), 
                            choices = ordenes_lista,
                            selected = 'O.P.'),
                tabBox(
                    id = "tabset1", height = "100", width="100",
                    tabPanel("Tab1",
                             #box(title="Tabla", DT::dataTableOutput("mytable4"))),
                             DT::dataTableOutput("mytable4")),
                    tabPanel("Tab2", box(title = "Duración de obispados", width = "100%",
                                         leafletOutput("mymap2", height = 600)))
                )
                
                )
    )
  )
)

server <- function(input, output) {

    output$plotduracion <- renderPlot({
        anos <- input$sliderduracion
        dur <- dplyr::filter(duracion, inicio >= anos[1], fin <= anos[2])
        hist(dur$duracion)
    })

    output$mymap <- renderLeaflet({
        leaflet(bis_per_dioc) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude, radius = ~total)
    })

    output$mymap2 <- renderLeaflet({
        em_dioc <- actualizar_em_orden(input$seleccionar_orden)
        leaflet(em_dioc) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude, radius = ~count)
    })

    output$mytable1 <- renderDataTable({
        bish_todos_sinseculares_conafil %>%
            group_by(order_acronym) %>%
            summarise (n = n()) %>%
            mutate(freq = round(n / sum(n), 2)) %>%
            arrange(-freq)
    })

    output$mytable2 <- renderDataTable({
        bish_todos_sinseculares_conafil_por_diocesis_totales
    })

    output$mytable3 <- renderDataTable({
        bish_todos_sinseculares_conafil_por_diocesis_porcentaje
    })

    output$mytable4 <- renderDataTable({
        em_dioc <- actualizar_em_orden(input$seleccionar_orden)
        em_dioc %>% select(diocesis, count)
        })
}

shinyApp(ui, server)


