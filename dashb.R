#!/usr/bin/Rscript --vanilla

library(tidyverse)
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
               menuItem("Órdenes religiosas (totales)", tabName = "t_org_edm", icon = icon("th")))
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
                tabsetPanel(
                    id = "tabset2",# height = "100", width="100",
                    tabPanel("tabla1", 
                             box(title="Tabla", DT::dataTableOutput("mytable1"))),
                    tabPanel("tabla2", 
                             DT::dataTableOutput("mytable2")),
                    tabPanel("tabla3", 
                             DT::dataTableOutput("mytable3"))
                )),
        
        # Edad Moderna  
        tabItem(tabName = "t_org_edm",
                selectInput("seleccionar_orden", label = 'Probando',
                            choices = ordenes_lista,
                            selected = 'O.P.'),
                tabsetPanel(
                    id = "tabset1", #height = "100", width="100",
                    tabPanel("Totales de obispos", DT::dataTableOutput("mytable4")),
                    tabPanel("Duración en años", DT::dataTableOutput("mytable5")),
                    tabPanel("Por países", DT::dataTableOutput("mytable6")),
                    tabPanel("Mapa (total)", leafletOutput("mymap2", height = 600)),
                    tabPanel("Mapa (duración)", leafletOutput("mymap3", height = 600)),
                    tabPanel("Evolución", plotOutput("plotevolucion"))
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

    output$plotevolucion <- renderPlot({
        em_bishops_per_year <- actualizar_em_evolucion(input$seleccionar_orden)
        ggplot(em_bishops_per_year, aes(x=serie, y=count)) + geom_line()
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

    output$mymap3 <- renderLeaflet({
        em_dioc_duracion <- actualizar_em_duracion(input$seleccionar_orden)
        leaflet(em_dioc_duracion) %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
                             ) %>%
            addCircleMarkers(lat = ~latitude, lng= ~longitude, radius = ~duracion)
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
        em_dioc %>% select(diocesis, count) %>% arrange(-count)
    })

    output$mytable5 <- renderDataTable({
        em_dioc_duracion <- actualizar_em_duracion(input$seleccionar_orden)
        em_dioc_duracion %>% select(diocesis, duracion) %>% arrange(-duracion)
        })

    output$mytable6 <- renderDataTable({
        em_por_pais <- actualizar_em_porpais(input$seleccionar_orden)
        em_por_pais %>% select(pais, total) %>% arrange(-total)
        })

    output$mytable7 <- renderDataTable({
        em_por_pais_duracion <- actualizar_em_porpais_duracion(input$seleccionar_orden)
        em_por_pais_duracion %>% select(pais, duracion) %>% arrange(-duracion)
        })

}

shinyApp(ui, server)


