#!/usr/bin/Rscript --vanilla

library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets) # https://github.com/dreamRs/shinyWidgets
library(RPostgreSQL)
library(leaflet)
library(DT)
library(htmltools) # para usar htmlEscape con leaflet 

setwd("/home/igor/geschichte/artikel/obisposdominicos/analisis/dash/")

source("./functions.R")

con <- dbConnect(drv= "PostgreSQL",
                 dbname = "dominicos",
                 user="igor",
                 host="localhost")

source("./loaddata/load_general.R")
source("./loaddata/load_diocesis.R")
source("./loaddata/load_orrgs.R")
source("./loaddata/load_confesionalization.R")
# aquí está los tabs del dashboardbody
source("./tabitems.R", local = TRUE)

# esto sirve para q muestre errores en la consola de firefox 
# options(shiny.trace = TRUE)

ui <- dashboardPage( skin = "gree",
  dashboardHeader(title = "Obispos católicos"),
  dashboardSidebar(sidebarMenu(
      menuItem("General", tabName = "general", icon = icon("dashboard")),
      # menuItem("Descriptiva", tabName = "descriptiva", icon = icon("dashboard")),
      menuItem("Diócesis", tabName = "diocesis", icon = icon("th"),
               menuItem("General", tabName = "d_general", icon = icon("th")),
               menuItem("Sec-vs-Nosec", tabName = "d_secnosec", icon = icon("th")),
               menuItem("Duración", tabName = "d_duracion", icon = icon("th")),
               menuItem("Papas", tabName = "d_papas", icon = icon("th")),
               menuItem("Rentas", tabName = "d_rentas", icon = icon("th")),
               menuItem("Periplos (seculares)", tabName = "d_periplo", icon = icon("th")),
               menuItem("Fin", tabName = "d_fin", icon = icon("th"))
               ),
      menuItem("Órdenes religiosas", tabName = "ops", icon = icon("th"),
               menuItem("Órdenes", tabName = "o_ordenesgeneral", icon = icon("th")),
               menuItem("Consulta general", tabName = "o_consultageneral", icon = icon("th")),
               menuItem("Presencia en diócesis", tabName = "o_diocesis", icon = icon("th")),
               menuItem("Diócesis sin órdenes", tabName = "d_orrg", icon = icon("th")),
               menuItem("Periplos", tabName = "o_periplos", icon = icon("th")),
               menuItem("Series temporales", tabName = "o_series", icon = icon("th")),
               menuItem("Motivos fin", tabName = "o_motivosfin", icon = icon("th")),
               menuItem("Monasterio/obispado", tabName = "o_monasterioobispado", icon = icon("th")),
               menuItem("Bizancio", tabName = "o_bizancio", icon = icon("th"))
               ),
      menuItem("Confesionalización", tabName = "confesional", icon = icon("th"),
               menuItem("Órdenes", tabName = "c_ordenes", icon = icon("th")),
               menuItem("Por años", tabName = "c_ordenes_anos", icon = icon("th"))
               )
  )),

  # body content
  dashboardBody(variadostabs))

server <- function(input, output) {

    source("./outputs/out_dioceses.R", local = TRUE)
    source("./outputs/out_orrgs.R", local = TRUE)
    source("./outputs/out_confesionalization.R", local = TRUE)

}

shinyApp(ui, server)

