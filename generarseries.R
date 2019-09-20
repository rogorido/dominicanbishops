#!/usr/bin/Rscript --vanilla

# fichero general de análisis de la Edad Moderna. 
# Esto no es para shiny! Es para generar plots 

library(RPostgreSQL)
#library(DescTools)
library(tidyverse)

con <- dbConnect(drv= "PostgreSQL",
                 dbname = "dominicos",
                 user="igor", host="localhost")

# para usarlo luego con el parámetro path= de ggsave
dir_edm = "/home/igor/geschichte/artikel/obisposdominicos/analisis/plots/edadmoderna"
dir_emd = "/home/igor/geschichte/artikel/obisposdominicos/analisis/plots/periodolargo"

source("./dash/functions.R")

#######################################
# Edad moderna 
#######################################

datossql <- getSQL("./dash/sqls/ordenes_seriestemporales_edm.sql")
orrg_series_edm_general <- dbGetQuery(con, datossql)
orrg_series_edm_general$order_acronym <- factor(orrg_series_edm_general$order_acronym)
orrg_series_edm_general$country <- factor(orrg_series_edm_general$country)

p <- ggplot(orrg_series_edm_general, aes(x=serie, y= totalobispos)) +
    geom_line() + facet_wrap(~order_acronym)

ggsave(p, filename= 'series_obispos_general.png',
       path = dir_edm)

## Esta versión es juntando los tipos de OFMs
orrg_series_edm_unido <- orrg_series_edm_general
orrg_series_edm_unido$order_acronym <-
    fct_collapse(orrg_series_edm_unido$order_acronym,
                 "O.F.M." = c("O.F.M.", "O.F.M. Conv."))

p <- ggplot(orrg_series_edm_unido, aes(x=serie, y= totalobispos)) +
    geom_line() + facet_wrap(~order_acronym)

ggsave(p, filename= 'series_obispos_ofm.png',
       path = dir_edm)

# ponemos el axis x de 1500 a 1800 pq si no, ggplot lo adapta

for (i in levels(orrg_series_edm_unido$order_acronym)) {

    # quitamos los puntos de los nombres de las órdenes religiosas 
    nombre = gsub("\\.", "", i)
    nombre = gsub(" ", "", nombre)

    p <- orrg_series_edm_unido %>%
        filter(order_acronym == i) %>%
        ggplot(aes(x = serie, y = totalobispos)) +
        geom_line(size = 1.2) +
        scale_x_continuous(limits = c(1500, 1800)) +
        labs(title = paste0("Total de obispos de ", nombre),
             x = "Años",
             y = "Total de obispos") + 
        theme_light()

    ggsave(p, filename = paste0("order_",nombre,".png"),
           path = dir_edm)
}

#### tenemos tb en cuenta los países

for (i in levels(orrg_series_edm_unido$order_acronym)) {

    # quitamos los puntos de los nombres de las órdenes religiosas 
    nombre = gsub("\\.", "", i)
    nombre = gsub(" ", "", nombre)

    p <- orrg_series_edm_unido %>%
        filter(order_acronym == i) %>%
        ggplot(aes(x = serie, y = totalobispos)) +
        geom_line(size = 0.9) +
        facet_wrap(~country) +
        scale_x_continuous(limits = c(1500, 1800)) +
        labs(title = paste0("Total de obispos de ", nombre, " (por país)"),
             x = "Años",
             y = "Total de obispos") + 
        theme_light()
        
    ggsave(p, filename = paste0("order_",nombre,"_paises.png"),
           path = dir_edm)
}

#######################################
# Perioodo 1200-1800 (emd)
#######################################

datossql <- getSQL("./dash/sqls/ordenes_seriestemporales_emd.sql")
orrg_series_emd_general <- dbGetQuery(con, datossql)
orrg_series_emd_general$order_acronym <- factor(orrg_series_emd_general$order_acronym)
orrg_series_emd_general$country <- factor(orrg_series_emd_general$country)

p <- ggplot(orrg_series_emd_general, aes(x=serie, y= totalobispos)) +
    geom_line() + facet_wrap(~order_acronym)

ggsave(p, filename= 'series_obispos_general.png',
       path = dir_emd)

## Esta versión es juntando los tipos de OFMs
orrg_series_emd_unido <- orrg_series_emd_general
orrg_series_emd_unido$order_acronym <-
    fct_collapse(orrg_series_emd_unido$order_acronym,
                 "O.F.M." = c("O.F.M.", "O.F.M. Conv."))

p <- ggplot(orrg_series_emd_unido, aes(x = serie, y = totalobispos)) +
    geom_line() + facet_wrap(~order_acronym)

ggsave(p, filename= 'series_obispos_ofm.png',
       path = dir_emd)

# ponemos el axis x de 1200 a 1800 pq si no, ggplot lo adapta

for (i in levels(orrg_series_emd_unido$order_acronym)) {

    # quitamos los puntos de los nombres de las órdenes religiosas 
    nombre = gsub("\\.", "", i)
    nombre = gsub(" ", "", nombre)
    p <- orrg_series_emd_unido %>%
        filter(order_acronym == i) %>%
        ggplot(aes(x=serie, y= totalobispos)) +
        geom_line(size = 1.2) +
        scale_x_continuous(limits = c(1200, 1800)) +
        labs(title = paste0("Total de obispos de ", nombre),
             x = "Años",
             y = "Total de obispos") + 
        theme_light()

    ggsave(p, filename = paste0("order_",nombre,".png"),
           path = dir_emd)
}

#### tenemos tb en cuenta los países

for (i in levels(orrg_series_emd_unido$order_acronym)) {

    # quitamos los puntos de los nombres de las órdenes religiosas 
    nombre = gsub("\\.", "", i)
    nombre = gsub(" ", "", nombre)

    p <- orrg_series_emd_unido %>%
        filter(order_acronym == i) %>%
        ggplot(aes(x = serie, y = totalobispos)) +
        geom_line(size = 0.9) +
        facet_wrap(~country) +
        labs(title = paste0("Total de obispos de ", nombre, " (por país)"),
             x = "Años",
             y = "Total de obispos") + 
        theme_light()
        
    ggsave(p, filename = paste0("order_",nombre,"_paises.png"),
           path = dir_emd)
}
