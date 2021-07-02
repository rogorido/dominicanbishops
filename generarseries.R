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
dir_cph = "/home/igor/geschichte/artikel/obisposdominicos/analisis/plots/cph"

source("./dash/functions.R")

#######################################
# Edad moderna 
#######################################

datossql <- getSQL("./sqls/ordenes_seriestemporales_edm.sql")
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

#######################################
# Perioodo 1300-1600
#######################################

datossql <- getSQL("./sql/series_temporales_cph_op.sql")
op_cph <- dbGetQuery(con, datossql)
op_cph$country <- factor(op_cph$country)

op_cph_agg <- op_cph %>%
        group_by(serie) %>%
        summarise(total = sum(totalobispos))


p <- ggplot(op_cph_agg, aes(x=serie, y= total)) +
    geom_line(size = 1.6) +
    labs(x = element_blank(),
         y = "# of bishops")

p + theme_light() +
    theme(axis.title = element_text(size = rel(2)),
          axis.text = element_text(size = rel(2)))

p +
    theme_bw() + 
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank()) +
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)) +
    theme(axis.title = element_text(size = rel(2)),
          axis.text = element_text(size = rel(2)))

op_cph %>%
    filter(country %in% c("Spain", "France", "Italy", "Germany")) %>%
    ggplot(., aes(x=serie, y= totalobispos, color = country)) +
    geom_line(size = 1.6) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank()) +
    theme(panel.border= element_blank())+
    theme(axis.line.x = element_line(color="black", size = 1),
          axis.line.y = element_line(color="black", size = 1)) +
    theme(axis.title = element_text(size = rel(2)),
          axis.text = element_text(size = rel(2)))


# otra versión de lo mismo pero con wrap 
op_cph %>%
    filter(country %in% c("Spain", "France", "Italy", "Germany")) %>%
    ggplot(., aes(x=serie, y= totalobispos)) +
    geom_line(size = 1.3) + facet_wrap(~country) + 
    labs(x = element_blank(),
         y = "# of bishops") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank()) +
    theme(panel.border= element_blank()) +
    theme(text = element_text(size = 20))  +
    theme(panel.grid.major.y = element_line(colour = "grey80"))

# los de eslovenia, etc.
op_cph$countrycollapsed <- op_cph$country
op_cph$countrycollapsed <-
    fct_collapse(op_cph$countrycollapsed,
                 Balcans = c("Bosnia and Herzegovina", "Croacia",
                      "Slovenia", "Montenegro"))

op_cph %>%
    filter(countrycollapsed == "Balcans") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3)
    labs(x = element_blank(),
         y = "# of bishops") +
    theme(plot.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank()) +
    theme(panel.border= element_blank()) +
    theme(text = element_text(size = 20))  +
    theme(panel.grid.major.y = element_line(colour = "grey80"))


j <-op_cph %>%
    filter(countrycollapsed == "Balcans") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))

j <- j %>% filter(serie=="1501-01-01")

ggsave(p, filename= 'series_obispos_general.png',
       path = dir_emd)


## for (i in levels(orrg_series_emd_unido$order_acronym)) {

##     # quitamos los puntos de los nombres de las órdenes religiosas 
##     nombre = gsub("\\.", "", i)
##     nombre = gsub(" ", "", nombre)

##     p <- orrg_series_emd_unido %>%
##         filter(order_acronym == i) %>%
##         ggplot(aes(x = serie, y = totalobispos)) +
##         geom_line(size = 0.9) +
##         facet_wrap(~country) +
##         labs(title = paste0("Total de obispos de ", nombre, " (por país)"),
##              x = "Años",
##              y = "Total de obispos") + 
##         theme_light()
        
##     ggsave(p, filename = paste0("order_",nombre,"_paises.png"),
##            path = dir_emd)
## }

