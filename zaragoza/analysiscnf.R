#!/usr/bin/Rscript --vanilla

# fichero general de análisis de la Edad Moderna. 
# Esto no es para shiny! Es para generar plots 

library(RPostgreSQL)
library(DescTools)
library(tidyverse)
library(sf)

con <- dbConnect(drv= "PostgreSQL",
                 dbname = "dominicos",
                 user="igor", host="localhost")

setwd("/home/igor/geschichte/artikel/obisposdominicos/analisis/zaragoza")
source("../functions.R")
source("./owntheme_cnf.R")
source("./variables.R")

dir_cnf = "/home/igor/geschichte/artikel/obisposdominicos/analisis/plots/cnf"

#######################################
# seculars vs non seculares
#######################################

## atención estos datos están mal pq está contando los obispos varias veces
sql <- getSQL("../sql/cnf/typebishops_cnf.sql")
typebishops <- dbGetQuery(con, sql)

p <- ggplot(typebishops, aes(x= bishoptype, y=total)) +
    geom_bar(stat="identity", fill = "#a12828") +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa()

typebishops$percentage <- typebishops$total * 100 /
    sum(typebishops$total)

ggsave(p, filename= 'type_bishops.png',
       path = dir_cnf)

#######################################
# datos generales de obispos OPs con sus duraciones 
#######################################

sql <- "SELECT * from vistas.bishops_individuals_cnf_op;"
ops_cnf_generaldata <- dbGetQuery(con, sql)

# historiograma 
p <- ggplot(ops_cnf_generaldata, aes(x=anos)) +
    geom_histogram(fill = "#a12828") +
    labs(x = "# of years",
         y = "# of persons") +
    theme_sosa()
    
ggsave(p, filename= 'histogram_no_years.png',
       path = dir_cnf)

# función cumulativa 
ggplot(ops_cnf_generaldata, aes(x=anos)) + stat_ecdf(geom = "step")

# los valores
anos.ecdf <- ecdf(ops_cnf_generaldata$anos)

# y así podemos saber dónde está pej los diez años
anos.ecdf(10)
# [1] 0.6796117
anos.ecdf(c(1:3))
# [1] 0.07119741 0.17152104 0.26537217

quantile(ops_cnf_generaldata$anos, probs = seq(0,1,0.1),
         na.rm =TRUE )                                                              

quantile(ops_cnf_generaldata$anos,
         na.rm =TRUE )

Desc(ops_cnf_generaldata$anos)
Desc(ops_cnf_generaldata$anos ~ factor(ops_cnf_generaldata$ordinal))

# hacer ttest entrer OPs y seculares?

#######################################
# dioceses con nº de OPs obispos 
#######################################

sql <- getSQL("../sql/cnf/aggdioceses_cnf.sql")
aggdioceses <- dbGetQuery(con, sql)

# nos muestra una pequeña tabla conle número de diocesis con 1, 2, 3, obispos
aggdioceses %>%
    group_by(factor(total))%>%
    summarise(cuantos = n())

# lo mismo pero como historiograma
p<- aggdioceses %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'bishops_number_dioceses.png',
       path = dir_cnf)

## ITaly

italy_number_dioceses <- aggdioceses %>%
    filter (country == 'Italy')
Desc(factor(italy_number_dioceses$total))

# coger? Solo Itlaia 
p<- aggdioceses %>%
    filter (country == 'Italy') %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'italy_number_dioceses.png',
       path = dir_cnf)

aggdioceses_sf <- st_as_sf(aggdioceses,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(aggdioceses_sf, "aggdioceses_sf.geojson", append = FALSE)

## Francia
france_number_dioceses <- aggdioceses %>%
    filter (country == 'France')
Desc(factor(france_number_dioceses$total))

p<- aggdioceses %>%
    filter (country == 'France') %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'france_number_dioceses.png',
       path = dir_cnf)

aggdioceses_sf <- st_as_sf(aggdioceses,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(aggdioceses_sf, "aggdioceses_sf.geojson", append = FALSE)

## Irland
ireland_number_dioceses <- aggdioceses %>%
    filter (country == 'Ireland')
Desc(factor(ireland_number_dioceses$total))

p<- aggdioceses %>%
    filter (country == 'Ireland') %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'ireland_number_dioceses.png',
       path = dir_cnf)

aggdioceses_sf <- st_as_sf(aggdioceses,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(aggdioceses_sf, "aggdioceses_sf.geojson", append = FALSE)

## Spain
spain_number_dioceses <- aggdioceses %>%
    filter (country == 'Spain')
Desc(factor(spain_number_dioceses$total))

# solo spain
p<- aggdioceses %>%
    filter (country == 'Spain') %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'spain_number_dioceses.png',
       path = dir_cnf)

aggdioceses_sf <- st_as_sf(aggdioceses,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(aggdioceses_sf, "aggdioceses_sf.geojson", append = FALSE)

## américa
america_number_dioceses <- aggdioceses %>%
    filter (country %in% c_america)
Desc(factor(america_number_dioceses$total))

p<- aggdioceses %>%
    filter (country %in% c_america) %>%
    ggplot(., aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'america_number_dioceses.png',
       path = dir_cnf)

aggdioceses_sf <- st_as_sf(aggdioceses,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(aggdioceses_sf, "aggdioceses_sf.geojson", append = FALSE)


#######################################
# sumamos los años q están en cada 
#######################################

sql <- getSQL("../sql/cnf/totalyears.sql")
totalyears <- dbGetQuery(con, sql)

totalyears.agg <-
    totalyears %>%
    group_by(country) %>%
    summarise(yearsagg = sum(total))

ggplot(totalyears.agg, aes(x = reorder(country, yearsagg), y = yearsagg)) +
    geom_bar(stat="identity") +
    coord_flip()

Desc(totalyears$total)
Desc(totalyears.agg$yearsagg)

totalyears_filtered <- totalyears %>% drop_na()

totalyears_sf <- st_as_sf(totalyears_filtered,
                          coords = c("longitude", "latitude"),
                          agr = "constant")
st_write(totalyears_sf, "totalyears_sf.geojson", append = FALSE)


#######################################
# en el número de diócesis q están 
#######################################

sql <- getSQL("../sql/cnf/numberindioceses_cnf.sql")
numberindioceses <- dbGetQuery(con, sql)

# coger? 
p<- ggplot(numberindioceses, aes(x=factor(total))) +
    geom_bar(fill = "#a12828") +
    labs(x = "Presence in # of dioceses",
         y = "# of persons") +
    theme_sosa()

ggsave(p, filename= 'number_dioceses_seguidas.png',
       path = dir_cnf)

Desc(numberindioceses$total)

#######################################
# obispos con más de una diócesis 
#######################################

sql <- getSQL("../sql/morethanonediocesis.sql")
moredioceses <- dbGetQuery(con, sql)

ggplot(moredioceses, aes(x=factor(total))) + geom_bar()

#######################################
# análisis más detallado de los q están en 2
#######################################

sql <- getSQL("../sql/cnf/bishopsin2dioceses_cnf.sql")
intwodioceses <- dbGetQuery(con, sql)

intwodioceses$diocese_name <- factor(intwodioceses$diocese_name)
intwodioceses$country <- factor(intwodioceses$country)

Desc(intwodioceses$diocese_name) # intwodiocesesperdiocese

Desc(intwodioceses$country)

Desc(intwodioceses$anos)
Desc(intwodioceses$anos ~ intwodioceses$ordinal)
Desc(intwodioceses$anos ~ intwodioceses$country)
Desc(intwodioceses$anos ~ intwodioceses$diocese_name)

# exportamos como geojson 
intwodioceses_sf <- st_as_sf(intwodioceses,
                     coords = c("longitude", "latitude"),
                     agr = "constant")
st_write(intwodioceses_sf, "intwodioceses_sf.geojson", append = FALSE)

# podemos exportar como geojson lo de italia
italy <- intwodioceses %>% filter(country == 'Italy')
italy_sf <- st_as_sf(italy,
                     coords = c("longitude", "latitude"),
                     agr = "constant")
st_write(italy_sf, "italy_sf.geojson", append = FALSE)


#######################################
# otra vez series temporales  por paises
#######################################

sql <- getSQL("../sql/cnf/series_temporales_cnf_op.sql")
op_series_cnf <- dbGetQuery(con, sql)
op_series_cnf$country <- factor(op_series_cnf$country)

op_series_cnf_agg <- op_series_cnf %>%
        group_by(serie) %>%
        summarise(total = sum(totalobispos))

p <-ggplot(op_series_cnf_agg, aes(x=serie, y= total)) +
    geom_line(size = 1.6, color = "#a12828") +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'series_op_general.png',
       path = dir_cnf)

# Italia 
p <- op_series_cnf %>%
    filter(country == "Italy") %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= totalobispos)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'series_italy.png',
       path = dir_cnf)

# España  
p <- op_series_cnf %>%
    filter(country == "Spain") %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= totalobispos)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() +
    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'spain_series.png',
       path = dir_cnf)



# otra versión de lo mismo pero con wrap 
op_series_cnf %>%
    filter(country %in% c("Spain", "France", "Italy", "Germany")) %>%
    na_if(0) %>%
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

op_series_cnf %>%
    filter(country == "Portugal") %>%
    na_if(0) %>%
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


op_series_cnf %>%
    filter(country == "Ireland") %>%
    na_if(0) %>%
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

op_series_cnf %>%
    filter(country %in% c("Ireland", "Great Britain",
                          "Norway", "Denmark", "Sweden")) %>%
    na_if(0) %>%
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




#######################################
# otra vez series temporales  por diócesis 
#######################################
sql <- getSQL("../sql/cnf/series_temporales_cnf_op_diocesis.sql")
op_series_cnf <- dbGetQuery(con, sql)
op_series_cnf$country <- factor(op_series_cnf$country)

op_series_cnf_agg <- op_series_cnf %>%
        group_by(serie) %>%
        summarise(total = sum(totalobispos))


#######################################
# para ver en un mapa en qué diócesis están
# y qen qué diócesis no están 
#######################################
sql <- getSQL("../sql/cnf/ops_bool_perdiocesis.sql")
rs = dbSendQuery(con, sql, params = list("Italy"))
ops_bool_perdiocesis <- dbFetch(rs)
dbClearResult(rs)

ops_bool_perdiocesis.italy_sf  <- st_as_sf(ops_bool_perdiocesis,
                                     coords = c("longitude", "latitude"),
                                     agr = "constant")
st_write(ops_bool_perdiocesis.italy_sf, "ops_bool_perdiocesis.italy.geojson", append = FALSE)
