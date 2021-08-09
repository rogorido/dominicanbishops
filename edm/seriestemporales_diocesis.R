## imporante es que tenemos que volver a "reinicializar" la lista de países
## cada vez q colapsamos pq si no, se pierden algunos

sql_series <- getSQL("../sql/edm/series_temporales_edm_per_diocesis.sql")
sql_anosdiocesis <- getSQL("../sql/edm/anos_per_diocesis.sql")

#################################################
# Pòrtugal
#################################################

rs = dbSendQuery(con, sql_series, params = list('Portugal'))
series_per_diocesis <- dbFetch(rs)
dbClearResult(rs)

p <- series_per_diocesis %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~diocese_name) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_germany_countries.png',
       path = dir_edm)

# y ahora sumamos años
anos_per_diocesis = dbGetQuery(con, sql_anosdiocesis)

p <- anos_per_diocesis %>%
    filter(pais == 'Portugal') %>%
    ggplot(., aes(x=reorder(diocesis, duracion), y= duracion)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    coord_flip() + 
    labs(x = element_blank(),
         y = "Total years in the bishopric") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))


#################################################
# Italia 
#################################################

rs = dbSendQuery(con, sql_series, params = list('Italy'))
series_per_diocesis <- dbFetch(rs)
dbClearResult(rs)

# escogemos las 20 q tienen más años que lo he sacado del
# anos_per_diocesis q está aquí abajo 

italy_dioceses_most_years <- c("Minori", "Alghero","Satriano","Tortona",
                               "Chiogga","Benevento",
                               "Borgo,San,Sepulcro","Saluzzo","Lipari",
                               "Cagliari","Génova","Marsico,Nuovo",
                               "Gravina","Agrigento","Giovinazzo",
                               "Concordia","Noli","Como","Modena",
                               "Trani")

p <- series_per_diocesis %>%
    filter(diocese_name %in% italy_dioceses_most_years) %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~diocese_name) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_germany_countries.png',
       path = dir_edm)

# y ahora sumamos años
anos_per_diocesis = dbGetQuery(con, sql_anosdiocesis)

p <- anos_per_diocesis %>%
    filter(pais == 'Italy', diocesis %in% italy_dioceses_most_years) %>%
    ggplot(., aes(x=reorder(diocesis, duracion), y= duracion)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    coord_flip() + 
    labs(x = element_blank(),
         y = "Total years in the bishopric") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

