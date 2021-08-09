
sql <- getSQL("../sql/edm/nombramientos_decadas.sql")
decadas <- dbGetQuery(con, sql)

p <- ggplot(decadas, aes(x=decada, y = total)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

p2 <- ggplot(decadas, aes(x=decada, y = total)) +
    geom_line(color = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'nombramientos_decadas.png',
       path = dir_edm)


#### por países
# rtt la abfrage no está bien hecha. No consigo lo de q me ponga 0s
# en los casos en los q no hay nombramientos. Com por ahora lo uso para
# italia, etc. en ppo no hay mucha dificultad.
sql <- getSQL("../sql/edm/nombramientos_decadas_paises.sql")
decadas_paises <- dbGetQuery(con, sql)

# en italia

p <- decadas_paises %>% filter(country == 'Italy') %>%
    ggplot(., aes(x=r_from, y = total)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'nombramientos_decadas_italia.png',
       path = dir_edm)

# en span_global
p <- decadas_paises %>% filter(country %in% c_spain_global) %>%
    ggplot(., aes(x=r_from, y = total)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'nombramientos_decadas_spain_global.png',
       path = dir_edm)

# en américa
p <- decadas_paises %>% filter(country %in% c_america) %>%
    ggplot(., aes(x=r_from, y = total)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'nombramientos_decadas_america.png',
       path = dir_edm)

# enfrance
p <- decadas_paises %>% filter(country == 'France') %>%
    ggplot(., aes(x=r_from, y = total)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = "Decades",
         y = "# of bishops") +
    theme_sosa()

ggsave(p, filename= 'nombramientos_decadas_france.png',
       path = dir_edm)
