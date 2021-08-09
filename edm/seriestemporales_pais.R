## imporante es que tenemos que volver a "reinicializar" la lista de países
## cada vez q colapsamos pq si no, se pierden algunos 

# Countries lists to collapse 
c_spain_global <- c("Spain", "Argentina", "Bolivia", "Chile", "Colombia",
               "Cuba", "Ecuador", "Dominican Republic", "Ecuador",
               "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panamá",
               "Paraguay", "Peru", "Philippines", "Puerto Rico",
               "Venezuela")

c_iberian_global  <- c("Spain", "Argentina", "Bolivia", "Chile", "Colombia",
               "Cuba", "Ecuador", "Dominican Republic", "Ecuador",
               "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panamá",
               "Paraguay", "Peru", "Philippines", "Puerto Rico",
               "Venezuela", "Portugal", "Brazil")

c_portugal_global  <- c("Portugal", "Brazil")

c_america <- c("Argentina", "Bolivia", "Chile", "Colombia",
               "Cuba", "Ecuador", "Dominican Republic", "Ecuador",
               "Guatemala", "Honduras", "Mexico", "Nicaragua", "Panamá",
               "Paraguay", "Peru", "Philippines", "Puerto Rico",
               "Venezuela")

c_balcans_with_turkey <- c("Bosnia and Herzegovina", "Croacia", "Slovenia",
               "Montenegro", "Greece", "Albania", "Cyprus", "Macedonia",
               "Serbia", "Turkey")

c_balcans_without_turkey <- c("Bosnia and Herzegovina", "Croacia", "Slovenia",
               "Montenegro", "Greece", "Albania", "Cyprus", "Macedonia",
               "Serbia")

c_ottomans <- c("Bosnia and Herzegovina", "Montenegro", "Greece",
                "Albania", "Macedonia", "Serbia", "Israel", "Palestina",
                "Egypt", "Jordania", "Lebanon", "Libia", "Syria", "Tunisia")

c_northern_europe <- c("Ireland", "Great Britain",
                       "Norway", "Denmark", "Sweden", "Finland")

c_germany <- c("Germany", "Austria", "Netherlands", "Belgium", "Switzerland")

c_eastern <- c("Poland", "Czech Republic", "Ucraine", "Russia",
               "Romania", "Lithuania", "Latvia", "Estonia", "Slovakia",
               "Hungary")

c_habsburgs <- c("Austria", "Hungary", "Czech Republic")

c_others  <- c("Iran", "Algeria", "Angola", "Cape Verde", "China",
               "Egypt", "India", "Israel", "Japan", "Jordania",
               "Lebanon", "Libia", "Malaysia", "Malta", "Palestina",
               "São Tomé and Príncipe", "Syria", "Tunisia", "Turkey")


# Spain global 
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 spainglobal = c_spain_global)

p <- op_series_edm %>%
    filter(countrycollapsed == "spainglobal") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_spainglobal.png',
       path = dir_edm)

# Iberian global
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 iberianglobal = c_iberian_global)

p <- op_series_edm %>%
    filter(countrycollapsed == "iberianglobal") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_iberianglobal.png',
       path = dir_edm)

# Portugal_global
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 portugalglobal = c_portugal_global)

p <- op_series_edm %>%
    filter(countrycollapsed == "portugalglobal") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_portugalglobal.png',
       path = dir_edm)

# America 
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 america = c_america)

p <- op_series_edm %>%
    filter(countrycollapsed == "america") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_america.png',
       path = dir_edm)

# comparando américa con España 
p <- op_series_edm %>%
    filter(countrycollapsed %in% c("america", "Spain")) %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total, color=countrycollapsed)) +
    geom_line(size = 1.3) +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_america_vs_spain.png',
       path = dir_edm)

# Balcnas con turquía
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 balcans_with_turkey = c_balcans_with_turkey)

p <- op_series_edm %>%
    filter(countrycollapsed == "balcans_with_turkey") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_balcans_with_turkey.png',
       path = dir_edm)

# Balcnas sin turquía
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 balcans_without_turkey = c_balcans_without_turkey)

p <- op_series_edm %>%
    filter(countrycollapsed == "balcans_without_turkey") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_balcans_without_turkey.png',
       path = dir_edm)


# Balcnas por países sin Turquía 
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed %in% c_balcans_without_turkey) %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~countrycollapsed) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_balcans_per_country.png',
       path = dir_edm)

# Ottomans
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 ottomans = c_ottomans)

p <- op_series_edm %>%
    filter(countrycollapsed == "ottomans") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_ottomans.png',
       path = dir_edm)

# Ottomans per country 
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed %in% c_ottomans) %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~countrycollapsed) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_ottomans_per_country.png',
       path = dir_edm)

# eastern
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 eastern = c_eastern)

p <- op_series_edm %>%
    filter(countrycollapsed == "eastern") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_eastern.png',
       path = dir_edm)

# eastern por países 
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed %in% c_eastern) %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~countrycollapsed)
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_eastern_per_countries.png',
       path = dir_edm)

# habsburgs
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 habsburgs = c_habsburgs)

p <- op_series_edm %>%
    filter(countrycollapsed == "habsburgs") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_habsburgs.png',
       path = dir_edm)

# otros
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 others = c_others)

p <- op_series_edm %>%
    filter(countrycollapsed == "others") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_habsburgs.png',
       path = dir_edm)

# germany 
op_series_edm$countrycollapsed <- op_series_edm$country
op_series_edm$countrycollapsed <-
    fct_collapse(op_series_edm$countrycollapsed,
                 germany = c_germany)

p <- op_series_edm %>%
    filter(countrycollapsed == "germany") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
        labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_germany.png',
       path = dir_edm)

# germany 
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed %in% c_germany) %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~countrycollapsed) +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_germany_countries.png',
       path = dir_edm)

# france
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed == "France") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa()
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_france_countries.png',
       path = dir_edm)

# irland
op_series_edm$countrycollapsed <- op_series_edm$country

p <- op_series_edm %>%
    filter(countrycollapsed == "Irland") %>%
    group_by(serie, countrycollapsed) %>%
    summarise(total = sum(totalobispos))%>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= total)) +
    geom_line(size = 1.3, color = "#a12828") +
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa()
#    scale_y_continuous(limits=c(0, 25))

ggsave(p, filename= 'series_irland_countries.png',
       path = dir_edm)
