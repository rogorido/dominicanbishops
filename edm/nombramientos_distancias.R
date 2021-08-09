# para analizar un pocolas distancias en los nombramientos

sql <- getSQL("../sql/edm/nombramientos_distancias.sql")
nom_distancias <- dbGetQuery(con, sql)

Desc(nom_distancias$anyos)
quantile(nom_distancias$anyos)

# quantile de 25%
q2 <- quantile(nom_distancias$anyos)[2]

# analizamos un poco mejor el 25% este
nom_distancias_q1 <-
    nom_distancias %>% filter(anyos < q1) %>%
    group_by(diocese_name) %>%
    summarise(total = n())%>%
    arrange(-total)

# una media tosca por países a lo bruto
nom_distancias_paises <-
    nom_distancias %>%
    group_by(country) %>%
    summarise(media = mean(anyos)) %>%
    arrange(-media)

# si miramos pej por américa en total
nom_distancias_america1 <-
    nom_distancias %>%
    filter(country %in% c_america) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_america1$pais <- "America"

# como me sale un pcoo alto, lo miro desgajado por Países
nom_distancias_america2 <-
    nom_distancias %>%
    filter(country %in% c_america) %>%
    group_by(country) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos)) %>%
    arrange(-media)

# si miramos pej por francia
nom_distancias_france <-
    nom_distancias %>%
    filter(country == 'France') %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_france$pais <- "France"

# si miramos pej por italy
nom_distancias_italy <-
    nom_distancias %>%
    filter(country == 'Italy') %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_italy$pais <- "Italy"

# por alemania & Co.
nom_distancias_germany <-
    nom_distancias %>%
    filter(country %in% c_germany) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_germany$pais <- "Germany and Co."

# por españa
nom_distancias_spain <-
    nom_distancias %>%
    filter(country ==  "Spain") %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_spain$pais <- "Spain"

# por adriático (sin turquía)
nom_distancias_balcans_without_turkey <-
    nom_distancias %>%
    filter(country %in% c_balcans_without_turkey) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_balcans_without_turkey$pais <- "Balcans without Turkey"

# portugal (global)
nom_distancias_portugal_global <-
    nom_distancias %>%
    filter(country %in% c_portugal_global) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_portugal_global$pais <- "Portugal (global)"

# portugal
nom_distancias_portugal <-
    nom_distancias %>%
    filter(country ==  "Portugal") %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_portugal$pais <- "Portugal"

# spain (global)
nom_distancias_spain_global <-
    nom_distancias %>%
    filter(country %in% c_spain_global) %>%
    summarise(media = mean(anyos),
              sd = sd(anyos),
              vcoef = sd(anyos) / mean(anyos))
nom_distancias_spain_global$pais <- "Spain (global)"


nom_distancias_por_zonas <-
    rbind(nom_distancias_italy, nom_distancias_france,
          nom_distancias_america1, nom_distancias_germany,
          nom_distancias_spain, nom_distancias_spain_global,
          nom_distancias_balcans_without_turkey,
          nom_distancias_portugal_global, nom_distancias_portugal)

# reordenamos las columnas
# https://dplyr.tidyverse.org/reference/relocate.html
nom_distancias_por_zonas <-
    nom_distancias_por_zonas %>%
    relocate(pais)

p <-
    ggplot(nom_distancias_por_zonas, aes(x = pais, y = media)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = element_blank(),
         y = "Mean of years") +
    theme_sosa()

ggsave(p, filename= 'nom_distancias_por_zonas_medias.png',
       path = dir_edm)

p <-
    ggplot(nom_distancias_por_zonas, aes(x = pais, y = vcoef)) +
    geom_bar(stat = "identity", fill = "#a12828") +
    labs(x = element_blank(),
         y = "vcoef of years") +
    theme_sosa()

ggsave(p, filename= 'nom_distancias_por_zonas_vcoef.png',
       path = dir_edm)
