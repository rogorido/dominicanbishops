# algunos cálculos sobre rentas

library(RPostgreSQL)
library(DescTools)
library(tidyverse)

con <- dbConnect(drv= "PostgreSQL", dbname = "dominicos", user="igor", host="localhost")

sql <- "REFRESH MATERIALIZED VIEW dioceses_global_fl_estimado;"
dbSendQuery(con, sql)

sql <- "SELECT * FROM bishops.dioceses_global_fl_estimado;"
dioc <- dbGetQuery(con, sql)
dioc <- select(dioc, -coord)
dioc$pais <- factor(dioc$pais)

### General

# qué porcentaje estimamos? 
Desc(dioc$mesa_estimada_bool)
Desc(dioc$tasa_estimada_bool)

# en general: 
Desc(dioc$mesa_estimado)
# por países cómo va? 
Desc(dioc$mesa_estimado ~ dioc$pais)

# la santa sede
Desc(dioc$essantasede)
Desc(dioc$mesa_estimado ~ dioc$essantasede)
Desc(dioc$parishes ~ dioc$essantasede)

dioc %>% group_by(essantasede) %>%
    summarise(totalparroquias = sum(parishes, na.rm = TRUE))

dioc %>% group_by(essantasede) %>%
    summarise(totalmesaestimado = sum(mesa_estimado, na.rm = TRUE))

### Relación entre rentas y número de parroquias
dioc.lm.parr <- lm(mesa_estimado ~ parishes, data = dioc)

# los paises no parecen ser signifitivos
# esto se hace así?
# dioc.lm.parr <- lm(mesa_estimado ~ parishes + pais, data = dioc)

ggplot(dioc, aes(x=parishes, y=mesa_estimado)) + geom_point()

# es la santa sede signifitivo?
# sí parece 
t.test(dioc$mesa_estimado ~ dioc$essantasede)

# es el país signitifivo?
# algunos parece que salen
dioc.pais.aov <- aov(dioc$mesa_estimado ~ dioc$pais)
summary(dioc.pais.aov)
TukeyHSD(dioc.pais.aov)

# pero mejor compararlo con uno estándar que sería Italia
DunnettTest(dioc$mesa_estimado ~ dioc$pais, control="Italy")

# si quitamos italia, hay difreencias entre ellas?
# ahora miso no parece que haya mucho...
dioc.sinitalia <- filter(dioc, pais != "Italy")

dioc.sinitalia.pais.aov <- aov(dioc.sinitalia$mesa_estimado ~ dioc.sinitalia$pais)
summary(dioc.sinitalia.pais.aov)

TukeyHSD(dioc.sinitalia.pais.aov)

