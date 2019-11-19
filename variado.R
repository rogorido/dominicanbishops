library(RPostgreSQL)
library(DescTools)
library(tidyverse)
library(vcd)
library(vcdExtra)
library(gmodels)

con <- dbConnect(drv= "PostgreSQL",
                 dbname = "dominicos",
                 user="igor", host="localhost")

setwd("/home/igor/geschichte/artikel/obisposdominicos/analisis/dash/")
source("./functions.R")

# vamos a necesitar estas temp views en algunos análisis 
ejecutar <- getSQL("./sqls/listas_paises.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

###############################################
# chi2 entre santa sede y frailes
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_santasede.sql")
santasede <- dbGetQuery(con, datossql)

santasede.t <- table(santasede$tiporeligioso, santasede$essantasede)
dimnames(santasede.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("nosede", "sede"))

CrossTable(santasede.t)

# es claramente muy significativo...
chisq.test(santasede.t)
Desc(santasede.t)

mosaicplot(santasede.t, shade = T)
fourfold(santasede.t)

###############################################
# chi2 entre bizancio y frailes
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_bizancio.sql")
bizancio <- dbGetQuery(con, datossql)

bizancio.t <- table(bizancio$tiporeligioso, bizancio$esbizancio)
dimnames(bizancio.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("bizancio", "no-bizancio"))

CrossTable(bizancio.t)

# es claramente muy significativo...
chisq.test(bizancio.t)
Desc(bizancio.t)

mosaicplot(bizancio.t, shade = T)
fourfold(bizancio.t)

###############################################
# chi2 entre américa y frailes
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_america.sql")
america <- dbGetQuery(con, datossql)

america.t <- table(america$tiporeligioso, america$esamerica)
dimnames(america.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("america", "no-america"))

CrossTable(america.t)

# es claramente muy significativo...
chisq.test(america.t)
Desc(america.t)

mosaicplot(america.t, shade = T)
fourfold(america.t)

###############################################
# chi2 entre ireland y frailes
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_ireland.sql")
ireland <- dbGetQuery(con, datossql)

ireland.t <- table(ireland$tiporeligioso, ireland$esireland)
dimnames(ireland.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("ireland", "no-ireland"))

CrossTable(ireland.t)

# es claramente muy significativo...
chisq.test(ireland.t)
Desc(ireland.t)

mosaicplot(ireland.t, shade = T)
fourfold(ireland.t)

###############################################
# chi2 entre spain y frailes: frente a todo el mundo 
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_spain.sql")
spain <- dbGetQuery(con, datossql)

spain.t <- table(spain$tiporeligioso, spain$esspain)
dimnames(spain.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("spain", "no-spain"))

CrossTable(spain.t)

# es claramente muy significativo...
chisq.test(spain.t)
Desc(spain.t)

mosaicplot(spain.t, shade = T)
fourfold(spain.t)

###############################################
# chi2 entre spain y frailes: frente a europa occidental. 
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_spain2.sql")
spainvseurope <- dbGetQuery(con, datossql)

spainvseurope.t <- table(spainvseurope$tiporeligioso, spainvseurope$esspain)
dimnames(spainvseurope.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("no-spain", "spain"))

CrossTable(spainvseurope.t)

# es claramente muy significativo...
chisq.test(spainvseurope.t)
Desc(spainvseurope.t)

mosaicplot(spainvseurope.t, shade = T)
fourfold(spainvseurope.t)


###############################################
# chi2 entre portugal y frailes: frente a europa occidental. 
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_portugal.sql")
portugalvseurope <- dbGetQuery(con, datossql)

portugalvseurope.t <- table(portugalvseurope$tiporeligioso, portugalvseurope$esportugal)
dimnames(portugalvseurope.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("no-portugal", "portugal"))

CrossTable(portugalvseurope.t)

# es claramente muy significativo...
chisq.test(portugalvseurope.t)
Desc(portugalvseurope.t)

mosaicplot(portugalvseurope.t, shade = T)
fourfold(portugalvseurope.t)

###############################################
# chi2 entre france y frailes: frente a europa occidental. 
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_france.sql")
francevseurope <- dbGetQuery(con, datossql)

francevseurope.t <- table(francevseurope$tiporeligioso, francevseurope$esfrance)
dimnames(francevseurope.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("france", "no-france"))

CrossTable(francevseurope.t)

# es claramente muy significativo...
chisq.test(francevseurope.t)
Desc(francevseurope.t)

mosaicplot(francevseurope.t, shade = T)
fourfold(francevseurope.t)

###############################################
# chi2 entre italy y frailes: frente a europa occidental. 
###############################################
# ver notas en el texto de análisis 
datossql <- getSQL("./sqls/chisq2_italy.sql")
italyvseurope <- dbGetQuery(con, datossql)

italyvseurope.t <- table(italyvseurope$tiporeligioso, italyvseurope$esitaly)
dimnames(italyvseurope.t) <- list(persona = c("fraile", "secular"),
                              obispado = c("italy", "no-italy"))

CrossTable(italyvseurope.t)

# es claramente muy significativo...
chisq.test(italyvseurope.t)
Desc(italyvseurope.t)

mosaicplot(italyvseurope.t, shade = T)
fourfold(italyvseurope.t)

