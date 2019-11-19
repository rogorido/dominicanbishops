message("Cargando los datos de OPs")

#######################################
# Órdenes en general
#######################################
datossql <- getSQL("./sqls/ordenes_listageneral.sql")
ordenes_listageneral <- dbGetQuery(con, datossql)

ordenes_listageneral$type_order <- factor(ordenes_listageneral$type_order)

# esto realmente es una lista que creamos para usarla en general
# en comoboxes para seleccionar dióceses
ordenes_listageneral$order_acronym <- factor(ordenes_listageneral$order_acronym)
orrg_lista <- as.list(levels(ordenes_listageneral$order_acronym))

#######################################
# Consulta general 
#######################################
# lo hacemos solo para la edm
ejecutar <- getSQL("./sqls/consulta_general_ss_sa_edm.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

orrg_consultageneral_edm <- dbGetQuery(con, "SELECT * FROM tv_consultageneral_ss_sa_edm")
orrg_consultageneral_edm$religious_order <- factor(orrg_consultageneral_edm$religious_order)
orrg_consultageneral_edm$diocese_name <- factor(orrg_consultageneral_edm$diocese_name)
orrg_consultageneral_edm$country <- factor(orrg_consultageneral_edm$country)

#######################################
# Presencia en diócesis
# primero emd y luego edm 
#######################################
datossql <- getSQL("./sqls/presencia_orrg_dioc_emd.sql")
orrg_presencia_emd <- dbGetQuery(con, datossql)

# factorizamos cosas 
orrg_presencia_emd$religious_order <- factor(orrg_presencia_emd$religious_order)
orrg_presencia_emd_lista <- as.list(levels(orrg_presencia_emd$religious_order))
orrg_presencia_emd$diocese_name <- factor(orrg_presencia_emd$diocese_name)
orrg_presencia_emd$country <- factor(orrg_presencia_emd$country)

## ahora edm
datossql <- getSQL("./sqls/presencia_orrg_dioc_edm.sql")
orrg_presencia_edm <- dbGetQuery(con, datossql)

# factorizamos cosas 
orrg_presencia_edm$religious_order <- factor(orrg_presencia_edm$religious_order)
orrg_presencia_edm_lista <- as.list(levels(orrg_presencia_edm$religious_order))
orrg_presencia_edm$diocese_name <- factor(orrg_presencia_edm$diocese_name)
orrg_presencia_edm$country <- factor(orrg_presencia_edm$country)



#######################################
# Periplos por diócesis 
#######################################

ejecutar <- getSQL("./sqls/ops_periplos1.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

datossql <- getSQL("./sqls/ops_periplos2.sql")
ops_periplos_emd <- dbGetQuery(con, datossql)

# edm 
ejecutar <- getSQL("./sqls/ops_periplos3.sql")
dbSendQuery(con, ejecutar) # creamos una temp view
datossql <- getSQL("./sqls/ops_periplos4.sql")
ops_periplos_edm <- dbGetQuery(con, datossql)

#######################################
# Periplos por diócesis (clusters) 
#######################################

periplos_clusters_edm <- dbGetQuery(con, "SELECT * from related_dioceses_edm_ss_sa;")
# lo mismo pero solo con los OPs.
periplos_clusters_edm_ops <- dbGetQuery(con, "SELECT * from related_dioceses_edm_ops;")


#######################################
# Series temporales 
#######################################
# creamos un df general donde está separados los OFMs
# y uno unido donde están unidos. 

# primero edm 
datossql <- getSQL("./sqls/ordenes_seriestemporales_edm.sql")
orrg_series_edm_general <- dbGetQuery(con, datossql)

orrg_series_edm_general$order_acronym <- factor(orrg_series_edm_general$order_acronym)
orrg_series_edm_general_lista <- as.list(levels(orrg_series_edm_general$order_acronym))

orrg_series_edm_unido <- orrg_series_edm_general
orrg_series_edm_unido$order_acronym <-
    fct_collapse(orrg_series_edm_unido$order_acronym,
                 "O.F.M." = c("O.F.M.", "O.F.M. Conv."))
orrg_series_edm_unido_lista <- as.list(levels(orrg_series_edm_unido$order_acronym))

# luego emd
datossql <- getSQL("./sqls/ordenes_seriestemporales_emd.sql")
orrg_series_emd_general <- dbGetQuery(con, datossql)

orrg_series_emd_general$order_acronym <- factor(orrg_series_emd_general$order_acronym)
orrg_series_emd_general_lista <- as.list(levels(orrg_series_emd_general$order_acronym))

orrg_series_emd_unido <- orrg_series_emd_general
orrg_series_emd_unido$order_acronym <-
    fct_collapse(orrg_series_emd_unido$order_acronym,
                 "O.F.M." = c("O.F.M.", "O.F.M. Conv."))
orrg_series_emd_unido_lista <- as.list(levels(orrg_series_emd_unido$order_acronym))

#######################################
# Motivos fin 
#######################################
# lo hacemos solo para la edm
ejecutar <- getSQL("./sqls/motivosfin_edm.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

orrg_motivosfin_edm <- dbGetQuery(con, "SELECT * FROM tv_motivos_fin_ss_edm")

orrg_motivosfin_edm$rel_order <- factor(orrg_motivosfin_edm$rel_order)
orrg_motivosfin_edm_lista <- as.list(levels(orrg_motivosfin_edm$rel_order))

#######################################
# Monasterio y obispado 
#######################################

datossql <- getSQL("./sqls/ops_monasterio_obispado_edm.sql")
ops_mon_obispado_edm <- dbGetQuery(con, datossql)

ops_mon_obispado_edm$place <- factor(ops_mon_obispado_edm$place)
ops_mon_obispado_edm$country <- factor(ops_mon_obispado_edm$country)


#######################################
# Bizancio 
#######################################
ejecutar <- getSQL("./sqls/ops_bizancio.sql")
dbSendQuery(con, ejecutar) # creamos una temp view

ops_bizancio <- dbGetQuery(con, "SELECT * FROM tv_ops_bizancio")
ops_bizancio$country <- factor(ops_bizancio$country)

# y ahora las décadas
datossql <- getSQL("./sqls/ops_bizancio_decadas.sql")
ops_bizancio_decadas <- dbGetQuery(con, datossql)

#######################################
# Odds ops/ofms 
#######################################

datossql <- getSQL("./sqls/odds_ops_ofms_edm.sql")
odds_ops_ofms_edm <- dbGetQuery(con, datossql)

# eel asunto es un poco liado, pero bstt lo q hacemos es pasar a wide
# luego sumamos los dos valores, construimos el porcentaje y el odds.
# Obviamente el problema es si este odds que se refiere solo a estas dos
# órdenes tiene sentido...
odds_ops_ofms_edm <-
    odds_ops_ofms_edm %>%
    spread(order_acronym, total) %>%
    mutate(totalambos = O.F.M. + O.P.,
           porc_op = round(O.P. / totalambos, 2),
           odds_op = round(porc_op / (1 - porc_op), 2))
