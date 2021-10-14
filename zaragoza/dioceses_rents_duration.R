
# calculamos la rlc entre duración y la tasa media...

sql <- getSQL("../sql/edm/dioceses_rents_duration.sql")
rentsduration_ops <- dbGetQuery(con, sql)

plot(rentsduration_ops$duracion ~ rentsduration_ops$tasa_media)

rentsduration_ops.lm <- lm(duracion ~ tasa_media, data = rentsduration_ops)
summary(rentsduration_ops.lm)

# con logarítima?
rentsduration_ops$duracion_log <- log(rentsduration_ops$duracion)
plot(rentsduration_ops$duracion_log ~ rentsduration_ops$tasa_media)

rentsduration_ops.lmlog <- lm(duracion_log ~ tasa_media, data = rentsduration_ops)
summary(rentsduration_ops.lmlog)
plot(rentsduration_ops.lmlog)
