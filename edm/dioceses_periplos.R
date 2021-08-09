
# analizamos así un poc a la brava cuántas veces una diócesis
# es primera dióces y cuanta 2º o 3º oetc. pero jutnamos todo lo que
# es >1 como "segundas"

sql <- getSQL("../sql/edm/dioceses_periplos_agregados.sql")
dioc_periplos_agregados <- dbGetQuery(con, sql)

# lo ponemos bien
dioc_periplos_agregados <- dioc_periplos_agregados %>%
    pivot_wider(names_from = periplos, values_from = total)

# miramos las q nunca son 2ª o 3ª
dioc_periplos_agregados %>% filter(is.na(segundas)) %>%
    arrange(-primeras)

# miramos las q nunca son 1ª y me salen unas 43
dioc_periplos_agregados %>% filter(is.na(primeras))


##############################################################
# calcumos si los q pasan a una 2ª diocesis están menos
# q los q solo se quedan en una
##############################################################
sql <- getSQL("../sql/edm/morethan2dioceses.sql")
morethan2dioceses <- dbGetQuery(con, sql)
morethan2dioceses$moredioceses <- factor(morethan2dioceses$moredioceses)

Desc(morethan2dioceses$moredioceses ~ morethan2dioceses$anos)

# tb da muy significativo
t.test(morethan2dioceses$anos ~ morethan2dioceses$moredioceses)
