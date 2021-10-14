
# analizamos aquí la bajada de finales del xvi

sql <- getSQL("../sql/edm/bajadadelxvi.sql")
bajada  <- dbGetQuery(con, sql)

ggplot(bajada, aes(x=r_from, y= total)) +
    geom_bar(stat = "identity")

Desc(bajada$total)

quantile(bajada$total)
