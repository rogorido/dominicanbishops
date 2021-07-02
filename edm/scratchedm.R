j <- intwodioceses %>%
    group_by(n) %>% count_pct()

Desc(factor(j$n))


op_series_edm %>%
    na_if(0) %>%
    ggplot(., aes(x=serie, y= totalobispos)) +
    geom_line(size = 1.3, color = "#a12828") +
    facet_wrap(~country) + 
    labs(x = element_blank(),
         y = "# of bishops") +
    theme_sosa() 


