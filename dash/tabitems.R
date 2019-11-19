## IMPORTANTE: pensar si meter los tabpanel en otro fichero como
## tengo en lo de capprov.

# font-awesome: https://fontawesome.com/icons?d=gallery&m=free 

tabgeneral <-
    tabItem(tabName = "general",
            fluidRow(valueBox(total_diocesis, "Total diócesis", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos, "Total obispos", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos_edm, "Total obispos (edad Moderna)", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos_frailes, "Total obispos frailes", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos_frailes_edm, "Total obispos frailes (edad moderna)",
                              width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos_op, "Total obispos (OPs)", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon")),
                     valueBox(total_obispos_op_edm, "Total obispos OPs (edad Moderna)", width = 3,
                              icon = icon("thumbs-up", lib = "glyphicon"))
                     ))

tabdiocgeneral <- 
    tabItem(tabName = "d_general",
            tabsetPanel(id = "tabset1",# height = "100", width="100",
                        tabPanel("Con seculares", icon = icon("th"),
                                 h4("Periodo 1200-1800"),
                                 DT::dataTableOutput("bis_per_dioc")),
                        tabPanel("Con seculares (mapa)",
                                 icon = icon("globe-africa", lib = "font-awesome"),
                                 h4("Periodo 1200-1800"),
                                 leafletOutput("bis_per_dioc_mapa", height = 1000)),
                        tabPanel("Seculares (por siglos)", icon = icon("th"),
                                 h4("Periodo 1200-1800"),
                                 DT::dataTableOutput("bis_per_dioc_por_siglos")),
                        tabPanel("Santa sede",
                                 icon = icon("globe-africa", lib = "font-awesome"),
                                 leafletOutput("dioc_santasede", height=1000))
                        ))

tabindividual <- 
    tabItem(tabName = "d_individual",
            fluidRow(column(4,
                            pickerInput("seleccionar_diocs",
                                        label = 'Seleccionar diócesis',
                                        choices = dioceses_lista_ids,
                                        choicesOpt = list(content = dioceses_lista),
                                        multiple = FALSE, #inline = TRUE,                       
                                        options = list(
                                            `actions-box` = TRUE,`live-search` = TRUE),
                                        selected = '497')), # selecionamos Aarhus por nada especial
                     column(8,
                            h3(htmlOutput("nombrediocesis_url")))),
            hr(),
            leafletOutput("ind_dioc_mapa"),
            hr(),
            fluidRow(valueBoxOutput("ind_total_obispos_emd"),
                     valueBoxOutput("ind_total_obispos_edm")),
            hr(),
            fluidRow(column(3,
                            DT::dataTableOutput("ind_tipos_obispos_emd")),
                     column(3, 
                            DT::dataTableOutput("ind_tipos_obispos_edm"))))

tabsecnosec <- 
    tabItem(tabName = "d_secnosec",
            tabsetPanel(id = "tabsetsecnosec",# height = "100", width="100",
                        tabPanel("Periodo 1200-1800",
                                 h3("Periodo 1200-1800 (porcentajes)"),
                                 h4("Los odds se refieren a sec. vs no-seculares."),
                                 DT::dataTableOutput("dioc_sec_vs_nosec_emd")),
                        tabPanel("Periodo 1200-1800 (mapa)",
                                 h3("Periodo 1200-1800 (porcentajes de órdenes)"),
                                 leafletOutput("dioc_sec_vs_nosec_emd_mapa", height = 1000)),
                        tabPanel("Edad moderna",
                                 h4("Los odds se refieren a sec. vs no-seculares."),
                                 DT::dataTableOutput("dioc_sec_vs_nosec_edm")),
                        tabPanel("Edad moderna (mapa)",
                                 h3("Periodo edm (porcentajes de órdenes)"),
                                 leafletOutput("dioc_sec_vs_nosec_edm_mapa", height = 1000))
                        ))

tabdioc_orrg <- 
    tabItem(tabName = "d_orrg",
            tabsetPanel(id = "tabset2",# height = "100", width="100",
                        tabPanel("Diócesis sin órdenes",
                                 h4("Periodo 1200-1800"),
                                 leafletOutput("dioc_sin_orrg_emd")),
                        tabPanel("ORRGs sin diócesis (1200-1800)",
                                 h4("Diócesis en las que no hay una orden determinada"),
                                 h4("Con top10 de órdenes religiosas de la Edad Moderna"),
                                 hr(),
                                 h4("Seleccionar orden"),
                                 selectInput("seleccionar_orrg_sin_dioc_emd",
                                             label = 'Seleccionar orden',
                                             choices = orrg_sin_dioc_emd_lista,
                                             selected = 'O.P.'),
                                 fluidRow(column(6,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("orrg_sin_dioc_emd")),
                                          column(6,
                                                 h4("Datos agregados"),
                                                 DT::dataTableOutput("orrg_sin_dioc_emd_agregados"))),
                                 hr(),
                                 leafletOutput("orrg_sin_dioc_emd_mapa")),
                        tabPanel("ORRGs sin diócesis (edm)",
                                 h4("Diócesis en las que no hay una orden determinada"),
                                 h4("Con top10 de órdenes religiosas de la Edad Moderna"),
                                 hr(),
                                 h4("Seleccionar orden"),
                                 selectInput("seleccionar_orrg_sin_dioc_edm",
                                             label = 'Seleccionar orden',
                                             choices = orrg_sin_dioc_edm_lista,
                                             selected = 'O.P.'),
                                 fluidRow(column(6,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("orrg_sin_dioc_edm")),
                                          column(6,
                                                 h4("Datos agregados"),
                                                 DT::dataTableOutput("orrg_sin_dioc_edm_agregados"))),
                                 hr(),
                                 leafletOutput("orrg_sin_dioc_edm_mapa")
                                 )))

tabpresencia_orrg_diocs <- 
    tabItem(tabName = "o_diocesis",
            fluidRow(
                column(4, 
                       selectInput("seleccionar_orrg_presencia",
                                   label = 'Seleccionar orden',
                                   choices = orrg_lista,
                                   selected = 'O.P.')),
                column(4,
                       prettyRadioButtons(
                           inputId = "seleccionar_tipo_diocesis",
                           label = "Tipo de diócesis:", 
                           choices = c("Todos", "Bizancio", "Extraeuropeas"),
                           inline = TRUE, 
                           status = "danger",
                           fill = TRUE
                       ))),
            tabsetPanel(id = "tabset_presencia_orrg_diocs",# height = "100", width="100",
                        tabPanel("Presencia (1200-1800)",
                                 h4("Número de obispos en diócesis"),
                                 fluidRow(
                                     column(6,
                                            h4("Datos generales"),
                                            DT::dataTableOutput("orrg_presencia_emd")),
                                     column(6,
                                            h4("Datos agregados"),
                                            DT::dataTableOutput("orrg_presencia_emd_agregados"))),
                                 hr(),
                                 leafletOutput("orrg_presencia_emd_mapa")),
                        tabPanel("Presencia (EDM)",
                                 fluidRow(column(6,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("orrg_presencia_edm")),
                                          column(6,
                                                 h4("Datos agregados"),
                                                 DT::dataTableOutput("orrg_presencia_edm_agregados"))),
                                 hr(),
                                 leafletOutput("orrg_presencia_edm_mapa")
                                 ),
                        tabPanel("OPs vs OFMs",
                                 DT::dataTableOutput("odds_ops_ofms_edm"),
                                 hr(),
                                 leafletOutput("odds_ops_ofms_edm_mapa")
                                 ))
            )

tabconsultageneral <- 
    tabItem(tabName = "o_consultageneral",
            h4("Se trata de edad moderna y solo frailes"),
            DT::dataTableOutput("orrg_consultageneral_edm"))

tab5 <- 
    tabItem(tabName = "d_papas",
            tabsetPanel(id = "tabset3",# height = "100", width="100",
                        tabPanel("Papas y nombramientos",
                                 fluidRow(column(2,
                                                 h4("Seleccionar papa"),
                                                 selectInput("seleccionar_dioc_papas",
                                                             label = 'Seleccionar papa',
                                                             choices = dioc_papas_lista,
                                                             selected = 'Alexander VI (1492-1503)')),
                                          column(5,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("dioc_papas"))),
                                 hr(),
                                 leafletOutput("dioc_papas_mapa")
                                 ),
                        tabPanel("Diócesis sin papas",
                                 fluidRow(column(2,
                                                 h4("Seleccionar papa"),
                                                 selectInput("seleccionar_dioc_sinpapas",
                                                             label = 'Seleccionar papa',
                                                             choices = dioc_sinpapas_congis_lista,
                                                             selected = 'Alexander VI (1492-1503)')),
                                          column(5,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("dioc_sinpapas"))),
                                 hr(),
                                 leafletOutput("dioc_sinpapas_mapa")
                                 )))

tab6 <- 
    tabItem(tabName = "d_rentas",
            tabsetPanel(id = "tabset4",# height = "100", width="100",
                        tabPanel("Rentas",
                                 DT::dataTableOutput("dioc_rentas"),
                                 hr(),
                                 leafletOutput("dioc_rentas_mapa")
                                 ),
                        tabPanel("Rentas para orrg",
                                 h4("Solo edad moderna y top10"),
                                 DT::dataTableOutput("rentas_por_obispo_orrg_top10")
                                 )))

tabduracion <- 
    tabItem(tabName = "d_duracion",
            tabsetPanel(id = "tabset5",
                        tabPanel("Duración",
                                 h3("Total (edad moderna y con seculares)"),
                                 fluidRow(column(3,
                                                 sliderInput("seleccionar_dioc_duraciones",
                                                             label = 'Seleccionar duración',
                                                             min = dioc_duraciones_min,
                                                             max = dioc_duraciones_max,
                                                             value = c(dioc_duraciones_min,
                                                                       dioc_duraciones_max))),
                                          column(5,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("dioc_duraciones"))),
                                 hr(),
                                 leafletOutput("dioc_duraciones_mapa")
                                 ),
                        tabPanel("Huecos",
                                 h4("añadir ")
                                 #,DT::dataTableOutput("rentas_por_obispo_orrg_top10")
                                 )))

tabperiplogeneral <- 
    tabItem(tabName = "d_periplo",
            tabsetPanel(id="tabset7",
                        tabPanel("General (1200-1800)",
                                 h4("Datos generales  1200-1800"),
                                 DT::dataTableOutput("periplos_emd_cs_sa_totales")),
                        tabPanel("General (edm)",
                                 h4("Datos generales edm"),
                                 DT::dataTableOutput("periplos_edm_cs_sa_totales")),
                        tabPanel("General (1200-1800): décadas",
                                 h4("Datos generales edm"),
                                 DT::dataTableOutput("periplos_emd_cs_sa_decadas")),
                        tabPanel("Pares de diócesis (1200-1800)",
                                 DT::dataTableOutput("periplospares_emd_cs_sa")),
                        tabPanel("Pares de diócesis (edm)",
                                 DT::dataTableOutput("periplospares_edm_cs_sa"))
                        )
            )

tab8 <- 
    tabItem(tabName = "d_fin",
            tabsetPanel(id = "tabset7",# height = "100", width="100",
                        tabPanel("Diócesis y fin", DT::dataTableOutput("dioc_con_fin")),
                        tabPanel("Diócesis y fin (mapa)",
                                 leafletOutput("dioc_con_fin_mapa"),
                                 hr(),
                                 fluidRow(box(title = "Años",
                                              sliderInput("slider_dioc_con_fin", "Años:", 1500, 1800,
                                                          value=c(1500, 1800)))))))

#######################################3
# OPs
#######################################3

tab_ordenes <- 
    tabItem(tabName = "o_ordenesgeneral",
            h4("Solo las órdenes religiosas que aparecen en mis datos."),
            dataTableOutput("ordenes_listageneral")
            )


tab_orrg_periplos <- 
    tabItem(tabName = "o_periplos",
            tabsetPanel(id="tabset9",
                        tabPanel("Frailes (1200-1800)",
                                 h4("Datos generales 1200-1800"),
                                 DT::dataTableOutput("periplos_emd_ss_sa_totales")),
                        tabPanel("Frailes (edm)",
                                 h4("Datos generales edm"),
                                 DT::dataTableOutput("periplos_edm_ss_sa_totales")),
                        tabPanel("Pares de diócesis (1200-1800)",
                                 DT::dataTableOutput("periplospares_emd_ss_sa")),
                        tabPanel("Pares de diócesis (edm)",
                                 DT::dataTableOutput("periplospares_edm_ss_sa")),
                        tabPanel("OPs (1200-1800)",
                                 DT::dataTableOutput("ops_periplos_emd")),
                        tabPanel("OPs (Edad Moderna)",
                                 DT::dataTableOutput("ops_periplos_edm")),
                        tabPanel("Clusters (frailes, edad moderna)",
                                 pickerInput("seleccionar_diocs_periplos_clusters",
                                             label = 'Seleccionar diócesis',
                                             choices = dioceses_lista_ids,
                                             choicesOpt = list(content = dioceses_lista),
                                             multiple = FALSE, #inline = TRUE,                       
                                             options = list(
                                                 `actions-box` = TRUE,`live-search` = TRUE),
                                             selected = '497'), # selecionamos Aarhus por nada especial
                                 DT::dataTableOutput("periplos_clusters_edm"),
                                 hr(),
                                 leafletOutput("periplos_clusters_edm_mapa")),
                        tabPanel("Clusters (OPs, edad moderna)",
                                 hr(),
                                 DT::dataTableOutput("periplos_clusters_edm_ops_totales"),
                                 hr(),
                                 pickerInput("seleccionar_diocs_periplos_clusters_ops",
                                             label = 'Seleccionar diócesis',
                                             choices = dioceses_lista_ids,
                                             choicesOpt = list(content = dioceses_lista),
                                             multiple = FALSE, #inline = TRUE,                       
                                             options = list(
                                                 `actions-box` = TRUE,`live-search` = TRUE),
                                             selected = '497'), # selecionamos Aarhus por nada especial
                                 DT::dataTableOutput("periplos_clusters_edm_ops"),
                                 hr(),
                                 leafletOutput("periplos_clusters_edm_ops_mapa"))
                        ))

tab_orrg_motivosfin <- 
    tabItem(tabName = "o_motivosfin",
            selectInput("seleccionar_orrg_motivosfin_edm",
                        label = 'Seleccionar orden',
                        choices = orrg_motivosfin_edm_lista,
                        selected = 'O.P.'),
            hr(),
            fluidRow(
                column(
                    dataTableOutput("orrg_motivosfin_edm"), width = 6)
            ))

tab_monasterioobispado <- 
    tabItem(tabName = "o_monasterioobispado",
            h4("Solo Edad moderna"),
            hr(),
            fluidRow(
                column(
                    dataTableOutput("ops_monasterioobispado_edm"), width = 6)),
            hr(),
            leafletOutput("ops_monasterioobispado_edm_mapa"))

tab_bizancio <- 
    tabItem(tabName = "o_bizancio",
            dataTableOutput("ops_bizancio"),
            hr(),
            h4("Nº total de obispos por décadas"),
            plotOutput("ops_bizancio_decadas"))


#######################################3
# Confesionalización
#######################################3

tab_cnf1 <- 
    tabItem(tabName = "c_ordenes",
            tabsetPanel(id="tabset11",
                        tabPanel("Órdenes", 
                                 fluidRow(column(2,
                                                 selectInput("seleccionar_orrg_confe",
                                                             label = 'Seleccionar orden',
                                                             choices = cnf_ordenes_lista,
                                                             selected = 'O.P.')),
                                          column(3,
                                                 h4("Datos generales"),
                                                 DT::dataTableOutput("cnf_ordenes"))),
                                 hr(),
                                 leafletOutput("cnf_ordenes_mapa"))))

tab_cnf2 <- 
    tabItem(tabName = "c_ordenes_anos",
            fluidRow(column(2,
                            selectInput("seleccionar_orrg_confe_sumaanos",
                                        label = 'Seleccionar orden',
                                        choices = cnf_ordenes_sumaanos_lista,
                                        selected = 'O.P.')),
                     column(3,
                            h4("Datos generales"),
                            DT::dataTableOutput("cnf_ordenes_sumaanos")),
                     column(3,
                            h4("Por países"),
                            DT::dataTableOutput("cnf_ordenes_sumaanos_paises"))
                     ),
            hr(),
            leafletOutput("cnf_ordenes_sumaanos_mapa"))

tab_seriestemporales <- 
    tabItem(tabName = "o_series",
            fluidRow(
                column(3, 
                       pickerInput("seleccionar_orrg_series",
                                   label = 'Seleccionar órdenes',
                                   choices = orrg_lista,
                                   multiple = TRUE,
                                   options = list(
                                       `actions-box` = TRUE, 
                                       size = 10,
                                       `selected-text-format` = "count > 7"),
                                   selected = 'O.P.')),
                column(3, 
                       pickerInput("seleccionar_orrg_series_paises",
                                   label = 'Seleccionar países',
                                   choices = lista_paises_todos,
                                   multiple = TRUE,
                                   options = list(
                                       `actions-box` = TRUE, 
                                       size = 10,
                                       `selected-text-format` = "count > 7",
                                       `live-search` = TRUE),
                                   selected = '')),
                column(3,
                       radioGroupButtons(
                           inputId = "seleccionar_series_ofms",
                           label = "OFMs",
                           choices = c("Separados", "Unidos"),
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon"))))),
            fluidRow(
                column(3,
                       radioGroupButtons(
                           inputId = "seleccionar_series_rango",
                           label = "Seleccionar fechas",
                           choices = c("1200-1800", "1500-1800"),
                           checkIcon = list(
                               yes = icon("ok",
                                          lib = "glyphicon")))),
                column(3,
                       sliderInput("slider_series_rango",
                                   label = "Fechas",
                                   min = 1200, max = 1800,
                                   value = c(1200, 1800)))),

            h4("Serie general"),
            plotOutput("orrg_series_general"),
            hr(),
            h4("Por países"),
            plotOutput("orrg_series_general_porpaises"))


variadostabs <- tabItems(tabgeneral, tabdiocgeneral, tabindividual, tabsecnosec, tabdioc_orrg,
                         tab5, tab6, tabduracion, tabperiplogeneral, tabpresencia_orrg_diocs,
                         tab_orrg_motivosfin,
                         tab_ordenes, tabconsultageneral, tab_monasterioobispado,
                         tab_bizancio, tab_seriestemporales,
                         tab8, tab_orrg_periplos, tab_cnf1, tab_cnf2)
