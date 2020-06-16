# Dashboard

dashboardPage(skin = "blue",
              dashboardHeader(title = "Afiliados"),
              dashboardSidebar(
                sidebarMenu(disable = TRUE, br(),
                            tags$img(src = "Logo.png", height=40, width=200, align="center"),
                            shinyjs::hidden(menuItem("INSTRUCCIONES", tabName = "dummy")),
                            menuItem("PUNTO", tabName = "punto", icon = icon("dashboard"),
                                     menuSubItem("Check", tabName = "punto", icon = icon("check-circle")),
                                     sliderInput("Distancia3", label = h3("Distancia (km)"), min = 0.1,max = 5, step = 0.1, value=1),
                                     h5("Escriba coordenadas"),
                                     textInput("CX",label = "Longitud", value = "-74.135424"),
                                     textInput("CY", label = "Latitud", value = "4.618715"),
                                     br(),
                                     h4("Ejecutar consulta"),
                                     actionButton("go7", label = "Go"),
                                     br(),
                                     tags$a("Consulta Coordenadas", 
                                            href = "https://geoportal.dane.gov.co/v3/en/catastro.html"),
                                     tags$hr()
                                     ),
                            menuItem("PUNTOS", tabName = "puntos", icon = icon("dashboard"),
                                     menuSubItem("Check", tabName = "puntos", icon = icon("check-circle")),
                                     sliderInput("Distancia2", label = h3("Distancia (km)"), min = 0.1,max = 5, step = 0.1, value= 2),
                                     fileInput("plus", label= h6("Cargue el Listado de Puntos (csv)"), 
                                               multiple = FALSE, placeholder="Explore el archivo", buttonLabel="Buscar",
                                               accept = c("text/csv", "text/comma-separated-values,text/plain",".csv")),
                                     br(),
                                     h4("Ejecutar consulta"),
                                     actionButton("go2", label = "Go"),
                                     tags$hr()
                                     ),
                            menuItem("EMPRESA", tabName = "empresa", icon = icon("dashboard"),
                                     menuSubItem("Check", tabName = "empresa", icon = icon("check-circle")),
                                     radioButtons("tipodoc","Tipo de documento",
                                                  choices = c("NIT" = "NIT",
                                                              "Cédula de Ciudadania" = "CC",
                                                              "Tarjeta de Identidad" = "TI",
                                                              "Registro Civil" = "RC",
                                                              "Cedula de Extranjería" = "CE",
                                                              "NUIP" = "NUIP",
                                                              "Pasaporte" = "PAS",
                                                              "Carnet Diplomatico" = "CD"),
                                                  selected = "NIT"),
                                     textInput("xnit_empresa", "Nit empresa", value = "8001972684"),
                                     br(),
                                     actionButton("go", label = "Go"))
                            )),
              dashboardBody(
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"),
                tabItems(
                  tabItem("dummy",
                          fluidRow(
                            column(1),
                            column(10,
                                   h1("Aplicacion Georrefenciación Afiliados y Empresas"),
                                   br(),br(),
                                   h3("La presente aplicacion tiene por objetivo ser una herramienta de consulta de geolocalizacion por coordenaadas de Latitud y 
                                   longitud.El documento se divide en 3 secciones:"),
                                   br(),
                                   h3("La primera pestaña muestra informacion por 'Punto' indicando las coordendas de Latitud y Longitud."),
                                   br(),
                                   h3("La segunda pestaña corresponde a una consulta por 'Puntos' cargando un archivo .csv con la siguiente estructura:"),
                                   div(img(src = "Alignment.PNG", width = 500), style = "text-align: center;"),
                                   h3("Una vez cargado el archivo .csv puede visualizar los puntos en el mapa. Para realizar la consulta de click en 'Go'.
                                      El tiempo aproximado de consulta para 10 puntos es de 45 segundos."),
                                   br(),
                                   h3("La tercera pestaña tiene por objetivo mostrar la ubicacion de empleados por empresa"),
                                   br(),
                                   h3("Nota: En cada cambio de pestaña debe dar click en 'Check' para cambiar el panel principal y luego 'Go' para ejecutar las consultas"),
                                   br(),
                                   h4("Fecha actualizacion: 16/06/2020 (Corte Mayo)")
                            ),
                            column(1)
                          )
                  ),
                  tabItem(tabName = "punto",
                          h3("Ubicacion punto"),
                          p(class = "text_small", "En esta seccion puede encontrar la geolocalizacion del punto y resumen descriptivo"),
                          fluidRow(
                            column(width = 6,
                                   box(title = "Ubicacion Punto (Viven y/o trabajan)",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       withLoader(leafletOutput("Mapapunto", height = 900),type="html", loader="loader1"), width=12),
                                   column(3,
                                          downloadButton("downloadData_viven", "Afiliados Viven")),
                                   column(3,
                                          downloadButton("downloadData_trabajan", "Afiliados Trabajan")),
                                   column(3,
                                          downloadButton("downloadData", "Afiliados Viven o Trabajan"))
                                   ),
                            column(width = 6,
                                   fluidRow(
                                     box(title = "Resumen Afiliados",status = "primary", solidHeader = TRUE,collapsible = TRUE, width = 12,
                                         valueBoxOutput("info_emp1",width = 3),
                                         valueBoxOutput("info_emp2",width = 3),
                                         valueBoxOutput("info_emp3",width = 3),
                                         valueBoxOutput("info_emp4",width = 3))
                                   ),
                                   fluidRow(
                                   box(title = "Resumen Afiliados por Edad y Categoria",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("resumen_punto_afiliado"), width = 12)
                                   ),
                                   fluidRow(
                                   box(title = "Resumen Afiliados por Genero y Segmento",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("resumen_punto_afiliado4"), width = 12)
                                   ),
                                   fluidRow(
                                   box(title = "Resumen Afiliados por Edad y Segmento",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("resumen_punto_afiliado2"), width = 12)
                                   ),
                                   fluidRow(
                                   box(title = "Resumen Afiliados por Sector y Segmento",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       dataTableOutput("resumen_punto_afiliado3"), width = 12)
                                   ),
                                   fluidRow(
                                     box(title = "Resumen Empresa por Actividad y Piramide 1",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                         dataTableOutput("resumen_empresa_actividad"), width = 12)
                                   )
                                   ))
                  ),
                  tabItem(tabName = "puntos",
                          fluidRow(
                            column(6,
                                   box(title = "Ubicacion Puntos",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       withLoader(leafletOutput("Mapapunto2", height = 900),type="html",loader="loader1"), width=12)),
                            column(6,
                                   h3("Previsualizacion Puntos"),
                                   br(),
                                   withLoader(dataTableOutput("Preview"),type="html",loader="loader1"),
                                   br(),
                                   fluidRow(
                                     box(title = "Resumen Afiliados",status = "primary", solidHeader = FALSE, collapsible = TRUE, width = 12,
                                         valueBoxOutput("info_emp1_p",width = 4),
                                         valueBoxOutput("info_emp2_p",width = 4),
                                         valueBoxOutput("info_emp3_p",width = 4)
                                         )
                                   ),
                                   h3("Afiliados Objetivo"),
                                   br(),
                                   withLoader(dataTableOutput("Calificada"),type="html",loader="loader1"),
                                   br(),
                                   column(3,
                                          downloadButton("downloadData_viven2", "Afiliados Viven")),
                                   column(3,
                                          downloadButton("downloadData_trabajan2", "Afiliados Trabajan")),
                                   column(3,
                                          downloadButton("downloadData2", "Afiliados Viven o Trabajan")),
                                   br()))
                  ),
                  tabItem(tabName = "empresa",
                          h3("Ubicacion afiliados"),
                          p(class = "text_small", "En esta seccion puede encontrar la geolocalizacion de los afiliados a una empresa"),
                          fluidRow(
                            column(12,
                                   box(title = "Ubicacion Afiliados",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                                       withLoader(leafletOutput("Mapaafiliados", height = 1200), type="html", loader="loader1"), width=12))
                            # ,
                            # column(6,
                            #        box(title = "Ubicacion Infraestructura",status = "primary", solidHeader = TRUE,collapsible = TRUE,
                            #            leafletOutput("Mapaafiliados2", height = 750), width=12))
                          )
                    
                  )
                )
              )
)

