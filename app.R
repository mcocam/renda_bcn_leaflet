
library(shiny)
library(leaflet)
library(shinyWidgets)
library(classInt)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ineq)
library(sp)

seccio = readRDS("shapes/SeccioCensal.Rds")
barri = readRDS("shapes/Barri.Rds")
districte = readRDS("shapes/Districte.Rds")
perfil = readRDS("shapes/Perfil.Rds")

dades = readRDS("data/data.Rds")

Years_c = unique(dades$Any)
Years_c = sort(Years_c, decreasing = T)

dataAct = paste0("12/03/2022")


ui <- tagList(
    
    #CSS
    tags$head(
        tags$style(type = "text/css",
                   HTML(
                       "@import url('https://fonts.googleapis.com/css2?family=Lexend:wght@400&display=swap');
                        @import url('https://fonts.googleapis.com/css2?family=Otomanopee+One&display=swap');

                                body {
                                    font-family: 'Lexend', sans-serif;
                                }
                                    
                                .navbar-brand {
                                    font-family: 'Otomanopee One', sans-serif;
                                }
                                h2 {
                                    font-family: 'Otomanopee One', sans-serif;
                                    text-align: center;
                                    padding-bottom: 2%;
                                }
                                
                                a {
                                text-decoration: none;
                                color: #7C83FD;
                                }
                                
                                a:link {
                                text-decoration: none;
                                color: #7C83FD;
                                }
                                
                                a:hover {
                                text-decoration: none;
                                color: #185ADB;
                                font-weight: bold;
                                }
                                
                                a:visited {
                                text-decoration: none;
                                color: #7C83FD;
                                
                                }
                                
                                a:active {
                                text-decoration: none;
                                color: #185ADB;
                                font-weight: bold;
                                
                                }
                                
                                p {
                                
                                font-family: 'Lexend', sans-serif;
                                margin-left: 10%;
                                margin-right: 10%;}
                       
                       .navbar-default .navbar-nav > .active > a:hover {
                                background-color: white;
                                font-weight: bold;
                                color: #0A1931;}
                                
                       .navbar-default .navbar-nav > li > a:hover {
                                background-color: white;
                                font-weight: bold;
                                color: #0A1931;}
                                
                       #leaf_renda {height: calc(100vh - 53px) !important;}"
                   ))
        ),
    
    #Content
    fluidPage(
        navbarPage(title = "Renda mitjana Barcelona",
                   collapsible = T,
                   id = "page-nav",
                   tabPanel(title = "Mapa",
                            icon = icon("map-marked-alt"),
                            id = "tabpanel1",
                            fluidRow(
                                
                                
                                sidebarPanel(
                                    selectInput("mapa",
                                                label = div(HTML("<b>Agregació</b>")),
                                                choices = c("Secció censal",
                                                            "Barri",
                                                            "Districte"),
                                                selected = "Secció censal"),
                                    
                                    selectInput("projeccio",
                                                label = div(HTML("<b>Funció de projecció per l'agregació (no aplica Secció Censal)</b>")),
                                                choices = c("Mitjana",
                                                            "Mediana"),
                                                selected = "Mitjana"),
                                    
                                    radioButtons("years",
                                                 label = div(HTML("<b>Any</b>")),
                                                 choices = Years_c,
                                                 selected = Years_c[1],
                                                 inline = T),
                                    
                                    plotOutput("kernels",
                                               height = "300"),
                                    
                                    plotOutput("evolutiu",
                                               height = "200")
                                ),
                                
                                
                                mainPanel(
                                    leafletOutput("leaf_renda")
                                ))
                            ),
                   tabPanel(title = "Info",
                            icon = icon("info"),
                            
                            h2("L'aplicació"),
                            p("L'espai web mostra les dades sobre renda mitjana de la ciutat de barcelona que publica ", a(href = "https://opendata-ajuntament.barcelona.cat/data/ca/dataset/renda-tributaria-per-persona-atlas-distribucio", "Open Data BCN", target="_blank"), "."),
                            p("En origen, les dades es troben agregades per secció censal, però es dona la possibilitat de projectar-les per barris o districtes. Aquí és on entra en joc la", strong("funció de projecció"), "que permet agregar les dades segons la mitjana o la mediana dels seus districtes corresponents."),
                            p("L'aplicació s'ha elaborat amb llenguatge R (framework Shiny), en plan hobby, i tots els errors van a càrreg de l'autor"),
                            h2("L'autor"),
                            p("Hola! Em dic Marc, soc de Lleida i, si voleu, podeu seguir-me a ", a(href= "https://twitter.com/marc_coca", "Twitter", target="_blank"), ", on comparteixo, sobretot, visualitzacions de dades, aplicacions com aquestes, codis, etc."),
                            h2("Fonts i data d'actualització"),
                            p("· ", a(href = "https://opendata-ajuntament.barcelona.cat/data/ca/dataset/renda-tributaria-per-persona-atlas-distribucio", "Open Data BCN", target="_blank")),
                            p("· Última data d'actualització: ", dataAct)
                            )
                   )
        )
    
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$leaf_renda = renderLeaflet({
        
        data_temp = dades%>%
            filter(Any == input$years)%>%
            select(DIST_SECCIO,
                   Codi_Barri,
                   Codi_Districte,
                   Nom_Barri,
                   Nom_Districte,
                   Import_Euros)
        
        
        
        if (input$mapa == "Secció censal"){
            
            seccio_d = merge(seccio,
                             data_temp,
                             by.x = "DIST_SECCI",
                             by.y = "DIST_SECCIO")
            
            BinsJenks = unique(classIntervals(seccio_d$Import_Euros, 5,
                                              style = "jenks", unique = T, samp_prop = 0.5)$brks)
            
            SeccioPal = colorBin("viridis", 
                                 domain = seccio_d$Import_Euros,
                                 bins = BinsJenks,
                                 na.color = "white", reverse = T)
            
            leaflet()%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                addPolygons(data = seccio_d,
                            weight = 0.5,
                            fillColor = ~SeccioPal(seccio_d$Import_Euros),
                            fillOpacity = 0.8,
                            color = ifelse(seccio_d$Import_Euros<BinsJenks[2], "black", "white"))%>%
                
                addPolygons(data = districte,
                            weight = 2,
                            fillOpacity = 0,
                            color = "white")%>%
                addPolygons(data = perfil, 
                            color = "black", 
                            fillOpacity = 0,
                            weight = 2)%>%
                addPolygons(data = seccio_d,
                            weight = 0,
                            fillOpacity = 0,
                            
                            popup = paste("<b>Districte: </b>", seccio_d$Nom_Districte,"<br>",
                                          "<b>Barri: </b>", seccio_d$Nom_Barri,"<br>",
                                          "<b>Secció censal: </b>", seccio_d$SECCIO,"<br>",
                                          "<b>Renda mitjana: </b>", format(seccio_d$Import_Euros, big.mark = ".", decimal.mark = ","),
                                          " (",input$years, ")"))%>%
                addLegend(pal = SeccioPal,
                          values = seccio_d$Import_Euros,
                          labFormat = labelFormat(big.mark = " ", digits = 0),
                          position = "bottomright",
                          na.label = "",
                          title = paste("<b>Renda mitjana</b><br><small>(Intervals Jenks)</small>"))
            
        }else if (input$mapa == "Barri"){
            
            if (input$projeccio == "Mitjana"){
                
                data_temp = data_temp%>%
                    group_by(Codi_Barri,
                             Nom_Barri,
                             Nom_Districte)%>%
                    summarise(Import_Euros = mean(Import_Euros))%>%
                    ungroup()
                
            }else if (input$projeccio == "Mediana"){
                
                data_temp = data_temp%>%
                    group_by(Codi_Barri,
                             Nom_Barri,
                             Nom_Districte)%>%
                    summarise(Import_Euros = median(Import_Euros))%>%
                    ungroup()
                
            }
            
            barri_d = merge(barri,
                             data_temp,
                             by.x = "Barri",
                             by.y = "Codi_Barri")
            
            BinsJenks_b = unique(classIntervals(barri_d$Import_Euros, 5,
                                  style = "jenks", unique = T, samp_prop = 0.5)$brks)
            
            BarriPal = colorBin("viridis", 
                                 domain = barri_d$Import_Euros,
                                 bins = BinsJenks_b,
                                 na.color = "white", reverse = T)
            
            leaflet()%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                addPolygons(data = barri_d,
                            weight = 0.5,
                            fillColor = ~BarriPal(barri_d$Import_Euros),
                            fillOpacity = 0.8,
                            color = ifelse(barri_d$Import_Euros<BinsJenks_b[2], "black", "white"),
                            popup = paste("<b>Districte: </b>", barri_d$Nom_Districte,"<br>",
                                          "<b>Barri: </b>", barri_d$Nom_Barri,"<br>",
                                          ifelse(input$projeccio == "Mitjana", "<b>Renda mitjana: </b>", "<b>Renda mediana: </b>"), format(barri_d$Import_Euros, big.mark = ".", decimal.mark = ","),
                                          " (",input$years, ")"))%>%
                addPolygons(data = districte,
                            weight = 2,
                            fillOpacity = 0,
                            color = "white")%>%
                addPolygons(data = perfil, 
                            color = "black", 
                            fillOpacity = 0,
                            weight = 2)%>%
                addPolygons(data = barri_d,
                            weight = 0,
                            fillOpacity = 0,
                            popup = paste("<b>Districte: </b>", barri_d$Nom_Districte,"<br>",
                                          "<b>Barri: </b>", barri_d$Nom_Barri,"<br>",
                                          ifelse(input$projeccio == "Mitjana", "<b>Renda mitjana: </b>", "<b>Renda mediana: </b>"), format(barri_d$Import_Euros, big.mark = ".", decimal.mark = ","),
                                          " (",input$years, ")"))%>%
                addLegend(pal = BarriPal,
                          values = barri_d$Import_Euros,
                          labFormat = labelFormat(big.mark = " ", digits = 0),
                          position = "bottomright",
                          na.label = "",
                          title = ifelse(input$projeccio == "Mitjana", paste("<b>Renda mitjana</b><br><small>(Intervals Jenks)</small>"),
                                         paste("<b>Renda mediana</b><br><small>(Intervals Jenks)</small>")))
            
        }else if (input$mapa == "Districte"){
            
            if (input$projeccio == "Mitjana"){
                
                data_temp = data_temp%>%
                    group_by(Codi_Districte,
                             Nom_Districte)%>%
                    summarise(Import_Euros = mean(Import_Euros))%>%
                    ungroup()
                
            }else if (input$projeccio == "Mediana"){
                
                data_temp = data_temp%>%
                    group_by(Codi_Districte,
                             Nom_Districte)%>%
                    summarise(Import_Euros = median(Import_Euros))%>%
                    ungroup()
                
            }
            
            dist_d = merge(districte,
                            data_temp,
                            by.x = "DISTRICTE",
                            by.y = "Codi_Districte")
            
            BinsJenks_d = unique(classIntervals(dist_d$Import_Euros, 5,
                                                style = "jenks", unique = T, samp_prop = 0.5)$brks)
            
            DistPal = colorBin("viridis", 
                                domain = dist_d$Import_Euros,
                                bins = BinsJenks_d,
                                na.color = "white", reverse = T)
            
            leaflet()%>%
                addProviderTiles(providers$CartoDB.Positron)%>%
                addPolygons(data = dist_d,
                            weight = 1,
                            fillColor = ~DistPal(dist_d$Import_Euros),
                            fillOpacity = 0.8,
                            color = ifelse(dist_d$Import_Euros<BinsJenks_d[2], "black", "white"))%>%
                addPolygons(data = perfil, 
                            color = "black", 
                            fillOpacity = 0,
                            weight = 2)%>%
                addPolygons(data = dist_d,
                            weight = 0,
                            fillOpacity = 0,
                            popup = paste("<b>Districte: </b>", dist_d$Nom_Districte,"<br>",
                                          ifelse(input$projeccio == "Mitjana", "<b>Renda mitjana: </b>", "<b>Renda mediana: </b>"), format(dist_d$Import_Euros, big.mark = ".", decimal.mark = ","),
                                          " (",input$years, ")"))%>%
                addLegend(pal = DistPal,
                          values = dist_d$Import_Euros,
                          labFormat = labelFormat(big.mark = " ", digits = 0),
                          position = "bottomright",
                          na.label = "",
                          title = ifelse(input$projeccio == "Mitjana", paste("<b>Renda mitjana</b><br><small>(Intervals Jenks)</small>"),
                                         paste("<b>Renda mediana</b><br><small>(Intervals Jenks)</small>")))
            
        }
        
    })
    
    output$kernels = renderPlot({
        
        ggplot(data = dades,
               aes(Import_Euros, colour = factor(Any), fill = factor(Any)))+
            geom_density(alpha = 0.2,
                         size = 1)+
            scale_fill_tableau()+
            scale_colour_tableau()+
            labs(x = "Renda",
                 y = "Densitat",
                 title = "Distribució de la renda mitjana per districtres",
                 subtitle = paste0(min(dades$Any), " - ", max(dades$Any)),
                 fill = "",
                 color = "")+
            theme_hc()+
            theme(axis.text.y = element_blank(),
                  
                  panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill = "transparent"),
                  legend.box.background = element_blank())
        
    },
    bg="transparent")
    
    output$evolutiu = renderPlot({
        
        dades_ineq = dades%>%
            group_by(Any)%>%
            summarise(Gini = ineq(Import_Euros, type = "Gini"))%>%
            ungroup()
        
        ggplot(data = dades_ineq,
               aes(x = Any, y = Gini))+
            geom_line(color = "#185ADB")+
            geom_point()+
            labs(x = "",
                 y = "Coeficient de Gini",
                 title = "Desigualtat en la renda (districtes)")+
            theme_hc()+
            theme(panel.background = element_rect(fill = "transparent"),
                  plot.background = element_rect(fill = "transparent", color = NA),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.background = element_rect(fill = "transparent"),
                  legend.box.background = element_blank())
        
    },
    bg="transparent")
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)
