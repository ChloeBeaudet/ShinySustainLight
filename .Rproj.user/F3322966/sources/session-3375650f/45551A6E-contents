#################################################################
#                                                               #
#                                                               #
#                       Script shiny                            # 
#                                                               #
#                                                               #
#################################################################
rm(list=ls())
library(shiny)
library(shinythemes)
library(shinycustomloader)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(leaflegend)
library(terra)
library(tidyterra)


load("Data_vf.RData")

# Source text for the "About" panel
tabPanelAbout <- source("About.R")$value

## App

ui <- bootstrapPage(
  navbarPage(
    title = HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;font-weight:normal;" class="active" href="#">Planning sustainable lighting for biodiversity and society</a>'), id="nav",
    windowTitle = "SustainLight", 
    
    
    tabPanelAbout(), 
    
    # Summary Map
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Summary Map</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 withLoader(leafletOutput("map4", height="95vh"), loader = "loader3")),
             
             absolutePanel(                   
               id = "control1", class = "panel panel-default", fixed = TRUE,
               draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
               width = 345, height = "auto",
               
               h2("Summary Map"),
               #h3("Montpellier metropolitan area"),
               
               selectInput("choix_indic_accept_synthese",
                           label = strong("Choose the indicator of social acceptation"), 
                           choice = c("Extinction 1 a.m. - 5 a.m." = "1", 
                                      "Extinction 11 p.m. - 6 a.m." = "2")
               ),
               selectInput("priority_synthese",
                           label = strong("Choose the indicator of ecological stakes"), 
                           choice = c("Priority areas for light pollution mitigation policies (high ecological stakes without light pollution)" = "2", 
                                      "Priority areas for light pollution mitigation policies (moderate ecological stakes without light pollution)" = "3"),  
               ),
               
               actionButton("go3", label = "Go!", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               #tags$style(type='text/css', "button#go3 {margin-left: 60%;}"), 
               
             )),
    # Socio-economic indicators
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Socio-economic indicators</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 withLoader(leafletOutput("map3", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(
                   id = "control1", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 395, height = "auto",
                   
                   h2("Socio-economic indicators"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("choix_indic_accept",
                               label = strong("Choose the indicator of social acceptation"), 
                               choice = c("Extinction 1 a.m. - 5 a.m." = "1", 
                                          "Extinction 11 p.m. - 6 a.m." = "2")
                   )
                 )
             )), 
    
    
    
    # Ecological indicators
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Ecological indicators</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 
                 withLoader(leafletOutput("map1", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(                   
                   id = "control1", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 345, height = "auto",
                   
                   h2("Ecological indicators"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("famille_espece",
                               label = strong("Choose a species family"), 
                               choice = c("European nightjar" = "Nightjar",
                                          "Amphibians"= "Amphibian", 
                                          "Insects (wetlands)" = "Insects",
                                          "Lampyridae"  = "Lampyridae",
                                          "Myotis spp" = "Murine",
                                          "Rhinolophus spp" = "Rhinolophus", 
                                          "Global Indicators for ecological stakes" = "global"
                               )
                   ),
                   selectInput("indicateur_ecolo",
                               label = strong("Choose an indicator"),
                               choice = c("Impact of light pollution on dispersion according to ecological stakes" = "priority23",
                                          "Biodiversity reservoirs" = "rb")
                   ), 
                   
                   actionButton("go", label = "Go!", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                   #tags$style(type='text/css', "button#go {margin-left: 60%;}")
                   
                 )
             )),
    # Light pollution indicators
    tabPanel(HTML('<span style="font-size:100%;color:white;font-weight:normal;">Light pollution indicator</span></a>'),
             
             tags$head(
               includeCSS("styles.css"), tags$link(rel = "icon", type = "image/png", href = "starry_sky.jpg")
             ),
             div(class="outer",
                 
                 withLoader(leafletOutput("map0", height="95vh"), loader = "loader3"),
                 
                 absolutePanel(
                   id = "control1", class = "panel panel-default", fixed = TRUE,
                   draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                   width = 365, height = "auto",
                   
                   h2("Light pollution indicators"),
                   #h3("Montpellier metropolitan area"),
                   
                   selectInput("indicator_pollum",
                               label = strong(""), 
                               choice = c("Level of upward emission" = "upward_emission"
                               ),  
                   )
                 )
                 
             )
    )
    
    
    
    
    
    
    
  ))

server <- function(input, output, session) {
  #########################################
  ##### 0 - Panel Light pollution     #####
  #########################################
  
  df_map0 <- reactive({
    
    dataset <- df_pollum %>% filter(type == input$indicator_pollum)
    
    return(dataset)
    
  })
  
  map0 <- reactive({
    
    
    pal <-  c("#1749e4", "#F9DC4C", "#fd0504")
    
    domain <- factor(df_map0()$level, levels = c("Low","Moderate", "High"))
    
    pal2 <- colorNumeric(pal, 
                         domain =  df_map0()$indice)
    
    pal3 <- colorFactor(pal, 
                        domain =  domain)
    
    labels <- sprintf(
                       "%s",
                       paste0(df_map0()$level, " level of upward emission")
                     ) %>% lapply(htmltools::HTML)
    
    
    title <- "Level of upward emission"
    
    map0 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      
      addPolygons(data = df_mmm_shp,
                  color = "grey", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0,
                  fillColor = "white",
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "black", weight = 2,
                  #                                     bringToFront = TRUE)
      ) %>%
      
      addPolygons(data = df_map0(), color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.9,
                  fillColor = ~pal2(df_map0()$indice),
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal3, values = domain,
                title = title,
                opacity = 1
      )
    
    
  })
  
  output$map0 <- renderLeaflet(map0())
  
  
  
  
  #########################################
  ##### 1 - Panel Ecological stakes   #####
  #########################################
  
  
  df <- tibble("Nightjar" = c("Impact of light pollution on dispersion according to ecological stakes",
                              "Biodiversity reservoirs"),
               "Amphibian" = c("Biodiversity reservoirs", 
                               NA), 
               "Lampyridae" = c("Impact of light pollution on dispersion according to ecological stakes",
                                "Biodiversity reservoirs"), 
               "Insects" = c("Impact of light pollution on dispersion according to ecological stakes",
                             "Biodiversity reservoirs"), 
               "Murine" = c("Impact of light pollution on dispersion according to ecological stakes",
                            NA),
               "Rhinolophus" = c("Impact of light pollution on dispersion according to ecological stakes",
                                 NA),
               "global" = c("Overall score", 
                            "Priority areas for light pollution mitigation policies")) %>%
    gather(species, variable, Nightjar:global) %>%
    filter(!is.na(variable))
  
  observe({
    x <- df[df$species == input$famille_espece,]$variable
    
    if (is.null(x))
      x <- character(0)
    
    h6(updateSelectInput(getDefaultReactiveDomain(), "indicateur_ecolo",
                         label = NULL,
                         choices = x
    ))})
  
  
  
  
  map1 <- reactive({
    
    df_functionality_map3_nightjar <- df_map_species %>%
      filter(species == "Nightjar", 
             type == "priority23",
             priority == "3") %>%
      mutate(legend = factor(legend, levels = c("Strong loss of functionality", "Moderate loss of functionality")))
    
    
    df_functionality_map2_nightjar <- df_map_species %>%
      filter(species == "Nightjar", 
             type == "priority23",
             priority == "2") %>%
      mutate(legend = factor(legend, levels = c("Strong loss of functionality", "Moderate loss of functionality")))
    
    
    pal3 <- colorFactor(c("#092e6a", "#3b97c8"), 
                        domain = df_functionality_map3_nightjar[["indicator"]])
    
    pal33 <- colorFactor(c("#092e6a", "#3b97c8"), 
                         domain = df_functionality_map3_nightjar[["legend"]])
    
    pal2 <- colorFactor(c("#b31700", "#ef6547"), 
                        domain =  df_functionality_map2_nightjar[["indicator"]])
    
    pal22 <- colorFactor(c("#b31700", "#ef6547"), 
                         domain = df_functionality_map2_nightjar[["legend"]])
    
    labels2 <- sprintf(
      paste0("<strong> Nightjar </strong><br/>%s<br/>%s"),
      "Areas with high ecological stakes due to light pollution",
      paste0(df_functionality_map2_nightjar[["legend"]], " due to light pollution")
    ) %>% lapply(htmltools::HTML)
    
    labels3 <- sprintf(
      paste0("<strong> Nightjar </strong><br/>%s<br/>%s"),
      "Areas with moderate ecological stakes due to light pollution",
      paste0(df_functionality_map3_nightjar[["legend"]], " due to light pollution")
    ) %>% lapply(htmltools::HTML)
    
    map2 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(data = df_functionality_map2_nightjar,
                  color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.7,
                  fillColor = ~pal2(df_functionality_map2_nightjar[["indicator"]]),
                  highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                      bringToFront = TRUE),
                  label = labels2,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addPolygons(data = df_functionality_map3_nightjar,
                  color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                  opacity = 1, fillOpacity = 0.7,
                  fillColor = ~pal3(df_functionality_map3_nightjar[["indicator"]]),
                  highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                      bringToFront = TRUE),
                  label = labels3,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal33, values = df_functionality_map3_nightjar[["legend"]],
                #title = "<strong style='font-weight:normal'>Areas with moderate ecological stakes without light pollution</strong><br>",
                title = HTML("
                  <div style='width: 380px;'>
                    <span style='font-weight: normal;'>Areas with moderate ecological stakes without light pollution</span><br>
                  </div>
                "),
                opacity = 1
      ) %>%
      
      addLegend("bottomleft", pal = pal22, values = df_functionality_map2_nightjar[["legend"]],
                #title = paste0("Impact of light pollution on dispersion (Nightjar) <br> <strong style='font-weight:normal'>Areas with high ecological stakes without light pollution</strong><br>"),
                title = HTML("
                  <div style='width: 380px;'>
                    <b>Impact of light pollution on dispersion (Nightjar)</b><br>
                    <span style='font-weight: normal;'>Areas with high ecological stakes without light pollution</span><br>
                  </div>
                "),
                opacity = 1
      ) 
    
    
  })
  
  output$map1 <- renderLeaflet(map1())
  
  df_map1 <- reactive({
    
    indicateur_ecolo_abreviation <- switch(input$indicateur_ecolo,
                                           "Impact of light pollution on dispersion according to ecological stakes" = "priority23",
                                           "Biodiversity reservoirs" = "rb",
                                           "Overall score" = "note",
                                           "Priority areas for light pollution mitigation policies" = "prio2_lighted_note")
    
    dataset <- df_map_species %>%
      filter(species == input$famille_espece, 
             type == indicateur_ecolo_abreviation)
    
    return(dataset)
    
  })
  
  observeEvent(input$go, {
    
    indicateur_ecolo_abreviation <- switch(input$indicateur_ecolo,
                                           "Impact of light pollution on dispersion according to ecological stakes" = "priority23",
                                           "Biodiversity reservoirs" = "rb",
                                           "Overall score" = "note",
                                           "Priority areas for light pollution mitigation policies" = "prio2_lighted_note")
    
    if(indicateur_ecolo_abreviation == "priority23"){
      
      df_functionality_map3 <- df_map1() %>%
        filter(priority == "3") %>%
        mutate(legend = factor(legend, levels = c("Strong loss of functionality", "Moderate loss of functionality")))
      
      
      df_functionality_map2 <- df_map1() %>%
        filter(priority == "2") %>%
        mutate(legend = factor(legend, levels = c("Strong loss of functionality", "Moderate loss of functionality")))
      
      
      pal3 <- colorFactor(c("#092e6a", "#3b97c8"), 
                          domain = df_functionality_map3[["indicator"]])
      
      pal33 <- colorFactor(c("#092e6a", "#3b97c8"), 
                           domain = df_functionality_map3[["legend"]])
      
      pal2 <- colorFactor(c("#b31700", "#ef6547"), 
                          domain =  df_functionality_map2[["indicator"]])
      
      pal22 <- colorFactor(c("#b31700", "#ef6547"), 
                           domain = df_functionality_map2[["legend"]])
      
      labels2 <- sprintf(
        paste0("<strong>", as.character(input$famille_espece), "</strong><br/>%s<br/>%s"),
        "Areas with high ecological stakes due to light pollution",
        paste0(df_functionality_map2[["legend"]], " due to light pollution")
      ) %>% lapply(htmltools::HTML)
      
      labels3 <- sprintf(
        paste0("<strong>", as.character(input$famille_espece), "</strong><br/>%s<br/>%s"),
        "Areas with moderate ecological stakes due to light pollution",
        paste0(df_functionality_map3[["legend"]], " due to light pollution")
      ) %>% lapply(htmltools::HTML)
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(data = df_functionality_map2,
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_functionality_map2[["indicator"]]),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addPolygons(data = df_functionality_map3,
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal3(df_functionality_map3[["indicator"]]),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels3,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addLegend("bottomleft", pal = pal33, values = df_functionality_map3[["legend"]],
                  #title = "<strong style='font-weight:normal'>Areas with moderate ecological stakes without light pollution</strong><br>",
                  title = HTML("
                  <div style='width: 380px;'>
                    <span style='font-weight: normal;'>Areas with moderate ecological stakes without light pollution</span><br>
                  </div>
                "),
                  opacity = 1
        )%>%
        
        addLegend("bottomleft", pal = pal22, values = df_functionality_map2[["legend"]],
                  #title = paste0("Impact of light pollution on dispersion (", as.character(input$famille_espece), ") <br> <strong style='font-weight:normal'>Areas with high ecological stakes without light pollution</strong><br>"),
                  title = HTML(
                    paste0("<div style='width: 380px;'>",
                           "<b>Impact of light pollution on dispersion (", as.character(input$famille_espece),")</b><br>",
                           "<span style='font-weight: normal;'>Areas with high ecological stakes without light pollution</span><br>
                  </div>")
                  ),
                  opacity = 1
        ) 
      
      
      
      
      
      
      
    }
    
    else if (indicateur_ecolo_abreviation == "rb") {
      df_rb <- df_map1() %>%
        mutate(legend = factor(legend, levels = c("Unaffected by light pollution", 
                                                  "Affected by light pollution")))
      
      pal2 <- colorFactor(c("#092e6a", "#a0c9e4"), 
                          domain =  df_rb$indicator)
      
      pal22 <- colorFactor(c("#092e6a", "#a0c9e4"), 
                           domain =  df_rb$legend)
      
      labels2 <- sprintf(
        paste0("<strong>",  as.character(input$famille_espece), "</strong><br/>%s"),
        df_rb$legend
      ) %>% lapply(htmltools::HTML)
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(data = df_rb, 
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_rb[["indicator"]]),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        addLegend("bottomleft", pal = pal22, values = df_rb$legend,
                  title = paste0("Biodiversity reservoirs (", as.character(input$famille_espece), ")"), 
                  
                  opacity = 1
        ) 
      
      
    }
    
    else{
      palette_indicator <- switch(indicateur_ecolo_abreviation, 
                                  "note" = c("#fde725", "#9ed93a", "#4ac26d", "#1fa288", "#277f8e", "#365c8d", "#46327f", "#440154"), 
                                  "prio2_lighted_note" = c("#fde725", "#9ed93a", "#4ac26d", "#1fa288", "#277f8e", "#365c8d", "#46327f", "#440154"))
      
      pal2 <- colorFactor(palette_indicator, 
                          domain =  df_map1()$indicator)
      
      
      labels2 <- switch(indicateur_ecolo_abreviation, 
                        "note" = sprintf(
                          "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                          "Overall score", 
                          paste0(ifelse(df_map1()$priority %in% c("2"), "High ecological stakes without light pollution", "Moderate ecological stakes without light pollution")), 
                          paste0(df_map1()$lose_functionality, " loss of functionality"),
                          paste0(df_map1()$impacted_species, " impacted groups of species")
                        ) %>% lapply(htmltools::HTML), 
                        
                        "prio2_lighted_note" = sprintf(
                          "<strong>%s</strong><br/>%s<br/>%s<br/>%s",
                          "Overall score", 
                          paste0(ifelse(df_map1()$priority %in% c("2"), "High ecological stakes without light pollution", "Moderate ecological stakes without light pollution")), 
                          paste0(df_map1()$lose_functionality, " loss of functionality"),
                          paste0(df_map1()$impacted_species, " impacted groups of species")
                        ) %>% lapply(htmltools::HTML))
      
      image <- switch(indicateur_ecolo_abreviation, 
                      "note" = "legend_map_global", 
                      "prio2_lighted_note" = "legend_map_light")
      
      title <- switch(indicateur_ecolo_abreviation,
                      "note" = "Overall score",
                      "prio2_lighted_note" = "Priority areas for light pollution mitigation policies")
      
      
      
      map2 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(data = df_map1(),
                    color = "#ffffff00", weight = 1, smoothFactor = 0.5,
                    opacity = 1, fillOpacity = 0.7,
                    fillColor = ~pal2(df_map1()[["indicator"]]),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    label = labels2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        )  %>%
        
        addLegendImage(images = paste0("./WWW/", image, ".png"), 
                       labels = "", 
                       position = "bottomleft",
                       title = title, 
                       width = 350,
                       height = 330)
    }
    
    
    
    
    
    
    output$map1 <- renderLeaflet(map2)
    
  })
  
  
  
  
  #####################################################
  ##### 2 - Onglet enjeux d'acceptabilité sociale #####
  #####################################################
  
  
  
  map3 <- reactive({
    
    bins <- switch(input$choix_indic_accept, 
                   "1" = c(-77, -20, -5, 5, 15, 30, 50), 
                   "2" = c(-121, -20, -5, 5, 15, 30, 47))
    
    domain <- switch(input$choix_indic_accept, 
                     "1" = df_wtp$acceptabilite_extinction1, 
                     "2" = df_wtp$acceptabilite_extinction2)
    
    pal2 <- colorBin(c("#d73027", "#fdae61", "#fffdbf", "#abd9e9", "#4575b4", "#323695"), 
                     bins = bins, 
                     na.color = "white")
    pal3 <- colorFactor(c("#d73027", "#fdae61", "#fffdbf", "#abd9e9", "#4575b4", "#323695"), 
                        domain =  domain)
    
    labels <- switch(input$choix_indic_accept, 
                     "1" = sprintf(
                       "<strong>%s</strong><br/>Acceptation score: %g (%s)",
                       paste0(df_wtp[["LIBCOM"]], ", ", df_wtp[["LIBIRIS"]]), round(df_wtp$wtp_extinction1, 1), 
                       df_wtp$acceptabilite_extinction1
                     ) %>% lapply(htmltools::HTML), 
                     
                     "2" = sprintf(
                       "<strong>%s</strong><br/>Acceptation score: %g (%s)",
                       paste0(df_wtp[["LIBCOM"]], ", ", df_wtp[["LIBIRIS"]]), round(df_wtp$wtp_extinction2, 1), 
                       df_wtp$acceptabilite_extinction2
                     ) %>% lapply(htmltools::HTML))
    
    chloropeth <- switch(input$choix_indic_accept, 
                         "1" = df_wtp$wtp_extinction1, 
                         "2" = df_wtp$wtp_extinction2)
    
    title <- switch(input$choix_indic_accept, 
                    "1" = "Social acceptation for public</br>lighting extinction from 1 a.m. to 5 a.m.", 
                    "2" = "Social acceptation for public</br>lighting extinction from 11 p.m. to 6 a.m.")
    
    map2 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      
      addPolygons(data = df_mmm_shp,
                  color = "black", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0,
                  fillColor = "white",
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "black", weight = 2,
                  #                                     bringToFront = TRUE)
      ) %>%
      
      addPolygons(data = df_wtp, 
                  color = "darkgrey", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.9,
                  fillColor = ~pal2(chloropeth),
                  highlightOptions = highlightOptions(color = "black", weight = 2,
                                                      bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      addLegend("bottomleft", pal = pal3, values = domain,
                title = title, 
                
                opacity = 1
      )
    
    
  })
  
  output$map3 <- renderLeaflet(map3())
  
  
  
  ########################################
  ##### 3 - Onglet carte de synthese #####
  ########################################
  
  map4 <- reactive({ 
    
    pal44 <- colorFactor(palette = c( "#94bdd4", "#51a5d6", "#0088d9", 
                                      "#0285a1", 
                                      "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                      "#d9be01", "#9aab00", "#539600", "#007b00"),
                         domain = bivariate_prio2$indicator_extinction1_44,
                         na.color = "white")
    
    labels <- sprintf(
      "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of functionality <br/> <strong>%s</strong> groups of impacted species",
      paste0(bivariate_prio2[["LIBCOM"]], ", ", bivariate_prio2[["LIBIRIS"]]), 
      round(bivariate_prio2$wtp_extinction1, 1),
      bivariate_prio2$acceptabilite_extinction1,
      bivariate_prio2$lose_functionality, 
      bivariate_prio2$impacted_species
    ) %>% lapply(htmltools::HTML)
    
    map4 <- leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>%
      
      addPolygons(data = df_mmm_shp,
                  color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, 
                  fillColor = "white", 
                  fillOpacity = 0.2#,
                  # highlightOptions = highlightOptions(color = "white", weight = 2,
                  #                                     bringToFront = TRUE) 
      ) %>%
      
      addPolygons(data = bivariate_prio2,
                  color = "#ffffff00", 
                  fillColor = ~pal44(bivariate_prio2$indicator_extinction1_44),
                  highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                      bringToFront = TRUE),
                  opacity = 1.0,  
                  fillOpacity = 0.9,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      
      
      addLegendImage(images =  "./WWW/legend_map_4x4.png", 
                     labels = "", 
                     position = "bottomleft",
                     width = 400,
                     height = 400)
    
  })
  
  output$map4 <- renderLeaflet(map4())
  
  
  
  
  observeEvent(input$go3, {
    
    
    if(input$priority_synthese == "2"){
      
      pal44 <- switch(input$choix_indic_accept_synthese, 
                      "1" = colorFactor(palette = c( "#94bdd4", "#51a5d6", "#0088d9", 
                                                     "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = bivariate_prio2$indicator_extinction1_44,
                                        na.color = "white"), 
                      
                      "2" = colorFactor(palette = c( "#d3d3d3", "#94bdd4", "#51a5d6", "#0088d9", 
                                                     "#53a19f", "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c", 
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = bivariate_prio2$indicator_extinction2_44,
                                        na.color = "white")) 
      
      wtp <- switch(input$choix_indic_accept_synthese, 
                    "1" = bivariate_prio2$wtp_extinction1, 
                    "2" = bivariate_prio2$wtp_extinction2)
      
      indicator <- switch(input$choix_indic_accept_synthese, 
                          "1" = bivariate_prio2$indicator_extinction1_44, 
                          "2" = bivariate_prio2$indicator_extinction2_44)
      
      
      
      labels <- switch(input$choix_indic_accept_synthese, 
                       "1" = sprintf(
                         "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of functionality <br/> <strong>%s</strong> groups of impacted species",
                         paste0(bivariate_prio2[["LIBCOM"]], ", ", bivariate_prio2[["LIBIRIS"]]), 
                         round(wtp, 1),
                         bivariate_prio2$acceptabilite_extinction1,
                         bivariate_prio2$lose_functionality, 
                         bivariate_prio2$impacted_species
                       ) %>% lapply(htmltools::HTML), 
                       "2" = sprintf(
                         "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of functionality <br/> <strong>%s</strong> groups of impacted species",
                         paste0(bivariate_prio2[["LIBCOM"]], ", ", bivariate_prio2[["LIBIRIS"]]), 
                         round(wtp, 1),
                         bivariate_prio2$acceptabilite_extinction2,
                         bivariate_prio2$lose_functionality, 
                         bivariate_prio2$impacted_species
                       ) %>% lapply(htmltools::HTML)
                       
                       )
      
      
      map4 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(data = df_mmm_shp,
                    color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillColor = "white", 
                    fillOpacity = 0.2#,
                    # highlightOptions = highlightOptions(color = "white", weight = 2,
                    #                                     bringToFront = TRUE) 
        ) %>%
        
        addPolygons(data = bivariate_prio2,
                    color = "#ffffff00", 
                    fillColor = ~pal44(indicator),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    opacity = 1.0,  
                    fillOpacity = 0.9,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        
        addLegendImage(images =  "./WWW/legend_map_4x4.png", 
                       labels = "", 
                       position = "bottomleft",
                       width = 400,
                       height = 400)
      
      
    }
    
    else{
      pal44 <- switch(input$choix_indic_accept_synthese, 
                      "1" = colorFactor(palette = c( "#51a5d6", 
                                                     "#95b89e", "#53a19f", "#0285a1", 
                                                     "#d7c659", "#99b35a", "#529c5a", "#03815c",
                                                     "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = bivariate_prio3$indicator_extinction1_44,
                                        na.color = "white"), 
                      
                      "2" = colorFactor(palette = c(  "#d3d3d3",  
                                                      "#d5ce9c", "#95b89e", "#53a19f", "#0285a1", 
                                                      "#d7c659", "#99b35a", "#529c5a", "#03815c",
                                                      "#d9be01", "#9aab00", "#539600", "#007b00"),
                                        domain = bivariate_prio3$indicator_extinction2_44,
                                        na.color = "white")) 
      
      wtp <- switch(input$choix_indic_accept_synthese, 
                    "1" = bivariate_prio3$wtp_extinction1, 
                    "2" = bivariate_prio3$wtp_extinction2)
      
      
      indicator <- switch(input$choix_indic_accept_synthese, 
                          "1" = bivariate_prio3$indicator_extinction1_44, 
                          "2" = bivariate_prio3$indicator_extinction2_44)
      
      
      labels <- switch(input$choix_indic_accept_synthese,
                      "1" = sprintf(
                        "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of functionality <br/> <strong>%s</strong> groups of impacted species",
                        paste0(bivariate_prio3[["LIBCOM"]], ", ", bivariate_prio3[["LIBIRIS"]]), 
                        round(wtp, 1),
                        bivariate_prio3$acceptabilite_extinction1,
                        bivariate_prio3$lose_functionality, 
                        bivariate_prio3$impacted_species
                      ) %>% lapply(htmltools::HTML), 
                      "2" = sprintf(
                        "<strong>%s</strong><br/>Acceptation score : %g (%s)<br/>%s loss of functionality <br/> <strong>%s</strong> groups of impacted species",
                        paste0(bivariate_prio3[["LIBCOM"]], ", ", bivariate_prio3[["LIBIRIS"]]), 
                        round(wtp, 1),
                        bivariate_prio3$acceptabilite_extinction2,
                        bivariate_prio3$lose_functionality, 
                        bivariate_prio3$impacted_species
                      ) %>% lapply(htmltools::HTML))
                      
      
      map4 <- leaflet() %>%
        
        addProviderTiles(providers$CartoDB.Positron) %>%
        
        addPolygons(data = df_mmm_shp,
                    color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, 
                    fillColor = "white", 
                    fillOpacity = 0.2
        ) %>%
        
        addPolygons(data = bivariate_prio3,
                    color = "#ffffff00", 
                    fillColor = ~pal44(indicator),
                    highlightOptions = highlightOptions(color = "grey", weight = 1,
                                                        bringToFront = TRUE),
                    opacity = 1.0,  
                    fillOpacity = 0.9,
                    label = labels,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>%
        
        
        addLegendImage(images =  "./WWW/legend_map_4x4.png", 
                       labels = "", 
                       position = "bottomleft",
                       width = 400,
                       height = 400)
      
      
    }
    
    
    
    
    
    
    
    output$map4 <- renderLeaflet(map4)
    
  })
  
  
  
}


shinyApp(ui, server)


















