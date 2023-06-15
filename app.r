library(dplyr)
library(ggplot2) 
library(ggpubr) 
library(reshape2) 
library(tibble)
library(sf) 
library(tmap) 
library(broom)
library(plotly) 
library(magrittr) 
library(shiny) 
library(rsconnect) 
library(devtools)
library(XML)
library(leaflet)
library(DT)
library(bslib)
library(RColorBrewer)
library(reshape2)

# Import data ------------------------------------------------------------------

global_power_plant_database <- read.csv("global_power_plant_database.csv")
power <- read.csv("electricity-prod-source-stacked.csv", fileEncoding = "UTF-8")

pal <- colorFactor(palette = c("yellow","orange","#ffffaa","#FC0300", "blue",
                               "purple", "#777777", "pink", "#00FFFF", "green", 
                               "#836931","#00A693","#FF00FF","#ACE1AF","#CF2929"),
                   levels = c("Solar","Gas","Other","Oil","Hydro", "Nuclear"
                              ,"Coal","Waste","Wind","Biomass","Petcoke", 
                              "Wave and Tidal","Storage","Cogeneration","Geothermal"))

# Theme setting ----------------------------------------------------------------

custom_theme <- bs_theme(
  bg = "#101010", 
  fg = "#FDF7F7", 
  primary = "#00FFFF", 
  base_font = font_google("Prompt"),
  code_font = font_google("JetBrains Mono"))

# Navigation bar ---------------------------------------------------------------

ui <- fluidPage(
  theme = custom_theme,
                navbarPage(span("POWER PLANTS", style="color: #00FFFF; 
                                font-size: 40px"), theme = custom_theme,
                           
                           # Country panel -------------------------------------------------------------------------------
                           
                           tabPanel(h4("Country", style="margin: 0px; padding-left: 10px;"), 
                                    h6(strong("Authors: "), "Aleksandra Pełka, Łukasz Kozłowski", 
                                       style="margin: 0px; float: right;"),
                                    titlePanel(tags$b("Geoinformatics application - Power Plants")),
                                    tags$p("Location of power plants, powered by energy from various sources, 
                                           in a selected country and in the world"),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        checkboxGroupInput(inputId = "primary_fuel", 
                                                           label="Select primary fuel:",
                                                           choices = sort(unique(global_power_plant_database$primary_fuel)), 
                                                           selected=unique(global_power_plant_database$primary_fuel), 
                                                           inline=F, width="400px"),
                                        actionButton("goButton", "Submit"), tags$p(),
                                        selectInput(inputId = "country", 
                                                    label="Select country:",
                                                    choices = unique(global_power_plant_database$country_long), 
                                                    selected="Brazil"),
                                        sliderInput(inputId = "range", 
                                                    label="Select range:", 
                                                    min = 5, max = 100, value = 5, step = 5),
                                        actionButton("submit", label = "Submit"),
                                      ),
                                      
                                      mainPanel(
                                        tabsetPanel(
                                          tabPanel("Country Map",
                                                   tags$div(tags$h5("Map of the distribution of power plants in the selected country"), 
                                                            style="text-align: center; padding: 20px 0px;"),
                                                   value = 1, leafletOutput("map1", height = "600px")), 
                                          tabPanel("Electricity Mix",value=1,
                                                   tags$div(tags$h5("Graph showing power plant capacity over the years in a selected country."), 
                                                            style="text-align: center; padding: 20px 0px;"),
                                                   plotlyOutput("wykres", height = "550px")),
                                          tabPanel("Table",value=2, tags$div(tags$h5("Power plants in the selected country sorted in descending order by capacity variable"), 
                                                                             style="text-align: center; padding: 20px 0px;"),
                                                   DT::dataTableOutput("mytable", height = "600px"))
                                        )
                                      )
                                    )
                           ),
                           
                           
                           # Statistics panel -------------------------------------------------------------------------------
                           
                           tabPanel(h4("Statistics", style="margin: 0px"),
                                    sidebarLayout(
                                      sidebarPanel(width = 3, 
                                        selectInput(inputId = "country_stats", 
                                                    label="Select country:", 
                                                    choices = unique(global_power_plant_database$country_long), 
                                                    selected="Brazil"),
                                        selectInput(inputId = "limit", 
                                                    label="Table limit:", 
                                                    choices = seq(1:15), 
                                                    selected=1),
                                        actionButton("wyswietl", "Show Table", style="margin-bottom: 15px"), 

                                        tabsetPanel(id = "subTab2",
                                          tabPanel("Graph Country", value=1, 
                                                 tags$div(tags$h4("Power plants percentage in selected country"), 
                                                          style="text-align: center; padding: 20px 0px;"),
                                                 plotlyOutput("wykres_kolowy")),
                                          tabPanel("Graph World", value=2, 
                                                 tags$div(tags$h4("Power plants percentage in selected world"), 
                                                          style="text-align: center; padding: 20px 0px;"),
                                                 plotlyOutput("wykres_kolowy2")),
                                          tabPanel("Table Country", value=3,  
                                                   tags$div(tags$h4("Number of power plants by primary fuels"), 
                                                            style="text-align: center; padding: 20px 0px;"),
                                                 DT::dataTableOutput("mytable3")),
                                          tabPanel("Table World", value=4,  
                                                   tags$div(tags$h4("Number of power plants by primary fuels"), 
                                                            style="text-align: center; padding: 20px 0px;"),
                                                 DT::dataTableOutput("mytable4"))
                                        )
                                       ),
                                      
                                      mainPanel(width = 9, fluidRow(
                                          div(h6(strong(textOutput("typ"))), h1(textOutput("elektrownia"), div(textOutput("capacity"), style="color: purple; float: left;"), div(" MW", style="color: purple;")), p(textOutput("panstwo"),), 
                                              style="padding-left: 20px; letter-spacing: 4px; color: #333333; background-color: #00FFFF; border: 1px solid purple; width: 440px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Biomass", h4("🌱 528"), style="font-size: 14px")), h5("Zolling power station"),  p("Germany", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #22FF22; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Coal", h4("🌿 7000"), style="font-size: 14px")), h5("East Hope Metals Wucaiwan power station", style="font-size: 17px"), p("China", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #777777; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Cogeneration 🛢", h4("🛢 1404"), style="font-size: 14px")), h5("Grain CHP"), p("United Kingdom", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #ACE1AF; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Gas", h4("💥 8865"), style="font-size: 14px")), h5("Surgutskaya GRES-2"), p("Russia", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid orange; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Geothermal", h4("🔥 1163"), style="font-size: 14px")), h5("Geysers Unit 5-20"), p("United States of America", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #CF2929; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Hydro", h4("💧 22500"), style="font-size: 14px")), h5("Three Gorges Dam"), p("China", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid blue; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Nuclear️", h4("☣️️ 8212 "), style="font-size: 14px")), h5("Kashiwazaki Kariwa"), p("Japan", style="font-size: 12px")
                                              , style="color: #cccccc; border: 1px solid purple; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Oil️", h4("⛽ 6794 "), style="font-size: 14px")), h5("SHAIBA (SEC)"), p("Saudi Arabia", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #FC0300; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Other️", h4("⚗️ 845"), style="font-size: 14px")), h5("PARANA"), p("Argentina", style="font-size: 12px"),
                                              style="color: #cccccc; border: 1px solid #ffffaa; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Petcoke", h4("🕳 1707"), style="font-size: 14px")), h5("Brame Energy Center"), p("United States of America", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #836931; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Solar️", h4("☀  1021	" ), style="font-size: 14px")), h5("Miraah CSP"), p("Oman", style="font-size: 12px"),
                                              style="color: #cccccc; border: 1px solid yellow; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Storage", h4("🧺 400"), style="font-size: 14px")), h5("EFDA JJET Fusion Flywheel"), p("United Kingdom", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #FF00FF; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Waste", h4("💩 161"), style="font-size: 14px")), h5("Covington Facility"), p("United States of America", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid pink; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Wave & Tidal", h4("🌊 254"), style="font-size: 14px")), h5("Sihwa Lake Tidal Power Plant"), p("South Korea", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #00A693; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;"),
                                          div(h6(strong("Wind", h4("💨 6000"), style="font-size: 14px")), h5("Gansu Wind Farm"), p("China", style="font-size: 12px"), 
                                              style="color: #cccccc; border: 1px solid #00FFFF; width: 140px; margin: 5px; display: inline-block; border-radius: 10px;")
                                          ),
                                          tags$br(),
                                          tabsetPanel(id = "subTab3",
                                            tabPanel("Histogram Country", value=1, plotlyOutput("histogram")),
                                            tabPanel("Histogram World", value=2, plotlyOutput("histogram2")),
                                          )
                                       )
                                    )
                              ),
                           
                           # World panel -------------------------------------------------------------------------------
                           
                           tabPanel(h4("World", style="margin: 0px"), 
                                    h6(strong("Authors: "), "Aleksandra Pełka, Łukasz Kozłowski", 
                                       style="margin: 0px; float: right;"),
                                    titlePanel(tags$b("Geoinformatics application - Power Plants")),
                                    tags$p("Location of power plants, powered by energy from various sources, in a selected country and in the world"),
                                    
                                    sidebarLayout(
                                      sidebarPanel(
                                        tabsetPanel(id = "subTab1",
                                          tabPanel("Graph", value=1, plotlyOutput("wykres_pojedynczy")),
                                          tabPanel("Table", value=2, 
                                                   tags$div(tags$h4("Top 5 power plants by primary fuel"), 
                                                            style="text-align: center; padding: 20px 0px;"),
                                                   DT::dataTableOutput("mytable2")))
                                      ),
                                      mainPanel(
                                        sidebarPanel(
                                          radioButtons(
                                            inputId = "primary_fuel_radio", label = "Select primary fuel:",
                                            choices = sort(unique(global_power_plant_database$primary_fuel)),
                                            selected = NULL,  inline=T), width = "700px", 
                                          leafletOutput("map3", height = "500px"),
                                          mainPanel() )
                                      )
                                    )
                                  )
                )
)



# Server -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Setting the same country when switching between tabs -----------------------
  
  observeEvent(input[["country"]],
               {
                 updateSelectInput(session = session,
                                   inputId = "country_stats",
                                   selected = input[["country"]])
               })
  
  observeEvent(input[["country_stats"]],
               {
                 updateSelectInput(session = session,
                                   inputId = "country",
                                   selected = input[["country_stats"]])
               })
  

  # Preparing data for display  ------------------------------------------------
  
  g_input <- reactive({
    subset(global_power_plant_database, global_power_plant_database$country_long %in% input$country)
  }) 
  
  wykres_input <- reactive({
    pl_power <- power[power$Entity == input$country, ] 
    pl_power <- pl_power[pl_power$Year > 1984, ]
    colnames(pl_power)[4:12] <- c("Other","Biomass", "Solar", "Wind", "Hydro", "Nuclear", "Oil", "Gas", "Coal")
    pl_power$Total <- rowSums(pl_power[,4:12])
    pl_power[,4:12] <- pl_power[,4:12]/pl_power$Total
    pl_power <- pl_power[,3:12]
    pl_power2 <- melt(pl_power[,1:10], id.vars = "Year")
  }) 
  
  wykres_input2 <- reactive({
    power2 <- power
    power2 <- power2[power2$Year > 1984, ] # brak danych przed 1985
    power2 <- power2[power2$Year < 2021, ]
    colnames(power2)[4:12] <- c("Other","Biomass", "Solar", "Wind", "Hydro", "Nuclear", "Oil", "Gas", "Coal")
    power2 <- power2 %>% group_by(Year) %>%
      summarise(Other = mean(Other,na.rm=TRUE),
                Biomass = mean(Biomass,na.rm=TRUE),
                Solar = mean(Solar,na.rm=TRUE),
                Wind = mean(Wind, na.rm=TRUE),
                Hydro = mean(Hydro, na.rm=TRUE),
                Nuclear = mean(Nuclear, na.rm=TRUE),
                Oil = mean(Oil, na.rm=TRUE),
                Gas = mean(Gas, na.rm=TRUE),
                Coal = mean(Coal, na.rm=TRUE),
                )
    power2[,c("Year",input$primary_fuel_radio)]
  })
  
  # Creating a list for observeEvent to react to more variables ----------------
  
  toListen <- reactive({
    list(input$primary_fuel, input$country)
  })
  toListen2 <- reactive({
    list(input$country, input$goButton)
  })
  
  
  # Displaying maps in the Country panel ---------------------------------------
  
  observeEvent( toListen2(),{
    g <- g_input()
    
    output$map1<-  renderLeaflet(      
      leaflet(data = isolate(g[g$primary_fuel %in% input$primary_fuel,])) %>% 
        addTiles() %>%
        addCircleMarkers(
          color = ~pal(primary_fuel),
          stroke = FALSE, fillOpacity = 0.5,
          radius = ~ (sqrt(capacity_mw)/3),
          popup =  ~ paste("<h5><b>Name: </b>", name, "<br></h5><hr><b>Primary fuel: </b>", primary_fuel, 
                           "<br><b>Capacity: </b>", capacity_mw, "MW", "<br><b>Source: </b>", source, 
                           "<br><b>Commissioning year: </b>", commissioning_year),
          label = ~as.character(name))  %>%
          # background maps
          addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter (default)") %>%
          addProviderTiles(providers$CartoDB.Voyager, group = "CartoDB.Voyager") %>%
          addProviderTiles(providers$Stamen.TonerBackground, group = "Stamen.TonerBackground") %>%
          addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen.TerrainBackground") %>%
          # control panel
          addLayersControl(
            baseGroups = c("CartoDB.DarkMatter (default)", "CartoDB.Voyager", "Stamen.TonerBackground", "Stamen.TerrainBackground")) %>%
          addLegend(position = 'topright',
                    colors = ~pal(levels(as.factor(primary_fuel))), 
                    labels = ~levels(as.factor(primary_fuel))) %>%
          addEasyButton(easyButton(
            icon="fa-globe", title="Zoom to Level 1",
            onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
          addMiniMap(tiles = "CartoDB.DarkMatter")
  )})
  
  # Displaying the table in the Country panel ----------------------------------
  
  tab_data <- reactive({
    g <- g_input()
    g <- g[order(g$capacity_mw, decreasing = TRUE), ]
    global <- g[,c(2,3,5,8 )]
    
    c<-count(global)
    global$id <- seq(1:c$n)
    data_globa <- global[,c(5,1,2,3,4)]
  })
  
  v <- reactiveValues(
    liczba = 5
   )
  
  observeEvent(input$submit, {
    v$liczba <- input$range
  })
  output$mytable <- DT::renderDataTable(tab_data()[1:v$liczba,], 
                                        options = list(lengthChange = TRUE, dom = 'tp',
                                                       pageLength = 13, scrollCollapse=TRUE, scrollY = TRUE), 
                                        rownames = FALSE)
  
  
  observeEvent( input$country, {
    g <- g_input()
    
    # Finding the largest power plant in the country (Statistics panel) --------
    
    power_wykres <- wykres_input()
    sortCapacity <- arrange(g, desc(capacity_mw))
    maxCapacity <- sortCapacity$capacity_mw[1]
    maxType <- sortCapacity$primary_fuel[1]
    maxName <- sortCapacity$name[1]
    
    output$panstwo <- renderText({ input$country })
    output$typ <- renderText({ maxType })
    output$capacity <- renderText({ maxCapacity })
    output$elektrownia <- renderText({ maxName })
    
    # Displaying a histogram in the Statistics panel ---------------------------
    
    output$histogram <- renderPlotly( {ggplotly(
      ggplot(data = g, aes(x=primary_fuel)) 
      + ggtitle("Histogram shows the number of power plants grouped by primary fuel for the selected country")
      + xlab("primary fuel") + ylab("count")
      + geom_bar(fill="#00FFFF") 
      + theme(legend.title = element_text(size = 12, face = "bold", colour = "#FDF7F7"),
              legend.text = element_text(size = 10, colour = "#FDF7F7"),
              plot.title = element_text(size= 14, face = "bold", colour = "#FDF7F7"),
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = "#333333", colour = "#333333"),
              panel.grid.major = element_line(colour = "black"),
              axis.title.x = element_text(size= 10, face = "bold", colour = "#333333"),
              axis.title.y = element_text(size= 10, face = "bold", colour = "#333333")
              )
      )})
    
    output$histogram2 <- renderPlotly( {ggplotly(
      ggplot(data = global_power_plant_database, aes(x=primary_fuel)) 
      + ggtitle("Histogram shows the number of power plants grouped by primary fuel in the world")
      + xlab("primary fuel") + ylab("count")
      + geom_bar(fill="#00FFFF") 
      + theme(legend.title = element_text(size = 12, face = "bold", colour = "#FDF7F7"),
              legend.text = element_text(size = 10, colour = "#FDF7F7"),
              plot.title = element_text(size= 14, face = "bold", colour = "#FDF7F7"),
              plot.background = element_rect(fill = "black"),
              panel.background = element_rect(fill = "#333333", colour = "#333333"),
              panel.grid.major = element_line(colour = "black"),
              axis.title.x = element_text(size= 10, face = "bold", colour = "#333333"),
              axis.title.y = element_text(size= 10, face = "bold", colour = "#333333")
        )
    )})
    
    # Delay displaying data with eventReactive ---------------------------------
    
    evenRect <- eventReactive(input$wyswietl, {
      g <- g_input()
      z <- g %>% group_by(primary_fuel) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
      z <- z %>% arrange(desc(total_count))
      z <- head(z,as.numeric(input$limit))
    })
    
    evenRect2 <- eventReactive(input$wyswietl, {
      g <- global_power_plant_database
      z <- g %>% group_by(primary_fuel) %>% 
        summarise(total_count=n(),
                  .groups = 'drop')
      z <- z %>% arrange(desc(total_count))
      z <- head(z,as.numeric(input$limit))
    })
    
    # Displaying a table in the Statistics panel -------------------------------
    
    output$mytable3 <- DT::renderDataTable( evenRect(),  
                                            options = list(lengthChange = TRUE, dom = 'tp', pageLength = 5,
                                            scrollCollapse=TRUE, scrollY = TRUE), rownames = FALSE)
    output$mytable4 <- DT::renderDataTable( evenRect2(),  
                                            options = list(lengthChange = TRUE, dom = 'tp', pageLength = 5,
                                            scrollCollapse=TRUE, scrollY = TRUE), rownames = FALSE)
    
    # Displaying a pie chart in the Statistics panel ---------------------------
    
    palette = c("#26655f","#2b8980", "#2faea3", "#30d5c8", "#8ce4da", "#abebe3", "#c8f2ec", "#e4f9f6",  "#C6FFFF", "#AAFFFF", "#8DFFFF", "#71FFFF", "#55FFFF", "#38FFFF",  "#00FFFF",  "#11FFFF", "#22FFFF")
      
    output$wykres_kolowy <- renderPlotly({plot_ly(
      data = g, labels = g$primary_fuel, values = g$capacity_mw, 
      marker = list(colors = palette),
      type = 'pie') %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               paper_bgcolor='black')})
    
    output$wykres_kolowy2<-renderPlotly({plot_ly(
      data = global_power_plant_database, labels = global_power_plant_database$primary_fuel, 
      values = global_power_plant_database$capacity_mw, 
      marker = list(colors = palette),
      type = 'pie') %>% 
        layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          paper_bgcolor='black')})
    
    # Displaying the chart in the Country panel --------------------------------
    
    output$wykres <- renderPlotly( {
        poczatek <-"Electricity Mix in:"
        srodek <- as.character(input$country)
        koniec <- "(1985-2021) "
        ggplotly(ggplot() 
                 + geom_line(power_wykres, mapping = aes(x = Year, y = value , colour = variable), linewidth=1)  
                 + ggtitle(paste(poczatek,srodek,koniec)) +   xlab("Year") + ylab("% of the total production") 
                 + theme_minimal() 
                 + theme(legend.title = element_text(size = 12, face = "bold", colour = "#FDF7F7"),
                         legend.text = element_text(size = 10, colour = "#FDF7F7"),
                         plot.title = element_text(size= 14, face = "bold", colour = "#FDF7F7"),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill = "#333333", colour = "#222222"),
                         panel.grid.major = element_line(colour = "black"),
                         axis.title.x = element_text(color="#333333", size=10, face="bold"),
                         axis.title.y = element_text(color="#333333", size=10, face="bold"))
        )})
  })
  
  # Displaying the map in the World panel --------------------------------------
  
  observe({
    global <- global_power_plant_database
    output$map3 <- renderLeaflet(
      leaflet(data = global[global$primary_fuel %in% input$primary_fuel_radio,]) %>% 
      addTiles() %>%
      addCircleMarkers(color = ~pal(primary_fuel), stroke = FALSE, fillOpacity = 0.5,
                       radius = ~ (sqrt(capacity_mw)/3),
                       popup = ~ paste("<h5><b>Name: </b>", name, 
                                       "<br></h5><hr><b>Primary fuel: </b>", primary_fuel, 
                                       "<br><b>Capacity: </b>", capacity_mw, 
                                       "MW", "<br><b>Source: </b>", source, 
                                       "<br><b>Commissioning year: </b>", commissioning_year),
                       label = ~as.character(name))   %>%
      # background maps
      addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter (default)") %>%
      addProviderTiles(providers$CartoDB.Voyager, group = "CartoDB.Voyager") %>%
      addProviderTiles(providers$Stamen.TonerBackground, group = "Stamen.TonerBackground") %>%
      addProviderTiles(providers$Stamen.TerrainBackground, group = "Stamen.TerrainBackground") %>%
      # control panel
      addLayersControl(
        baseGroups = c("CartoDB.DarkMatter (default)", "CartoDB.Voyager", "Stamen.TonerBackground", "Stamen.TerrainBackground")) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addMiniMap(tiles = "CartoDB.DarkMatter"))
    
    g <- global[global$primary_fuel %in% input$primary_fuel_radio,]
    g <- g[,c(2,3,5,8 )]
    g <- g[order(g$capacity_mw, decreasing = TRUE), ]
    
    # Displaying a table in the World panel ------------------------------------
    
    output$mytable2 <- DT::renderDataTable(g[1:5,], 
                                           options = list(lengthChange = FALSE,dom = 't', scrollCollapse=TRUE), 
                                           rownames = FALSE )
    
    # Displaying the chart in the World panel ----------------------------------
    
    output$wykres_pojedynczy <- renderPlotly({
      
        power_wykres2 <- wykres_input2()
        poczatek <-"Mean capacity of"
        srodek <- as.character(g$primary_fuel[1])
        koniec <- "in world (1985-2021) "
        nazwa <- colnames(power_wykres2[2])
        colnames(power_wykres2)[1:2] <- c("Year","Value")
        
        ggplotly(ggplot() 
                 + geom_line(power_wykres2, mapping = aes(x = Year, y = Value , colour = nazwa), size=2)  
                 + scale_color_manual(values="#00FFFF") 
                 + xlab("Year") + ylab("mean capacity") 
                 + ggtitle(paste(poczatek,srodek,koniec)) 
                 + theme_minimal() 
                 + theme(legend.title = element_text(size = 12, face = "bold", colour = "#FDF7F7"),
                         legend.text = element_text(size = 10, colour = "#FDF7F7"),
                         plot.title = element_text(size= 14, face = "bold", colour = "#FDF7F7"),
                         plot.background = element_rect(fill = "black"),
                         panel.background = element_rect(fill = "#444444", colour = "#222222"),
                         panel.grid.major = element_line(colour = "black"),
                         axis.title.x = element_text(color="#444444", size=10, face="bold"),
                         axis.title.y = element_text(color="#444444", size=10, face="bold"))
        )
      })
  })

  # Displaying information in the console --------------------------------------
  
  observeEvent(input$primary_fuel, print(paste0("the number of active primary fuel=", 
                                                length(input$primary_fuel))))
}


shinyApp(ui = ui, server = server)