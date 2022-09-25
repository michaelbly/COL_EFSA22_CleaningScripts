#### 1 LOAD PACKAGES ###########################################################

library(dplyr)                                                                    # data wrangling work horse
library(tidyr)                                                                    # additional data wrangling
library(tidytidbits)                                                              # for conditional piping
library(stringr)                                                                  # to do some operations with strings
library(shiny)                                                                    # for shiny app functions
library(shinyWidgets)                                                             # additional UI options for shiny
library(shinythemes)                                                              # to apply a theme to the shiny app
library(sf)                                                                       # to read/manipulate shapefiles
library(leaflet)                                                                  # to display maps
library(leaflet.extras)                                                           # additional options for leaflet
library(highcharter)                                                              # to build plots
library(DT)                                                                       # for datatable in data explorer
library(kableExtra)                                                               # to make tables
library(scales)                                                                   # to define percentages
library(shinydashboard)
library(leaflet.extras)
      

#### 2 LOAD DATA ###############################################################
adm <- st_read("try_shiny/gis/shapefiles_unzipped/col_admbnda_adm1_mgn_20200416.shp") %>% 
  st_transform(crs = 4326)
adm$ADM1_ES <- accented_letters(adm$ADM1_ES)
adm$ADM1_ES <- to_alphanumeric_lowercase(adm$ADM1_ES)
adm$ADM1_ES
names(adm)[names(adm) == "ADM1_ES"] <- "departamento"
adm$departamento[adm$departamento == "bogota__d_c_"] <- "bogota_dc"



df <- read.csv("try_shiny/data/efsa_data_final.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")
overall_merged <- read.csv("try_shiny/data/overall_merged_map.csv", sep = ","
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")


df.map <- left_join(adm, overall_merged, by="departamento") %>%
  mutate(pct.done=pmin(Aceptado/Tamano_Muestra*100, 100))

cols      <- c("rgb(238,88,89)",   "rgb(88,88,90)",    "rgb(165,201,161)",        # define color palette for plot lines
               "rgb(86,179,205)",  "rgb(246,158,97)",  "rgb(255,246,122)",
               "rgb(210,203,184)", "rgb(247,172,172)", "rgb(172,172,173)",
               "rgb(210,228,208)", "rgb(171,217,230)", "rgb(251,207,176)",
               "rgb(255,251,189)", "rgb(233,229,220)")

# reshape dataset to only include count surveys total, and count for vocacion de permanencia and retornados
df$count <- 1
df$date_assessment <- substring(df$date_assessment, 6)

home_popgroup <- df %>% select(date_assessment, pop_group, count) %>%
                group_by(date_assessment, pop_group) %>%
                summarise(count = sum(count)) %>%
                filter(pop_group %in% c("retornado", "vocaci_n_de_permanencia")) %>%
                arrange(date_assessment)

home_total <- df %>% select(date_assessment, pop_group, count) %>%
                     group_by(date_assessment) %>%
                     summarise(count = sum(count)) %>%
                     mutate(pop_group = "total") 

home_merged <- home_total %>%
                     rbind(home_popgroup)


# generate map
pal.strata <- colorNumeric(c("red", "yellow", "green"), 0:100)
pal <- colorNumeric(palette = "RdYIGn", 0:100)


labels <- paste(
  "<strong>", df.map$departamento,
  "</strong><br>% Completado:", round(df.map$pct.done,  1),
  "</strong><br># Restante:", df.map$Restante, 
  "</strong><br># Marcada Tiempo:", df.map$Marcada_Tiempo) %>%
  lapply(htmltools::HTML)



# create overview table
start_date <- "05.08.2022"
municipios_covered    <- n_distinct(df$municipio, na.rm = FALSE)
departamentos_covered <- n_distinct(df$departamento, na.rm = FALSE)
nr_enumerators <- n_distinct(df$entrevistador, na.rm = FALSE)
median_duration <- paste0(round(mean(df$duracion/60),0), "min")

overview_round       <- data.frame(figure = c("Start Date", "Municipalities Covered", "Departamentos Covered", "# of Enumerators", "Median Interview Duration"),
                                   value  = c(start_date, municipios_covered, departamentos_covered, nr_enumerators, median_duration)
)

table_round <- overview_round %>%                                                                         # style overview table
  kbl(escape = F, format.args = list(big.mark = ","), align = "lr", col.names = NULL) %>%
  column_spec(1, width = "12em") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), fixed_thead = T, full_width = T) %>%
  row_spec(1,   extra_css = "font-size: 11.5px; border-top: 2px solid gainsboro") %>%
  row_spec(2:5, extra_css = "font-size: 11.5px;")

df$departamento <- as.character(as.factor(df$departamento))




#### 6 UI ######################################################################

ui <- bootstrapPage(

    navbarPage("EFSA22 | Data Collection",                                                # define dashboard title
               theme = shinytheme("flatly"),                                                             # set theme
               
               #### * 6.1 Home ######################################################################
               
               tabPanel("Overview",  
                        tags$head(
                          tags$style(HTML(".leaflet-container { background: #FFFFFF; }"))
                        ),            # define panel title

                        leafletOutput("map_home_gaggi", width = "100%", height = 1000), 
                        tags$style(type = "text/css", ".container-fluid {padding-left:0px;
                    padding-right:0px;}"),
                        tags$style(type = "text/css", ".navbar {margin-bottom: .5px;}"),
                        tags$style(type = "text/css", ".container-fluid .navbar-header 
                    .navbar-brand {margin-left: 0px;}"),# display background map
                        
                            absolutePanel(                                                                # define introduction box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = 70, left = "20", right = "auto", bottom = "auto", width = "600", height = 350,
                                h4("Introduction:"),
                                p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent at risus vehicula, dapibus justo ac, laoreet velit. In et orci vel libero placerat faucibus aliquet ut turpis. Fusce blandit at turpis eget hendrerit. 
                                  Integer fermentum bibendum lectus a fermentum. Pellentesque quis nisl sollicitudin, dictum lacus vitae, fringilla orci. Suspendisse ultrices egestas turpis, at eleifend magna consequat vel. Morbi tincidunt enim sed efficitur porta. 
                                  Aliquam volutpat tempor orci eu euismod. In hac habitasse platea dictumst. Aliquam gravida elit sed luctus accumsan. Cras dapibus mauris id tempus bibendum. Nam auctor congue finibus. In sem dui, faucibus eu placerat at, imperdiet sit amet urna. Quisque ligula felis, sagittis ac fringilla id, malesuada ut ex.",
                                  style="text-align:justify"),
                                p("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent at risus vehicula, dapibus justo ac, laoreet velit. In et orci vel libero placerat faucibus aliquet ut turpis. Fusce blandit at turpis eget hendrerit. Integer fermentum bibendum lectus a fermentum. 
                                  Pellentesque quis nisl sollicitudin, dictum lacus vitae, fringilla orci. Suspendisse ultrices egestas turpis, at eleifend magna consequat vel. Morbi tincidunt enim sed efficitur porta.",
                                  style="text-align:justify"),
                                br()
                            ),
                            
                            absolutePanel(                                                                # define introduction box
                              id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                              top = 440, left = "20", right = "auto", bottom = "auto", width = "600", height = 40,
                              h4("Total Number Surveys:", HTML('&nbsp;'),HTML('&nbsp;'),  
                                 strong(format(sum(home_total$count), big.mark = ","), style = "color: #922121"), 
                                 "(", paste0(round((sum(home_total$count)/7000)*100,0),"%"), ")")
                            ),
                              
                              
                            absolutePanel(                                                                    # define chart box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "500", left = "20", right = "auto", bottom = "auto", width = "600", height = "430",
                                h4("# of Surveys per Day:"),
                                tags$br(),
                                hchart(home_merged, "column",                                           # define chart
                                       hcaes(x = date_assessment, y = count, group = pop_group)) %>%
                                    hc_yAxis(min = 0, title = list(text = "")) %>%
                                    hc_xAxis(title = "", labels = list(align = "center")) %>%
                                    hc_size(height = "453") %>%
                                    hc_title(
                                        text = "",
                                        margin = 10,
                                        align = "left",
                                        style = list(fontSize = 15)
                                    ) %>%
                                    hc_colors(cols) %>%
                                    hc_legend(align = "left", 
                                              layout = "horizontal")
                            ),

                        absolutePanel(
                          id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                          top = "70", left = "650", right = "auto", bottom = "auto", width = "550", height = "185",
                          h4("Overview:"),
                          HTML(table_round), br()
                        ),
                  

                          absolutePanel(                                                                    # define chart box
                           id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                           top = "265", left = "650", right = "auto", bottom = "auto", width = "auto", height = "auto",
                           h4("Progress Map:"),
                           leafletOutput("map_progress", width = "600", height = "600")
                           ), 
                        

                        
                        
               ), 
               
               tabPanel("Data Explorer", 
                        
                        sidebarLayout(
                          sidebarPanel(
                            


                                      pickerInput("table_department",
                                                         label = "Department:",   
                                                         options = list(title = "Select"),
                                                         choices = sort(unique(df$departamento)),
                                                         multiple = FALSE, 
                                                         selected = "antioquia"
                                                  
                                             ),

 

                            hr(),
                            
                            downloadButton("downloadData", "Download as CSV"),
                            
                            width = 3
                          ),
                          
                          mainPanel(
                            DT::dataTableOutput("data_table_enumerator", width = "100%", height = "100%"),
                            width = 9
                          )
                        )
               )
               
                   #### * 6.3 Map ######################################################################
                   
                   
                   

    )                                                                                                         # close navbarpage
)                                                                                                             # close bootstrappage

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  
  output$map_home_gaggi <- renderLeaflet({
    map_home <- leaflet(options = leafletOptions(attributionControl=FALSE, zoomControl = FALSE, dragging = FALSE, minZoom = 12, maxZoom = 12)) %>%
      setView(lng = -74.052536, lat = 4.666264, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB",
                       options = providerTileOptions(opacity = 0.8))
  })
  
  
  pal <- colorNumeric(c("firebrick", "burlywood", "forestgreen"), 0:100)
  
  labels <- paste(
    "<strong>", df.map$departamento,
    "</strong><br>% Completado:", paste0(round(df.map$pct.done,  1),"%"),
    "</strong><br># Restante:", df.map$Restante, 
    "</strong><br># Marcada Tiempo:", df.map$Marcada_Tiempo) %>%
    lapply(htmltools::HTML)
  
  output$map_progress <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 5.7, maxZoom = 5.7, dragging = FALSE, width = "100%")) %>%
    addPolygons(data=df.map, color = ~pal(df.map$pct.done),
                weight = 1, opacity = 0.5, fill = T, fillOpacity = 0.7,
                label=labels) %>%
    addMeasure(primaryLengthUnit = "kilometers") 

  })
  
  ######## Data Table
  data_table_enumerator <- function(){
    df %>% 
      filter(departamento == input$table_department) %>% 
      dplyr::group_by(entrevistador) %>% 
      dplyr::summarise(Cargado = n(),
                       Flagged = sum(time_validity == "Flagged", na.rm = T),
                       Median_Duration = round(median(duracion / 60),0),
                       Nr_surveys_pd = round((n() / n_distinct(date_assessment, na.rm = T)),1) %>%
                         sort(Flagged, decreasing = T))
  }
  # MODIFY CODE BELOW: Render a DT output named "table_top_10_names"
  output$data_table_enumerator <- DT::renderDT({
    DT::datatable(data_table_enumerator(), options = list(
      lengthMenu = list(c(10, 50, -1), c('10', '50', 'Todos')),
      pageLength = 50))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("IRQ-JPMI-data-download-", Sys.Date(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_table_enumerator(), file, row.names = FALSE, na = "")
    }
  )
  

}                                                                                 # close server function

shinyApp(ui = ui, server = server)                                                # run the application

