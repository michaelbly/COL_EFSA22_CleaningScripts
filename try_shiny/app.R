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
pal <- colorNumeric(palette = "Blues", 0:100)


labels <- paste(
  "<strong>", df.map$departamento,
  "</strong><br>% Completado:", round(df.map$pct.done,  1),
  "</strong><br># Restante:", df.map$Restante, 
  "</strong><br># Marcada Tiempo:", df.map$Marcada_Tiempo) %>%
  lapply(htmltools::HTML)

map_home <- leaflet(options = leafletOptions(zoomControl = FALSE,
                                 minZoom = 5.7, maxZoom = 5.7, dragging = FALSE, width = "100%")) %>%
  addPolygons(data=df.map, color = "black", weight = 0.5, opacity = 1, fill = F, fillOpacity = 0, 
              smoothFactor = 0, stroke = TRUE) %>%
  addPolygons(data=df.map, color = ~pal(df.map$pct.done),
              weight = 1, opacity = 0.5, fill = T, fillOpacity = 0.7,
              label=labels) %>%
  addTiles() %>%
  addMeasure(primaryLengthUnit = "kilometers") %>% 
  addProviderTiles("Esri.WorldGrayCanvas")




#### 6 UI ######################################################################

ui <- bootstrapPage(

    navbarPage("EFSA22 | Data Collection",                                                # define dashboard title
               theme = shinytheme("flatly"),                                                             # set theme
               
               #### * 6.1 Home ######################################################################
               
               tabPanel("Overview",                                                                      # define panel title

                            absolutePanel(                                                                # define introduction box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = 70, left = "20", right = "auto", bottom = "auto", width = "600", height = 350,
                                h4("Introduction"),
                                p("The Joint Price Monitoring Initiative (JPMI) is a bi-monthly data collection exercise launched by the Iraq Cash Working Group (CWG)
                                   in November 2016. The initiative aims to inform cash-based interventions in Iraq by providing indicative information on key commodities
                                   sold in local marketplaces. The initiative is guided by the CWG, led by REACH and supported by the CWG members.",
                                  style="text-align:justify"),
                                p("This website displays a wide range of indicators collected through the JPMI, such as prices for key food
                                   and non-food items (NFIs), as well as the costs associated with the Survival Minimum Expenditure Basket (SMEB).",
                                  style="text-align:justify"),
                                br()
                            ),
                            
                            absolutePanel(                                                                # define introduction box
                              id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                              top = 440, left = "20", right = "auto", bottom = "auto", width = "600", height = 40,
                              h4("Total Number Surveys:", HTML('&nbsp;'),HTML('&nbsp;'),  strong(sum(home_total$count)))
                            ),
                              
                              
                            absolutePanel(                                                                    # define chart box
                                id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                                top = "500", left = "20", right = "auto", bottom = "auto", width = "600", height = "430",
                                hchart(home_merged, "column",                                           # define chart
                                       hcaes(x = date_assessment, y = count, group = pop_group)) %>%
                                    hc_yAxis(min = 0, title = list(text = "")) %>%
                                    hc_xAxis(title = "", labels = list(align = "center")) %>%
                                    hc_size(height = "453") %>%
                                    hc_title(
                                        text = "Overall Median SMEB Over Time (in IQD)",
                                        margin = 10,
                                        align = "left",
                                        style = list(fontSize = 15)
                                    ) %>%
                                    hc_colors(cols) %>%
                                    hc_legend(align = "left", 
                                              layout = "horizontal")
                            ),

                            
                          absolutePanel(                                                                    # define chart box
                           id = "home", class = "panel panel-default", fixed = FALSE, draggable = FALSE,
                           top = "70", left = "650", right = "auto", bottom = "auto", width = "auto", height = "auto",
                           h4(strong("Mapa:")),
                           leafletOutput("map_home", width = "600", height = "600")), 
                        
                        
               )
               
                   #### * 6.3 Map ######################################################################
                   
                   
                   

    )                                                                                                         # close navbarpage
)                                                                                                             # close bootstrappage

#### 7 SERVER ##################################################################

server <- function(input, output, session) {
  pal <- colorNumeric(palette = "Blues", 0:100)
  
  labels <- paste(
    "<strong>", df.map$departamento,
    "</strong><br>% Completado:", round(df.map$pct.done,  1),
    "</strong><br># Restante:", df.map$Restante, 
    "</strong><br># Marcada Tiempo:", df.map$Marcada_Tiempo) %>%
    lapply(htmltools::HTML)
  
  output$map_home <- renderLeaflet({
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                   minZoom = 5.7, maxZoom = 5.7, dragging = FALSE, width = "100%")) %>%
    addPolygons(data=df.map, color = ~pal(df.map$pct.done),
                weight = 1, opacity = 0.5, fill = T, fillOpacity = 0.7,
                label=labels) %>%
    addMeasure(primaryLengthUnit = "kilometers")
  })
  
}                                                                                 # close server function

shinyApp(ui = ui, server = server)                                                # run the application

