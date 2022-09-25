#SETUP
rm(list=ls(all=T))
R.version
#library(rlang)
library(tidyverse)
library(lubridate)
library(readxl)
library(kableExtra)
library(knitr)
library(rmarkdown)
library(stringi)
library(readr)
library(openxlsx)
library(sf)
library(Hmisc)
library(rsconnect)
library(htmlwidgets)

source("functions/function_handler.R")
source("functions/loop_generator.R")

assessment_start_date <- as.Date("2022-06-20")

delete_time_limit <- 20
flag_time_limit <- 35


# read data from excel file
#df <- read_excel("input/raw_data/raw_dataset_efsa22_240622.xlsx")
df <- read.csv("input/raw_data/CLEAN_FULL_dataset_WFP_EFSA22_110822.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = TRUE, encoding="UTF-8-BOM")
names(df)[names(df) == 'ï..Apl'] <- "Apl"


#############################
# change column names
#change_names <- read_excel("Input/Codebook_Questionnaire_EFSA22.xlsx", sheet = "colnames")
change_names <- read.csv("Input/Codebook_Questionnaire_EFSA22.csv", sep=";", 
                         fileEncoding="UTF-8-BOM")
names(df) <- plyr::mapvalues(names(df), from = change_names$old_name, to = change_names$new_name)
table(change_names$new_name %in% names(df))

`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}
if(any(!(change_names$new_name %in% names(df)))){
  warning("some names present in  not found in change_names dataset")
  warning(which(!(change_names$new_name %in% names(df))) %>% length)
}
change_names$new_name %find those not in% names(df)



############################
# replace accented letters with regular ones
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}

df <- rapply(df, f = accented_letters, classes = c("factor", "character"), how = "replace")
df <- rapply(df, f = accented_letters, classes = c("factor", "character"), how = "replace")



#############################
# set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}

df <- rapply(df, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
table(sapply(df, is.numeric))
check_numeric <- df[ , purrr::map_lgl(df, is.numeric)]


#############################
# rename bogota__d_c_ to bogota_dc
df$departamento[df$departamento == "bogota__d_c_"] <- "bogota_dc"



############################
# remove interviews that are market rechazado or en curso
#df <- filter(df, 
#                  estado == "finalizada__mobinet_")



############################
# classify household in one of the five groups
df$pop_group <- df$grupo_jefe_hogar



############################
# calculate mean interview duration by enumerator

df$duracion_min <- df$duracion / 60

df %>% 
  group_by(entrevistador) %>%
  summarise_at(vars(duracion_min), list(name = mean))
df$duracion_min <- round(df$duracion_min, 1)


############################
# convert all expenditure and income data from factor to numeric
df[c(which(startsWith(names(df), "gastos_")))] <- 
  as.data.frame(lapply(df[c(which(startsWith(names(df),"gastos_")))], 
                       function(x) as.numeric(as.character(x))))

cols = c("ingreso", "ingreso_V");    
df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)))


############################
# convert all age data from individual members to numeric
df[c(which(endsWith(names(df), "_edad")))] <- 
  as.data.frame(lapply(df[c(which(endsWith(names(df),"_edad")))], 
                       function(x) as.numeric(as.character(x))))

cols = c("ingreso", "ingreso_V");    
df[,cols] = apply(df[,cols], 2, function(x) as.numeric(as.character(x)))


####################################
# when survey does not continue to LCS section, survey is considered incomplete
df <- df %>% 
  mutate(not_eligible = case_when(lcs_sacar_ninos_escuela == "" ~ "not_eligible",
                                  TRUE ~ "eligible"))
table(df$not_eligible)


####################################
# change date format
df$date_assessment <- strptime(as.character(df$fecha_in), "%d_%m_%Y")
df$date_assessment <-  format(df$date, "%Y-%m-%d")



####################################
# set min time and max # interviews per day

# flag surveys that were below the time limit
df <- df %>% 
  mutate(time_validity = case_when(duracion_min < flag_time_limit ~ "Flagged", 
                                   duracion_min < delete_time_limit ~ "Delete",
                                   TRUE ~ "valid"))


##########################################################################################
# Deleted interviews column
df <- df %>% 
  mutate(
    delete = case_when(
      time_validity == "Delete" | iniciar == "no" | not_eligible == "not_eligible" ~ "yes",
      TRUE ~ "no"))
table(df$delete)


########################################################################################
###########################################################################################################
# calculate new variables
# number of NAs check
df$NAs <- apply(df,1,FUN=function(x){length(which(is.na(x)))})
df$NAs

# sum all expenditure in past 30d
df$tot_gastos <- as.numeric(apply(df[,c("gastos_cereales","gastos_tuberculos",
                                              "gastos_legumbres","gastos_vegetales",
                                              "gastos_frutas","gastos_carne",
                                              "gastos_pescado","gastos_huevos",
                                              "gastos_aceite", "gastos_leche",
                                              "gastos_azucar","gastos_condimentos",
                                              "gastos_bebidas_non_alcoholicas","gastos_comida_fuera_casa",
                                              "gastos_agua_beber", "gastos_agua_domestico",
                                              "gastos_renta", "gastos_electricidad",
                                              "gastos_basura","gastos_higiene",
                                              "gastos_transporte","gastos_comunicacion",
                                              "gastos_lena", "gastos_gasolina",
                                              "gastos_otros")], 
                                      1, sum))

# average consumption in past week
df$average_consumption <- (df$fcs_azucares + df$fcs_carne + df$fcs_cereales + df$fcs_condimentos +
  df$fcs_frutas + df$fcs_leche + df$fcs_vegetales) / 8


# clean gastos data according to Tito's guidance
source("functions/exp_cleaner.R")
df <- expenditure_cleaner(df)



# generate loop separately and remove individual data from household dataset
loop <- loop_generator(df)



## strip dataset of all PII
#df <- df[, -c(13:60)] # delete columns 5 through 7

#df[ ,c("nombre_respondiente", "ind1_nombre", "ind2_nombre", "ind3_nombre", 
#       "ind4_nombre", "ind5_nombre", "ind6_nombre", "ind7_nombre", "ind8_nombre", 
#       "ind9_nombre", "ind10_nombre", "ind11_nombre", "ind12_nombre", "ind13_nombre", 
#       "ind14_nombre", "ind15_nombre", "ind16_nombre", "ind17_nombre",
#       "ind18_nombre", "ind19_nombre")] <- list(NULL)


#select variables that will be shared with gifmm
#df <- df %>% select(registro, departamento, pcode, barrio, nacionalidad_jefe_hogar, tipo_migrante_jefe_hogar, 
#                         grupo_jefe_hogar, sexo_jh, sexo_jh_otro, nivel_estudios_jh, nr_escuela_colegio, 
#                         ingreso, tipo_vivienda, tipo_vivienda_otro, material_paredes_exteriores, 
#                         material_paredes_exteriores_otro, material_pisos, material_pisos_otro, nr_cuartos_total, 
#                         tipo_servicio_sanitario, tipo_servicio_sanitario_otro, acuerdo_ocupacion)

#loop <- loop %>% select(registro, sexo, edad, edad_anos_meses)




##########################################################################################################
### EXPORT FOR DATA CHECKING #############

##df<- df %>% 
##dplyr::select(-(snowballing_willing:`_gpslocation_precision`))

##### Write to csv for data checking ###################
write.csv(df, sprintf("output/data_checking/efsa_all_data_%s.csv",today()), row.names = F)
write.csv(df, sprintf("Dashboard/Input/efsa_all_data_%s.csv",today()), row.names = F)



###########################################################################################################
###########################################################################################################
###########################################################################################################
# DO CLEANING
# read cleaning conditions csv list
conditionDf_1 <- read.csv("input/conditions/v2_conditions_log.csv", as.is = TRUE, sep = ";")

#debug(read_conditions_from_excel_limited_row)

# return logs
logs <- read_conditions_from_excel_limited_row(df, conditionDf_1);


# create new columns "log_number"
logs$log_number = seq.int(nrow(logs))
# order data frame by log_number
ordered_df <- logs[order(logs$log_number),]
readr::write_excel_csv(ordered_df, sprintf("Output/cleaning_log/cleaning_log_%s.csv",today()))
readr::write_excel_csv(ordered_df, sprintf("Dashboard/Input/cleaning_log.csv"))


# export data in one single datasdet with household and loop data in separate sheets
xl_lst <- list('hogar' = df, 'individual' = loop)
write.xlsx(xl_lst, file = sprintf("output/data_checking/efsa_all_data_%s.xlsx",today()))


# import nutritional dataset and merge with household-level data
source("import_nutritional.R")


