source("functions/function_handler.R")
source("functions/loop_generator.R")



# read retoma nutritional data from csv file
n_df_retoma <- read.csv("input/raw_data/nutritional/retake_vdp_cda_nutritional_dataset_efsa22_230922.csv", sep = ";"
               , comment.char = "", strip.white = TRUE,
               stringsAsFactors = F, encoding="UTF-8-BOM")
names(n_df_retoma)[names(n_df_retoma) == 'ï..Apl'] <- "Apl"



# read original nutritional data from csv file
n_df_original <- read.csv("input/raw_data/nutritional/untake_all_nutritional_dataset_efsa22_230922.csv", sep = ";"
                 , comment.char = "", strip.white = TRUE,
                 stringsAsFactors = F, encoding="UTF-8-BOM")
names(n_df_original)[names(n_df_original) == 'ï..Apl'] <- "Apl"



#############################
# change column names retoma
#change_names <- read_excel("Input/Codebook_Questionnaire_EFSA22.xlsx", sheet = "colnames")
change_names_n_retoma <- read.csv("Input/V2_Codebook_Nutritional_Questionnaire_EFSA22.csv", sep=";")
names(n_df_retoma) <- plyr::mapvalues(names(n_df_retoma), from = change_names_n_retoma$old_name, to = change_names_n_retoma$new_name)
table(change_names_n_retoma$new_name %in% names(n_df_retoma))


# change column names original data
change_names_n_original <- read.csv("Input/Codebook_Nutritional_Questionnaire_EFSA22.csv", sep=";")
names(n_df_original) <- plyr::mapvalues(names(n_df_original), from = change_names_n_original$old_name, to=change_names_n_original$new_name)
table(change_names_n_original$new_name %in% names(n_df_original))


`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}
if(any(!(change_names_n_original$new_name %in% names(n_df_original)))){
  warning("some names present in  not found in change_names_n dataset")
  warning(which(!(change_names_n_original$new_name %in% names(n_df_original))) %>% length)
}
names(n_df_original) %find those not in% change_names_n_original$new_name



#merge original dataset with registro and popgroup of hh_dataset and filter out VdP and CdA
response <- read.csv("Input/raw_data/nutritional/household_full_efsa_all_data_2022-08-22.csv", sep = ";",
                     stringsAsFactors=F, check.names=T,
                     na.strings = c("", " ", "NA", "#N/A", "N/A"))

n_df_original <- response %>% select("pop_group", "registro") %>%
                              right_join(n_df_original, by="registro") %>%
                              filter((edad_gestante >= 0) | (edad_mayores >= 0 | 
                                                               (edad_ninos023 >= 0) | (edad_ninos2459 >= 0))) %>%
                              filter(pop_group %in% c("transito", "pendular", "retornado")) %>%
                              select(starts_with("final_"), "GENERO", starts_with("edad"), "pop_group", 
                                     "DEPARTAMENTO", "registro", "EDAD", starts_with("final_hemoglobina_")) %>%
                              rename("departamento" = "DEPARTAMENTO",
                                     "sexo" = "GENERO")


#merge retoma dataset with registro and popgroup of hh_dataset and filter out VdP and CdA
n_df_retoma <- n_df_retoma %>% select(starts_with("final_"), starts_with("sexo_"), starts_with("edad"), 
                                      "departamento", "registro", "EDAD") %>%
                               mutate(sexo = paste0(sexo_mayores, sexo_ninos023, sexo_ninos2459),
                                      sexo = case_when(sexo == "MujerMujer" ~ "Mujer",
                                                       sexo == "HombreHombre"~ "Hombre", 
                                                       sexo == "Hombre" ~ "Hombre",
                                                       sexo == "Mujer" ~ "Mujer", 
                                                       sexo == "NA" ~ "Mujer", 
                                                       TRUE ~ "Mujer"))

# add hemoglobin data to retake dataset
gaggi <- n_df_original %>% select(starts_with("final_hemoglobina_"), registro, EDAD) %>%
  right_join(n_df_retoma, by= "registro")



n_df_retoma <- response %>% select("pop_group", "registro") %>%
  right_join(n_df_retoma, by="registro") %>%
  filter((edad_gestante >= 0) | (edad_mayores >= 0 | 
                                   (edad_ninos023 >= 0) | (edad_meses_ninos2459 >= 0))) %>%
  filter(pop_group %in% c("comunidad_de_acogida", "vocacion_de_permanencia")) %>%
  select(starts_with("final_"), "sexo", starts_with("edad"), "pop_group", 
         "departamento", "registro", starts_with("final_hemoglobina_")) %>%
  rename("edad_ninos2459" = "edad_meses_ninos2459") 


n_df_original$final_peso_infante_ninos023 <- as.numeric(as.character(n_df_original$final_peso_infante_ninos023))

# merge both orginal and retake datasets together
n_df <- bind_rows(n_df_retoma, n_df_original)


gaggi <- n_df %>% select(starts_with("edad"), "EDAD")





############################
# replace accented letters with regular ones
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}

n_df <- rapply(n_df, f = accented_letters, classes = c("factor"), how = "replace")
n_df <- rapply(n_df, f = accented_letters, classes = c("factor"), how = "replace")



#############################
# set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}

n_df <- rapply(n_df, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
table(sapply(n_df, is.numeric))
check_numeric <- n_df[ , purrr::map_lgl(n_df, is.numeric)]


#############################
# rename bogota__d_c_ to bogota_dc
n_df$departamento[n_df$departamento == "bogota__d_c_"] <- "bogota_dc"


# remove all columns without a name
n_df <- n_df[ , !(names(n_df) == "")]



#n_df$presencia_ninos_0_59 <- ifelse(n_df$nino_2459_1_edad != "_" | n_df$nino_2459_2_edad != "_" |
##                                      n_df$nino_2459_3_edad != "_" | n_df$nino_2459_4_edad != "_" |
#                                      n_df$nino_2459_5_edad != "_" | n_df$nino_2459_6_edad != "_" |
#                                      n_df$nino_2459_7_edad != "_" | n_df$nino_2459_8_edad != "_" |
##                                      n_df$nino_2459_9_edad != "_" | n_df$nino_2459_10_edad != "_" |
#                                      n_df$nino_023_1_edad != "_" | n_df$nino_023_2_edad != "_" | 
##                                      n_df$nino_023_3_edad != "_" | n_df$nino_023_4_edad != "_" |
#                                      n_df$nino_023_5_edad != "_" | n_df$nino_023_6_edad != "_" |
#                                      n_df$nino_023_7_edad != "_" | n_df$nino_023_8_edad != "_" |
#                                      n_df$nino_023_9_edad != "_" | n_df$nino_023_10_edad != "_",1,0)


#n_df$presencia_mayores65 <- ifelse(n_df$mayores65_1_edad != "_" | n_df$mayores65_2_edad != "_" |
#                                      n_df$mayores65_3_edad != "_" | n_df$mayores65_4_edad != "_" |
#                                      n_df$mayores65_5_edad != "_" | n_df$mayores65_6_edad != "_" |
#                                      n_df$mayores65_7_edad != "_" | n_df$mayores65_8_edad != "_" |
#                                      n_df$mayores65_9_edad != "_" | n_df$mayores65_10_edad != "_",1,0)


#n_df$presencia_gestantes <- ifelse(n_df$embarazo_1_edad != "_" | n_df$embarazo_2_edad != "_" |
#                                     n_df$embarazo_3_edad != "_" | n_df$embarazo_4_edad != "_" |
#                                     n_df$embarazo_5_edad != "_" | n_df$embarazo_6_edad != "_" |
#                                     n_df$embarazo_7_edad != "_" | n_df$embarazo_8_edad != "_" |
#                                     n_df$embarazo_9_edad != "_" | n_df$embarazo_10_edad != "_",1,0)



write.csv(n_df, sprintf("output/data_checking/nutritional_data/nutritional_data_%s.csv",today()), row.names = F)
write.csv(n_df, sprintf("Dashboard/Input/nutritional_data/nutritional_data_%s.csv",today()), row.names = F)


# generate loop data
loop_ninos_023 <- n_df %>% dplyr::select(registro, pop_group, sexo, which(endsWith(names(n_df), "_ninos023"))) %>%
                          filter(final_talla_infantil_ninos023 != "") %>%
                          mutate(across(c(2:last_col()), na_if, 999)) %>%
                          mutate(across(c(2:last_col()), na_if, 999.0))


loop_ninos_2459 <- n_df %>% dplyr::select(registro, pop_group, sexo, which(endsWith(names(n_df), "_ninos2459"))) %>%
  filter(final_talla_ninos2459 != "") %>%
  mutate(across(c(4:last_col()), na_if, 999)) %>%
  mutate(across(c(4:last_col()), na_if, 999.0))

loop_gestantes <- n_df %>% dplyr::select(registro, pop_group, sexo, which(endsWith(names(n_df), "_gestante"))) %>%
  filter(final_talla_gestante != "") %>%
  mutate(across(c(4:last_col()), na_if, 999)) %>%
  mutate(across(c(4:last_col()), na_if, 999.0))
  

loop_mayores65 <- n_df %>% dplyr::select(registro, pop_group, sexo, which(endsWith(names(n_df), "_mayores"))) %>%
  filter(final_peso_mayores != "") %>%
  mutate(across(c(2:last_col()), na_if, 999)) %>%
  mutate(across(c(2:last_col()), na_if, 999.0))


xl_lst <- list('ninos_0_23' = loop_ninos_023, 'ninos_24_59' = loop_ninos_2459, 
               'embarazos' = loop_gestantes, 'mayores_65' = loop_mayores65)
write.xlsx(xl_lst, file = sprintf("output/data_checking/nutritional_data/joint_nutritional_data_%s.csv",today()))










###########################################
##CHECK IF ALL MATCH DATAFRAME:
`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}

if(any(!(n_df_original$registro %in% n_df_retoma$registro))){
  warning("some registros not found in HH dataframe")
  warning(which(!(n_df_original$registro %in% n_df_retoma$registro)) %>% length)
}
n_df_original$registro %find those not in% n_df_retoma$registro


# merge n_df with pop_group variable of household dataset
registro_hh <- select(df, registro, pop_group)
n_df <- merge(registro_hh, n_df, by="registro", all.y = T)



# merge all datasets together
colnames(loop) <- paste("loop", colnames(loop), sep = "_")
names(loop)[names(loop) == 'loop_registro'] <- "registro"
names(loop)[names(loop) == 'loop_nombre'] <- "nombre"
dat_nutritional_full <- merge(df, loop, by.x ="registro", all.y = T)


colnames(loop_nino_023) <- paste("nino_023", colnames(loop_nino_023), sep = "_")
names(loop_nino_023)[names(loop_nino_023) == 'nino_023_registro'] <- "registro"
names(loop_nino_023)[names(loop_nino_023) == 'nino_023_nombre'] <- "nombre"
dat_nutritional_full <- merge(dat_nutritional_full, loop_nino_023, by.y=c("registro", "nombre"), all.x = T)


colnames(loop_nino_2459) <- paste("nino_2459", colnames(loop_nino_2459), sep = "_")
names(loop_nino_2459)[names(loop_nino_2459) == 'nino_2459_registro'] <- "registro"
names(loop_nino_2459)[names(loop_nino_2459) == 'nino_2459_nombre'] <- "nombre"
dat_nutritional_full <- merge(dat_nutritional_full, loop_nino_2459, by.y=c("registro", "nombre"), all.x = T)

colnames(loop_mayores65) <- paste("mayores65", colnames(loop_mayores65), sep = "_")
names(loop_mayores65)[names(loop_mayores65) == 'mayores65_registro'] <- "registro"
names(loop_mayores65)[names(loop_mayores65) == 'mayores65_nombre'] <- "nombre"
dat_nutritional_full <- merge(dat_nutritional_full, loop_mayores65, by.y=c("registro", "nombre"), all.x = T)

colnames(loop_embarazo) <- paste("embarazadas", colnames(loop_embarazo), sep = "_")
names(loop_embarazo)[names(loop_embarazo) == 'embarazadas_registro'] <- "registro"
dat_nutritional_full$nombre_merge <- substring(dat_nutritional_full$nombre, 1, 4)
loop_embarazo$nombre_merge <- substring(loop_embarazo$embarazadas_nombre, 1, 4)
dat_nutritional_full <- merge(dat_nutritional_full, loop_embarazo, by=c("registro", "nombre_merge"), all.x = T)


`%find those not in%`<-function(x,y){x[!(x%in%y)] %>% unique}
if(any(!(loop_embarazo$embarazadas_nombre %in% dat_nutritional_full$embarazadas_nombre))){
  warning("some ages present in loop not found in nutritional dataset")
  warning(which(!(loop_embarazo$embarazadas_nombre %in% dat_nutritional_full$embarazadas_nombre)) %>% length)
}


dat_nutritional_full <- dat_nutritional_full %>% 
  filter(pop_group == "vocacion_de_permanencia") %>% 
  select(-contains("P1")) %>% 
  select(-contains("P1")) %>% 
  select(-contains("P3")) %>% 
  select(-contains("P4")) %>% 
  select(-contains("P5")) %>% 
  select(-contains("P6")) %>% 
  select(-contains("P7")) %>% 
  select(-contains("P8")) %>% 
  select(-contains("P9")) %>% 
  select(-contains("ind")) %>% 
  select(-contains("V_")) %>% 
  select(-ends_with("_V")) %>% 
  select(-contains("AGENDAMIENTO")) %>% 
  select(-contains("CONTINUAR")) %>% 
  select(-contains("LATITUD")) %>% 
  select(-contains("LONGITUD")) %>% 
  select(-contains("FOTOGRAFIA")) %>% 
  select(-contains("entrevistador")) %>% 
  select(-contains("telefono")) %>% 
  select(-contains("SUP")) %>% 
  select(-contains("evaluacion")) %>% 
  select(-contains("email")) %>% 
  select(-contains("id_contacto")) %>% 
  select(-contains("estado")) %>% 
  select(-contains("idioma")) %>% 
  select(-contains("fecha")) %>% 
  select(-contains("hora_")) %>% 
  select(-contains("duracion")) %>% 
  select(-contains("nombre")) 

## strip dataset of all PII
dat_nutritional_full <- dat_nutritional_full[, -c(12:59)] # delete columns 5 through 7

df[ ,c("nombre_respondiente", "Apl", "Mod", "evaluacion", 
       "entrevistador")] <- list(NULL)



write.xlsx(dat_nutritional_full, "Output/data_checking/GIFMM/nutritional_data_010822.xlsx")










