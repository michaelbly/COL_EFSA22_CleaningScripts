loop_generator <- function(df) {
  

ind_1 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind1_")))
names(ind_1) = gsub(pattern = "ind1_", 
                    replacement = "", x = names(ind_1))
df[c(which(startsWith(names(df), "ind1_")))] <- NULL


ind_2 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind2_")))
names(ind_2) = gsub(pattern = "ind2_", 
                    replacement = "", x = names(ind_2))
df[c(which(startsWith(names(df), "ind2_")))] <- NULL


ind_3 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind3_")))
names(ind_3) = gsub(pattern = "ind3_", 
                    replacement = "", x = names(ind_3))
df[c(which(startsWith(names(df), "ind3_")))] <- NULL


ind_4 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind4_")))
names(ind_4) = gsub(pattern = "ind4_", 
                    replacement = "", x = names(ind_4))
df[c(which(startsWith(names(df), "ind4_")))] <- NULL


ind_5 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind5_")))
names(ind_5) = gsub(pattern = "ind5_", 
                    replacement = "", x = names(ind_5))
df[c(which(startsWith(names(df), "ind5_")))] <- NULL


ind_6 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind6_")))
names(ind_6) = gsub(pattern = "ind6_", 
                    replacement = "", x = names(ind_6))
df[c(which(startsWith(names(df), "ind6_")))] <- NULL


ind_7 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind7_")))
names(ind_7) = gsub(pattern = "ind7_", 
                    replacement = "", x = names(ind_7))
df[c(which(startsWith(names(df), "ind7_")))] <- NULL


ind_8 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind8_")))
names(ind_8) = gsub(pattern = "ind8_", 
                    replacement = "", x = names(ind_8))
df[c(which(startsWith(names(df), "ind8_")))] <- NULL


ind_9 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind9_")))
names(ind_9) = gsub(pattern = "ind9_", 
                    replacement = "", x = names(ind_9))
df[c(which(startsWith(names(df), "ind9_")))] <- NULL


ind_10 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind10_")))
names(ind_10) = gsub(pattern = "ind10_", 
                    replacement = "", x = names(ind_10))
df[c(which(startsWith(names(df), "ind10_")))] <- NULL


ind_11 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind11_")))
names(ind_11) = gsub(pattern = "ind11_", 
                    replacement = "", x = names(ind_2))
df[c(which(startsWith(names(df), "ind11_")))] <- NULL


ind_12 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind12_")))
names(ind_12) = gsub(pattern = "ind12_", 
                    replacement = "", x = names(ind_12))
df[c(which(startsWith(names(df), "ind12_")))] <- NULL


ind_13 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind13_")))
names(ind_13) = gsub(pattern = "ind13_", 
                    replacement = "", x = names(ind_13))
df[c(which(startsWith(names(df), "ind13_")))] <- NULL


ind_14 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind14_")))
names(ind_14) = gsub(pattern = "ind14_", 
                    replacement = "", x = names(ind_14))
df[c(which(startsWith(names(df), "ind14_")))] <- NULL


ind_15 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind15_")))
names(ind_15) = gsub(pattern = "ind15_", 
                    replacement = "", x = names(ind_15))
df[c(which(startsWith(names(df), "ind15_")))] <- NULL


ind_16 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind16_")))
names(ind_16) = gsub(pattern = "ind16_", 
                    replacement = "", x = names(ind_16))
df[c(which(startsWith(names(df), "ind16_")))] <- NULL


ind_17 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind17_")))
names(ind_17) = gsub(pattern = "ind17_", 
                    replacement = "", x = names(ind_17))
df[c(which(startsWith(names(df), "ind17_")))] <- NULL


ind_18 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind18_")))
names(ind_18) = gsub(pattern = "ind18_", 
                    replacement = "", x = names(ind_18))
df[c(which(startsWith(names(df), "ind18_")))] <- NULL


ind_19 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind19_")))
names(ind_19) = gsub(pattern = "ind19_", 
                    replacement = "", x = names(ind_19))
df[c(which(startsWith(names(df), "ind19_")))] <- NULL


ind_20 <- df %>% dplyr::select(registro, which(startsWith(names(df), "ind20_")))
names(ind_20) = gsub(pattern = "ind20_", 
                    replacement = "", x = names(ind_20))
df[c(which(startsWith(names(df), "ind20_")))] <- NULL


loop <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
               ind_8, ind_9, ind_10, ind_11, ind_12, ind_13,
               ind_14, ind_15, ind_16, ind_17, ind_18, ind_19, ind_20)
  

loop <- loop[!is.na(loop$edad),]


return(loop)
}


loop_generator_ninos023 <- function(df) {
  
n_023 <- n_df %>% dplyr::select(registro, which(startsWith(names(n_df), "nino_023_")))
ind_1 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_1_")))
names(ind_1) = gsub(pattern = "nino_023_1_", 
                     replacement = "", x = names(ind_1))

ind_2 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_2_")))
names(ind_2) = gsub(pattern = "nino_023_2_", 
                    replacement = "", x = names(ind_2))

ind_3 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_3_")))
names(ind_3) = gsub(pattern = "nino_023_3_", 
                    replacement = "", x = names(ind_3))

ind_4 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_4_")))
names(ind_4) = gsub(pattern = "nino_023_4_", 
                    replacement = "", x = names(ind_4))

ind_5 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_5_")))
names(ind_5) = gsub(pattern = "nino_023_5_", 
                    replacement = "", x = names(ind_5))

ind_6 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_6_")))
names(ind_6) = gsub(pattern = "nino_023_6_", 
                    replacement = "", x = names(ind_6))

ind_7 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_7_")))
names(ind_7) = gsub(pattern = "nino_023_7_", 
                    replacement = "", x = names(ind_7))

ind_8 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_8_")))
names(ind_8) = gsub(pattern = "nino_023_8_", 
                    replacement = "", x = names(ind_8))

ind_9 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_9_")))
names(ind_9) = gsub(pattern = "nino_023_9_", 
                    replacement = "", x = names(ind_9))

ind_10 <- n_023 %>% dplyr::select(registro, which(startsWith(names(n_023), "nino_023_10_")))
names(ind_10) = gsub(pattern = "nino_023_10_", 
                    replacement = "", x = names(ind_10))


loop_nino_023 <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
              ind_8, ind_9, ind_10)


loop_nino_023 <- filter(loop_nino_023, loop_nino_023$nombre != "_")


return(loop_nino_023)
}


loop_generator_ninos2459 <- function(df) {
  
  n_2459 <- n_df %>% dplyr::select(registro, which(startsWith(names(n_df), "nino_2459_")))
  ind_1 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_1_")))
  names(ind_1) = gsub(pattern = "nino_2459_1_", 
                      replacement = "", x = names(ind_1))
  
  ind_2 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_2_")))
  names(ind_2) = gsub(pattern = "nino_2459_2_", 
                      replacement = "", x = names(ind_2))
  
  ind_3 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_3_")))
  names(ind_3) = gsub(pattern = "nino_2459_3_", 
                      replacement = "", x = names(ind_3))
  
  ind_4 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_4_")))
  names(ind_4) = gsub(pattern = "nino_2459_4_", 
                      replacement = "", x = names(ind_4))
  
  ind_5 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_5_")))
  names(ind_5) = gsub(pattern = "nino_2459_5_", 
                      replacement = "", x = names(ind_5))
  
  ind_6 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_6_")))
  names(ind_6) = gsub(pattern = "nino_2459_6_", 
                      replacement = "", x = names(ind_6))
  
  ind_7 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_7_")))
  names(ind_7) = gsub(pattern = "nino_2459_7_", 
                      replacement = "", x = names(ind_7))
  
  ind_8 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_8_")))
  names(ind_8) = gsub(pattern = "nino_2459_8_", 
                      replacement = "", x = names(ind_8))
  
  ind_9 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_9_")))
  names(ind_9) = gsub(pattern = "nino_2459_9_", 
                      replacement = "", x = names(ind_9))
  
  ind_10 <- n_2459 %>% dplyr::select(registro, which(startsWith(names(n_2459), "nino_2459_10_")))
  names(ind_10) = gsub(pattern = "nino_2459_10_", 
                       replacement = "", x = names(ind_10))
  
  
  loop_nino_2459 <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
                          ind_8, ind_9, ind_10)
  
  
  loop_nino_2459 <- filter(loop_nino_2459, loop_nino_2459$nombre != "_")
  
  
  return(loop_nino_2459)
}


loop_generator_embarazo <- function(df) {
  
  embarazo <- n_df %>% dplyr::select(registro, which(startsWith(names(n_df), "embarazo_")))
  ind_1 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_1_")))
  names(ind_1) = gsub(pattern = "embarazo_1_", 
                      replacement = "", x = names(ind_1))
  
  ind_2 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_2_")))
  names(ind_2) = gsub(pattern = "embarazo_2_", 
                      replacement = "", x = names(ind_2))
  
  ind_3 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_3_")))
  names(ind_3) = gsub(pattern = "embarazo_3_", 
                      replacement = "", x = names(ind_3))
  
  ind_4 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_4_")))
  names(ind_4) = gsub(pattern = "embarazo_4_", 
                      replacement = "", x = names(ind_4))
  
  ind_5 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_5_")))
  names(ind_5) = gsub(pattern = "embarazo_5_", 
                      replacement = "", x = names(ind_5))
  
  ind_6 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_6_")))
  names(ind_6) = gsub(pattern = "embarazo_6_", 
                      replacement = "", x = names(ind_6))
  
  ind_7 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_7_")))
  names(ind_7) = gsub(pattern = "embarazo_7_", 
                      replacement = "", x = names(ind_7))
  
  ind_8 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_8_")))
  names(ind_8) = gsub(pattern = "embarazo_8_", 
                      replacement = "", x = names(ind_8))
  
  ind_9 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_9_")))
  names(ind_9) = gsub(pattern = "embarazo_9_", 
                      replacement = "", x = names(ind_9))
  
  ind_10 <- embarazo %>% dplyr::select(registro, which(startsWith(names(embarazo), "embarazo_10_")))
  names(ind_10) = gsub(pattern = "embarazo_10_", 
                       replacement = "", x = names(ind_10))
  
  
  loop_embarazo <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
                         ind_8, ind_9, ind_10)
  
  
  loop_embarazo <- filter(loop_embarazo, loop_embarazo$nombre != "_")
  
  
  return(loop_embarazo)
}



loop_generator_mayores65 <- function(df) {
  
  mayores65 <- n_df %>% dplyr::select(registro, which(startsWith(names(n_df), "mayores65_")))
  ind_1 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_1_")))
  names(ind_1) = gsub(pattern = "mayores65_1_", 
                      replacement = "", x = names(ind_1))
  
  ind_2 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_2_")))
  names(ind_2) = gsub(pattern = "mayores65_2_", 
                      replacement = "", x = names(ind_2))
  
  ind_3 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_3_")))
  names(ind_3) = gsub(pattern = "mayores65_3_", 
                      replacement = "", x = names(ind_3))
  
  ind_4 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_4_")))
  names(ind_4) = gsub(pattern = "mayores65_4_", 
                      replacement = "", x = names(ind_4))
  
  ind_5 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_5_")))
  names(ind_5) = gsub(pattern = "mayores65_5_", 
                      replacement = "", x = names(ind_5))
  
  ind_6 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_6_")))
  names(ind_6) = gsub(pattern = "mayores65_6_", 
                      replacement = "", x = names(ind_6))
  
  ind_7 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_7_")))
  names(ind_7) = gsub(pattern = "mayores65_7_", 
                      replacement = "", x = names(ind_7))
  
  ind_8 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_8_")))
  names(ind_8) = gsub(pattern = "mayores65_8_", 
                      replacement = "", x = names(ind_8))
  
  ind_9 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_9_")))
  names(ind_9) = gsub(pattern = "mayores65_9_", 
                      replacement = "", x = names(ind_9))
  
  ind_10 <- mayores65 %>% dplyr::select(registro, which(startsWith(names(mayores65), "mayores65_10_")))
  names(ind_10) = gsub(pattern = "mayores65_10_", 
                       replacement = "", x = names(ind_10))
  
  
  loop_mayores65 <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
                          ind_8, ind_9, ind_10)
  
  
  loop_mayores65 <- filter(loop_mayores65, loop_mayores65$nombre != "_")
  
  
  return(loop_mayores65)
}