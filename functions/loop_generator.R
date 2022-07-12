loop_generator <- function(df) {
  

ind_1 <- df %>% select(registro, which(startsWith(names(df), "ind1_")))
names(ind_1) = gsub(pattern = "ind1_", 
                    replacement = "", x = names(ind_1))
df[c(which(startsWith(names(df), "ind1_")))] <- NULL


ind_2 <- df %>% select(registro, which(startsWith(names(df), "ind2_")))
names(ind_2) = gsub(pattern = "ind2_", 
                    replacement = "", x = names(ind_2))
df[c(which(startsWith(names(df), "ind2_")))] <- NULL


ind_3 <- df %>% select(registro, which(startsWith(names(df), "ind3_")))
names(ind_3) = gsub(pattern = "ind3_", 
                    replacement = "", x = names(ind_3))
df[c(which(startsWith(names(df), "ind3_")))] <- NULL


ind_4 <- df %>% select(registro, which(startsWith(names(df), "ind4_")))
names(ind_4) = gsub(pattern = "ind4_", 
                    replacement = "", x = names(ind_4))
df[c(which(startsWith(names(df), "ind4_")))] <- NULL


ind_5 <- df %>% select(registro, which(startsWith(names(df), "ind5_")))
names(ind_5) = gsub(pattern = "ind5_", 
                    replacement = "", x = names(ind_5))
df[c(which(startsWith(names(df), "ind5_")))] <- NULL


ind_6 <- df %>% select(registro, which(startsWith(names(df), "ind6_")))
names(ind_6) = gsub(pattern = "ind6_", 
                    replacement = "", x = names(ind_6))
df[c(which(startsWith(names(df), "ind6_")))] <- NULL


ind_7 <- df %>% select(registro, which(startsWith(names(df), "ind7_")))
names(ind_7) = gsub(pattern = "ind7_", 
                    replacement = "", x = names(ind_7))
df[c(which(startsWith(names(df), "ind7_")))] <- NULL


ind_8 <- df %>% select(registro, which(startsWith(names(df), "ind8_")))
names(ind_8) = gsub(pattern = "ind8_", 
                    replacement = "", x = names(ind_8))
df[c(which(startsWith(names(df), "ind8_")))] <- NULL


ind_9 <- df %>% select(registro, which(startsWith(names(df), "ind9_")))
names(ind_9) = gsub(pattern = "ind9_", 
                    replacement = "", x = names(ind_9))
df[c(which(startsWith(names(df), "ind9_")))] <- NULL


ind_10 <- df %>% select(registro, which(startsWith(names(df), "ind10_")))
names(ind_10) = gsub(pattern = "ind10_", 
                    replacement = "", x = names(ind_10))
df[c(which(startsWith(names(df), "ind10_")))] <- NULL


ind_11 <- df %>% select(registro, which(startsWith(names(df), "ind11_")))
names(ind_11) = gsub(pattern = "ind11_", 
                    replacement = "", x = names(ind_2))
df[c(which(startsWith(names(df), "ind11_")))] <- NULL


ind_12 <- df %>% select(registro, which(startsWith(names(df), "ind12_")))
names(ind_12) = gsub(pattern = "ind12_", 
                    replacement = "", x = names(ind_12))
df[c(which(startsWith(names(df), "ind12_")))] <- NULL


ind_13 <- df %>% select(registro, which(startsWith(names(df), "ind13_")))
names(ind_13) = gsub(pattern = "ind13_", 
                    replacement = "", x = names(ind_13))
df[c(which(startsWith(names(df), "ind13_")))] <- NULL


ind_14 <- df %>% select(registro, which(startsWith(names(df), "ind14_")))
names(ind_14) = gsub(pattern = "ind14_", 
                    replacement = "", x = names(ind_14))
df[c(which(startsWith(names(df), "ind14_")))] <- NULL


ind_15 <- df %>% select(registro, which(startsWith(names(df), "ind15_")))
names(ind_15) = gsub(pattern = "ind15_", 
                    replacement = "", x = names(ind_15))
df[c(which(startsWith(names(df), "ind15_")))] <- NULL


ind_16 <- df %>% select(registro, which(startsWith(names(df), "ind16_")))
names(ind_16) = gsub(pattern = "ind16_", 
                    replacement = "", x = names(ind_16))
df[c(which(startsWith(names(df), "ind16_")))] <- NULL


ind_17 <- df %>% select(registro, which(startsWith(names(df), "ind17_")))
names(ind_17) = gsub(pattern = "ind17_", 
                    replacement = "", x = names(ind_17))
df[c(which(startsWith(names(df), "ind17_")))] <- NULL


ind_18 <- df %>% select(registro, which(startsWith(names(df), "ind18_")))
names(ind_18) = gsub(pattern = "ind18_", 
                    replacement = "", x = names(ind_18))
df[c(which(startsWith(names(df), "ind18_")))] <- NULL


ind_19 <- df %>% select(registro, which(startsWith(names(df), "ind19_")))
names(ind_19) = gsub(pattern = "ind19_", 
                    replacement = "", x = names(ind_19))
df[c(which(startsWith(names(df), "ind19_")))] <- NULL


ind_20 <- df %>% select(registro, which(startsWith(names(df), "ind20_")))
names(ind_20) = gsub(pattern = "ind20_", 
                    replacement = "", x = names(ind_20))
df[c(which(startsWith(names(df), "ind20_")))] <- NULL


loop <- rbind(ind_1, ind_2, ind_3, ind_4, ind_5, ind_6, ind_7,
               ind_8, ind_9, ind_10, ind_11, ind_12, ind_13,
               ind_14, ind_15, ind_16, ind_17, ind_18, ind_19, ind_20)
  

loop <- loop[!is.na(loop$edad),]


return(loop)
}
