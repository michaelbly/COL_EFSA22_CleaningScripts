r <- df

# preliminary correlation analaysis
r$fcs <- 
  (as.numeric(r$fcs_cereales)*2) +(as.numeric(r$fcs_leguminosas)*3) +(as.numeric(r$fcs_leche)*4) + (as.numeric(r$fcs_carne)*4)+ 
  as.numeric(r$fcs_vegetales) + as.numeric(r$fcs_frutas) + (as.numeric(r$fcs_grasas)*0.5) + (as.numeric(r$fcs_azucares)*0.5)


r$poor_fcs <- ifelse(r$fcs <= 21, 1,0)
r$borderline_fcs <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
r$acceptable_fcs <- ifelse(r$fcs > 35,1,0)

table(r$poor_fcs, r$sexo_jh)

r$fcs_group<- case_when(r$fcs <= 21 ~ "poor_fcs", 
                  r$fcs > 21 & r$fcs <=35 ~ "borderline_fcs",
                  r$fcs > 35 ~ "acceptable_fcs")
