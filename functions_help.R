rm(list=ls(all=T))
R.version
library(rlang)
library(xlsx)
library(plyr) # rbind.fill
library(dplyr)
library(expss)
library(reshape)
library(data.table)
library(miceadds)
library(questionr)
library(koboquest) # manage kobo questionnaires
library(kobostandards) # check inputs for inconsistencies
library(surveyweights) # calculate weights from samplingframes
library(hypegrammaR) # simple stats 4 complex samples
library(composr) # horziontal operations
response <- read.csv("Input/hh_dataset.csv", 
                 stringsAsFactors=F, check.names=T,
                 sep=";")
#############################################################
#1) BASICS
#############################################################
rm(list=ls(all=T))
source("R/functions/postprocessing_functions.R")

#Change column name
names(response)[names(response) == 'X_uuid'] <- "uuid"


# Load input
questions <- read.csv("input/questionnaire/kobo_questions.csv", 
                      stringsAsFactors=T, check.names=T)
colnames(questions)[1] <- "type"
loop <- read.csv("Input/datasets/cleaned/loop.csv", 
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))
write.csv(indesign_full, "output/indesign_full_border_2.csv")


#Row and column bind
response <- plyr::rbind.fill(representative_hh, indicative_hh)
indesign_full<-cbind(pop_group_averages, extract_indesign, mergetop)


#Cool names
samplingframe$district <- to_alphanumeric_lowercase(samplingframe$district)


# Merge factors together
response$stratum <- paste0(response$region, response$oslo_area)
samplingframe_in_camp$stratum<-paste0(samplingframe_in_camp$camp,"idp_in_camp")
response <- response %>% 
  mutate(strata = paste0(district_mcna,population_group))


# Merge two dataframes
r <- merge(r, ila_analysis, by="strata", all.x = T)


# Select columns and change names
ila_analysis <- ila_analysis[,c("district_ocha", "critical_ranking_idp", "critical_ranking_ret")]
colnames(ila_analysis) <- c("district_mcna", "idp_out_camp", "returnee")


# Function
recoding_hno <- function(r, loop){
  return(r)
}
response_with_composites <- recoding_hno(response, loop)




#############################################################
# 2) RECODING
#############################################################
response <- response %>% mutate(s_6 = case_when(
  response$perc_unemp == 0 ~ 1,
  response$perc_unemp > 0 & r$perc_unemp <= 0.5 ~ 2,
  response$perc_unemp > 0.5 & r$perc_unemp <= 0.7 ~ 3,
  response$perc_unemp > 0.7 & r$perc_unemp <= 0.9 ~ 4,
  response$perc_unemp > 0.9 ~ 5
))

r <- r %>% mutate(s_7 = case_when(
  is.na(r$reasons_for_debt) ~ 1,
  r$reasons_for_debt %in% c("", "other", "purchase_pro_assets", "clothing") ~ 1,
  r$reasons_for_debt %in% c("house") ~ 2,
  r$reasons_for_debt %in% c("education", "basic_hh_expenditure") ~ 3,
  r$reasons_for_debt %in% c("food", "health") ~ 4
))

r$hno_severity_5 <- ifelse(r$final_severity == 5, 1,0)

r$female_headed <- case_when(r$hhh == "yes" & r$gender_respondent == "female"| 
                               r$gender_hhh == "female" ~ "female_headed",
                             TRUE ~ "male_headed")

r$in_camp_refugee <- case_when(r$refugee_status == "yes" & grepl("camp", r$strata) ~ "in_camp_refugee",
                               r$refugee_status == "yes" & !grepl("camp", r$strata) ~ "out_camp_refugee",
                               TRUE ~ NA_character_)


# Row-wise loop function
hno$critical <-  apply(hno, 1, function(y) {
  max(y[c("s_3", "s_11", "s_24", "s_7")], na.rm = T)
})

hno$mean <-  apply(hno, 1, function(y) {
  round2(mean(tail(sort(y), (floor(ncol(hno)/2)))))
})


# Select columns that start with xy
hno <-  r[c(which(startsWith(names(r), "s_")))]  
# Select cases that contain certain name with grepl
r$in_camp_refugee <- case_when(r$refugee_status == "yes" & grepl("camp", r$strata) ~ "in_camp_refugee",
                               r$refugee_status == "yes" & !grepl("camp", r$strata) ~ "out_camp_refugee",
                               TRUE ~ NA_character_)


# Sum two columns
summs$minor_total <- as.numeric(apply(summs[,c("female_0_17", "male_0_17")], 
                                      1, sum))




#############################################################
# 3) CLEANING
#############################################################

#DELETE COLUMNS
response[ ,c("calc_idp", "calc_returnee", "ind_level", "calc_separated", "exp_compare", 
             "calc_expenditure")] <- list(NULL)


# Character to numeric
summs$female_60 <- as.numeric(as.character(summs$female_60))
subset_age %<>% mutate_if(is.character,as.numeric)


#Change date to meaningful
df$today <- as.Date(df$start, "%Y-%m-%d")

# Check numeric data with histograms to identify outliers or < 0
hist(response$age_respondent)

#Cool names
samplingframe$district <- to_alphanumeric_lowercase(samplingframe$district)


####NAs#########
#Check for missing values in entire dataframe
any(is.na(response))
#Check for the number of missing values in dataframe
sum(is.na(response))
# number of NAs check
response$NAs <- apply(response,1,FUN=function(x){length(which(is.na(x)))})
#Eliminate missing values from particular column
na.omit(data$variable)
# Remove all rows with NA in specific variable
response <- response %>% drop_na(weights)
#Only keep those that are not NA
response <- response[!is.na(response$refugee_status), ]
#Number of NAs in one column
table(is.na(response$gender_hhh))
#Replace NAs in column with 0s
data$variable[is.na(data$variable)]<-0
#identify rows with NA
rownames(response)[apply(response, 2, anyNA)]
#Percent NAs per variable
pctmiss <- colSums(is.na(response))/nrow(response)
round(pctmiss, 2)


#Clean column names
clean <- clean_names(data)


#Remove empty columns or rows
clean_x <- response %>% remove_empty(whc=c("rows"))
clean_x <- response %>% remove_empty(whc=c("cols"))


#Remove duplicate rows
response <- distinct(response)
#Remove rows if duplicate in specific column
response2 <- response %>% distinct(age_respondent, .keep_all=T)


#Select specific variables
newdata <- select(response, age_respondent, gender_hhh, region)


#Filter Values
newdata <- filter(starwars, 
                  gender == "female" &
                  homeworls == "Alderaan")
newsdata <- filter(starwars, 
                   homeworls %in% c("Alderaan", "Courscant", "Endor"))




#############################################################
#4) DATA ANALYSIS
#############################################################

#Calculate mean height and weight by gender
newdata <- group_by(response, gender_hhh)
newdata <- summarize(newdata, 
                     mean_age = mean(age_respondent, na.rm=T), 
                     mean_nrmale = mean(tot_male, na.rm=T))
newdata


#Frequency Tables Single Variable
table(response$region)
prop.table(table(response$region))

#Frequency Tables Multiple Variables
prop.table(table(response$region, response$gender_respondent))
round(prop.table(table(response$region, response$gender_respondent))*100, 2)


#Weighted Analysis
#First create binary variables and make sure that there's no NA in the weights column
weighted.mean(response$female_headed, response$weights)
#Multiple Variables
response$below_poverty <- ifelse(response$tot_income <= 1000, 1,0)
response$above_poverty <- ifelse(response$tot_income > 1000, 1,0)
response$below_poverty[is.na(response$below_poverty)]<-0
response$above_poverty[is.na(response$above_poverty)]<-0
newdata <- group_by(response, region)
newdata <- summarize(newdata, 
                     pov_above = weighted.mean(above_poverty, weights), 
                     pov_below = weighted.mean(below_poverty, weights))
newdata


#Aggregate Values by group
#Single Group
aggregate(fes ~ region, response, mean)
#Multiple Groups
aggregate(fes_high ~ region + gender_hhh, response, mean)

aggregate(wfp_prices[c("price", "usdprice")], 
          list(wfp_prices$admin2, wfp_prices$admin1, wfp_prices$category), mean) #Admin2 is the primary group



#############################################################
#5) DATA VISUALIZATION
#https://rkabacoff.github.io/datavis/Time.html
#############################################################
#UNIVARIATE GRAPHS
#Categorical --> Bar Chart & Pie Charts & Tree Maps
#Quantitative --> Historgram & Density Plot 
###############################
#CATEGORICAL: Bar Chart
ggplot(response, aes(x = gender_hhh)) + 
  geom_bar(fill = "cornflowerblue",  #inside of bars
           color="black") + #outside borders of bars
  labs(x = "Gender HHH", 
       y = "Frequency", 
       title = "Participants by race")

#Percentage in descending order
plotdata <- response %>%
  count(oslo_area) %>%
  mutate(pct = n / sum(n),
         pctlabel = paste0(round(pct*100), "%"))

ggplot(plotdata, 
       aes(x = reorder(oslo_area, -pct),
           y = pct)) + 
  geom_bar(stat = "identity", 
           fill = "indianred3", 
           color = "black") +
  geom_text(aes(label = pctlabel), 
            vjust = -0.25) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Race", 
       y = "Percent", 
       title  = "Participants by race")


#CATEGORICAL: Pie Chart
plotdata <- response %>%
  count(oslo_area) %>%
  arrange(desc(oslo_area)) %>%
  mutate(prop = round(n * 100 / sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5  *prop)

ggplot(plotdata, 
       aes(x = "", 
           y = prop, 
           fill = oslo_area)) +
  geom_bar(width = 1, 
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = oslo_area), 
            color = "black") +
  coord_polar("y", 
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") +
  labs(title = "Participants by race")


#QUANTITATIVE: Histogram
ggplot(response, aes(x = age_respondent)) +
  geom_histogram(fill = "cornflowerblue", 
                 color = "white") + 
  labs(title="Participants by age",
       x = "Age")


#QUANTITATIVE: Density Plot
ggplot(response, aes(x = age_respondent)) +
  geom_density(fill = "indianred3") + 
  labs(title = "Participants by age")



#############################################################
#BIVARIATE GRAPHS
#Categorical vs. Categorical --> Stacked Bar Chart & Segmented Bar Chart
#Quantitative vs. Quantitative --> Scatterplots & Lineplots
#Categorical vs. Quantitative --> Bar Chart & Grouped Density Plots
###############################
#CATEGORICAL VS. CATEGORICAL: Stacked Bar Charts
ggplot(response, 
       aes(x = region, 
           fill = gender_respondent)) + 
  scale_fill_brewer(palette = "Blues") +
  geom_bar(position = "stack") 


#CATEGORICAL VS. CATEGORICAL: Segmented Bar Charts
library(scales)
ggplot(response, 
       aes(x = region, 
           fill = gender_respondent)) + 
  geom_bar(position = "fill") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Blues") +
  labs(y = "Proportion")



#QUANTITATIVE VS. QUANTITATIVE: Scatterplots
ggplot(response, 
       aes(x = tot_expenses, 
           y = tot_income)) +
  geom_point(color= "royalblue3") +
  geom_smooth(color = "pink4")


#QUANTITATIVE VS. QUANTITATIVE: Line Plots
ggplot(response, 
       aes(x = tot_expenses, 
           y = tot_income)) +
  geom_line() 



#CATEGORICAL VS. QUANTITATIVE: Bar Chart
plotdata <- response %>%
  group_by(oslo_area) %>%
  summarize(mean_income = mean(tot_income))

ggplot(plotdata, 
       aes(x = oslo_area, 
           y = mean_income)) +
  geom_bar(stat = "identity")


#CATEGORICAL VS. QUANTITATIVE: Grouped Density Plots
ggplot(response, 
       aes(x = tot_income, 
           fill = oslo_area)) +
  geom_density(alpha = 0.4) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Salary distribution by rank")


#CATEGORICAL VS. QUANTITATIVE: Multiple Boxplots
ggplot(response, mapping = aes(x = region, y = tot_income)) +
  geom_boxplot()



#############################################################
#MULTIVARIATE GRAPHS
###############################
#Two Quantitative Variables Grouped by Categorical Variable
ggplot(response, aes(x = tot_expenses, 
                     y = tot_income, 
                     color=oslo_area)) +
  geom_point() +
  labs(title = "Academic salary by rank and years since degree")


#Three Quantitative Variables Grouped by Categorical Variable
ggplot(response, 
       aes(x = tot_expenses, 
           y = tot_income, 
           color = oslo_area, 
           size = hh_size)) +
  scale_colour_manual(values = c("pink2", "steelblue3", "navyblue", "skyblue"))+
  geom_point(alpha = .6) +
  labs(title = "Academic salary by rank, years of service, and years since degree")


########################
#Side by Side Comparison
########################
#Point Diagram
p <- response %>% 
  ggplot(mapping = aes(x = gender_hhh, 
                       y = tot_income, 
                       color = gender_hhh,
                       fill = gender_hhh))
p + geom_point(size = 3) +
  scale_colour_manual(values = c("dodgerblue1", "dodgerblue3", "dodgerblue4"))


p + geom_jitter(width = 0.2) +
  scale_colour_manual(values = c("steelblue1", "steelblue2", "steelblue3"))+
  ggtitle("Zusammenhang zwischen Stress und Zufriedenheit") +
  xlab("Psychischer Stress")+
  ylab("Zufriedenheit") +
  labs(color = "Geschlecht",
       shape = "Geschlecht")

p + geom_boxplot() 

