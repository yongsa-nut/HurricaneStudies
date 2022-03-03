## Author: Nutchanon Yongsatianchot
## Hurricane Questionnaire Analysis: Florence and Michael 


library(readr)
library(ggplot2)
library(maps)
library(mapdata)
library(rstan)
library(plyr)
library(dplyr)
library(loo)
library(brms)
library(reshape2)
library(scales) # for muted function

setwd("~/Hurricane Experiments All Materials")
## Helper functions 

#### Adding child_0_5, child_6_12, child_13_18, elder_65_up
count_age_range <- function(lower,upper,subj)
{
  fam_size <- as.numeric(subj[which(names(subj) == "Q16")]) #Q16 == family size
  remain_member <- fam_size - 1 ## exclude the subject 
  ## living alone
  if(remain_member == 0) { return(0) }
  ## max allow number of family is 10
  if(remain_member > 9) { remain_member <- 9 }
  # q17_index = starting index of q17 + jumpindex [2*(n*n-1)/2]
  q17_index <- which(names(subj) == "Q17_1#1_1_1") + (remain_member * (remain_member-1)) 
  # test in rare case of missing data even though it's force response question
  if(is.na(subj[q17_index])) return(NA)
  # grab all the index
  age_list <- as.numeric( subj[q17_index : (q17_index+remain_member-1)] )
  #adjust age that is in calendar year
  cur_year <- 2018
  for(j in 1:length(age_list))
  {
    if(age_list[j] > 1900)  age_list[j] <- cur_year - age_list[j]
  }
  count <- sum( age_list >= lower & age_list <=upper)
  return(count)
}
adding_child_elder <- function(dat)
{
  temp0_5 <- c()
  temp6_12 <- c()
  temp13_18 <- c()
  temp65_up <- c()
  for(i in 1:dim(dat)[1])
  {
    temp0_5   <- c(temp0_5,count_age_range(0,5,dat[i,]))
    temp6_12  <- c(temp6_12,count_age_range(6,12,dat[i,]))
    temp13_18 <- c(temp13_18,count_age_range(13,18,dat[i,]))
    temp65_up    <- c(temp65_up,count_age_range(65,120,dat[i,]))
  }
  dat$child_0_5 <- temp0_5
  dat$child_6_12 <- temp6_12
  dat$child_13_18 <- temp13_18
  dat$elder_65_up <- temp65_up
  return(dat)
}


## Data Loading and Cleaning ##

###  Florence  ###
#### There are 3 variant of post florence questions due to minor typo

post_florence1 <- read_csv("Hurricane Survey Data/Post-Hurricane Survey - Hurricane Florence.csv")
post_florence2 <- read_csv("Hurricane Survey Data/Post-Hurricane Survey - Hurricane Florence - Fixed Minor Typo.csv")
post_florence3 <- read_csv("Hurricane Survey Data/Post-Hurricane Survey - Hurricane Florence - Fixed Minor Typo 2.csv")

##Remove the first two rows which contains full questions and markers.
post_florence1 <- post_florence1[-c(1,2),]
post_florence2 <- post_florence2[-c(1,2),]
post_florence3 <- post_florence3[-c(1,2),]

post_florence1 <- post_florence1[which(post_florence1$Finished == "True" ),]
post_florence2 <- post_florence2[which(post_florence2$Finished == "True" ),]
post_florence3 <- post_florence3[which(post_florence3$Finished == "True"),]

## Construct new dataframe for Pre and Post Hurricane
## The main purpose is to rename all the question number from Qualtrics

post_florence1 <- adding_child_elder(post_florence1)
post_florence2 <- adding_child_elder(post_florence2)
post_florence3 <- adding_child_elder(post_florence3)

gen_post_dat <- function(dat)
{
  dat_temp <- data.frame( 
    ## response characteristic
    "duration"  = as.numeric(dat$`Duration (in seconds)`),
    "EndDate"   = dat$EndDate,
    "MTurkCode" = as.numeric(dat$MTurkCode),
    "zipcode" = dat$Q9,
    
    ## Demoghahic 
    "age"               = as.numeric(dat$Q1),
    "gender"            = dat$Q2,
    "edu"               = dat$Q4,
    "income"            = dat$Q5,
    "house_structure"   = dat$Q7,
    "is_owned"          = dat$Q8,
    "city"              = dat$QCity_Q9,
    "state"             = dat$QState_Q9,
    "years_in_resident" = dat$Q10,
    "distance_to_coast" = dat$Q11,
    "num_vehicle"       = dat$Q12,
    "has_pet"           = dat$Q13,
    "type_of_pet"       = dat$Q14,
    "importance_of_pet" = dat$Q15,
    "family_size"       = as.numeric(dat$Q16),
    "child_0_5"         = as.factor(dat$child_0_5),
    "child_6_12"        = as.factor(dat$child_6_12),
    "child_13_18"       = as.factor(dat$child_13_18),
    "elder_65_up"       = as.factor(dat$elder_65_up),
    "special_need_med"  = dat$Q18,
    "special_need_moving" = dat$Q19,
    
    ## social influence
    "contribute_final_decision" = dat$Q20,
    "household_discussion"      = dat$Q21,
    "household_agreement"       = dat$Q22,
    "outside_family_discussion" = dat$Q23,
    "outside_family_agreement"  = dat$Q24,
    "outside_friend_discussion" = dat$Q25,
    "outside_friend_agreement"  = dat$Q26,
    "neighbors_observation"     = dat$Q27,
    "neighbors_agreement"       = dat$Q28,
    
    ## prev. experience and preparation
    "prev_exp"             = dat$Q31,
    "prev_exp_last_time"   = dat$Q32,
    "prev_exp_highest_cat" = dat$Q33,
    "prev_exp_notice"      = dat$Q34,
    "prev_exp_decision"    = dat$Q35,
    "prev_exp_same_choice" = dat$Q36,
    
    ## probability estimation 
    "safety_prob"   = dat$Q43,
    
    ## numerical estimation
    "flood_depth"             = dat$Q51,
    "electricity_lost_dur"    = dat$Q52,
    "normal_condition_return" = dat$Q53,
    "damage_est"              = as.numeric(dat$Q55),
    "hurricane_category"      = dat$Q50,
    
    ## info section
    "received_TV_Radio"    = dat$Q77,
    "TVR_dmg_done"         = dat$Q78_1,
    "TVR_forecasted_dmg"   = dat$Q78_2,
    "TVR_causualities"     = dat$Q78_3,
    "TVR_traffic_jams"     = dat$Q78_4,
    "TVR_preparing"        = dat$Q78_5,
    "TVR_stay"             = dat$Q78_6,
    "TVR_evac"             = dat$Q78_7,
    
    "received_social_med"  = dat$Q79,
    "SM_dmg_done"          = dat$Q80_1,
    "SM_forecasted_dmg"    = dat$Q80_2,
    "SM_causualities"      = dat$Q80_3,
    "SM_traffic_jams"      = dat$Q80_4,
    "SM_preparing"         = dat$Q80_5,
    "SM_stay"              = dat$Q80_6,
    "SM_evac"              = dat$Q80_7,
    
    ## post section
    
    "emergency_request_before" = dat$Q105_1,
    "emergency_request_during" = dat$Q105_2,
    "emergency_request_after"  = dat$Q105_3,
    "first_hear_florence"      = dat$Q81,
    "official_notice"          = dat$Q83,
    "notice_type"              = as.character(dat$Q84),
    "notice_when"              = dat$Q85,
    "evac_decision"            = dat$Q86,
    "same_choice"              = dat$Q106,
    
    ## Post - evac
    "date_evac"   = dat$Q87,
    "time_evac"   = dat$Q88,
    "how_evac"    = dat$Q89,
    "where_evac"  = dat$Q90,
    "how_far_evac" = dat$Q91,
    "traveling_cost_evac" = as.numeric(dat$Q92),
    "place_cost_evac" = as.numeric(dat$Q93),
    
    ## Post - stay
    "prepare_stay"        = dat$Q100,
    "when_prepare_stay"   = dat$Q101,
    "amount_prepare_stay" = dat$Q102,
    "traveling_est_stay"  = as.numeric(dat$Q103),
    "place_cost_est_stay" = as.numeric(dat$Q104)
  )
  return(dat_temp)
}

dat_post1 <- gen_post_dat(post_florence1)
dat_post2 <- gen_post_dat(post_florence2)
dat_post3 <- gen_post_dat(post_florence3)

#### fixing post1 wording on notice type: replace remain to stay
dat_post1$notice_type <- gsub("remain","stay",dat_post1$notice_type)

dat_post <- rbind(dat_post1,dat_post2,dat_post3)

#### Separate 4 notice types into 4 features
#### Types: Mandatory evacutation notice, Voluntary (Suggested) evacuation notice, 
####        Voluntary (Suggested) stay notice, Mandatory stay notice
dat_post$mandatory_evac_notice <- ifelse(grepl("Mandatory evacuation",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$voluntary_evac_notice <- ifelse(grepl("Voluntary (Suggested) evacuation",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$mandatory_stay_notice <- ifelse(grepl("Mandatory stay",dat_post$notice_type,fixed=TRUE),1,0)
dat_post$voluntary_stay_notice <- ifelse(grepl("Voluntary (Suggested) stay",dat_post$notice_type,fixed=TRUE),1,0)

###### compare with table(dat_post$notice_type) (pass)

#### Cleaning end date: date format YYYY:DD:MM HH:mm:ss
dat_post$EndDateTime <- substring(dat_post$EndDate,12,20)
dat_post$EndDate <- substring(dat_post$EndDate,1,10)

post_florence <- dat_post
post_florence <- post_florence[which(post_florence$state == "NC" | post_florence$state == "SC"),]

dim(post_florence) ## should be 716

###  Michael  ###

post_michael <- read_csv("Hurricane Survey Data/Post Hurricane Survey - Hurricane Michael.csv")

dat <- post_michael

dat <- dat[3:dim(post_michael)[1],]
dat <- dat[which(dat$Finished == "True"),]

# Construct a new dataframe

dat <- adding_child_elder(dat)

dat_michael <- data.frame( 
  ## response characteristic
  "duration"  = as.numeric(dat$`Duration (in seconds)`),
  "EndDate"   = dat$EndDate,
  "MTurkCode" = as.numeric(dat$MTurkCode),
  "zipcode" = dat$Q9,
  
  ## Demoghahic 
  "age"               = as.numeric(dat$Q1),
  "gender"            = dat$Q2,
  "employment"        = dat$Q3, ## new features for post michael
  "edu"               = dat$Q4,
  "income"            = dat$Q5,
  "house_structure"   = dat$Q7,
  "is_owned"          = dat$Q8,
  "city"              = dat$QCity_Q9,
  "state"             = dat$QState_Q9,
  "years_in_resident" = dat$Q10,
  "distance_to_coast" = dat$Q11,
  "num_vehicle"       = dat$Q12,
  "has_pet"           = dat$Q13,
  "type_of_pet"       = dat$Q14,
  "importance_of_pet" = dat$Q15,
  "family_size"       = as.numeric(dat$Q16),
  "child_0_5"         = as.factor(dat$child_0_5),
  "child_6_12"        = as.factor(dat$child_6_12),
  "child_13_18"       = as.factor(dat$child_13_18),
  "elder_65_up"       = as.factor(dat$elder_65_up),
  "special_need_med"  = dat$Q18,
  "special_need_moving" = dat$Q19,
  
  ## social influence
  "contribute_final_decision" = dat$Q20,
  "household_discussion"      = dat$Q21,
  "household_suggestion"       = dat$Q22,
  "outside_family_discussion" = dat$Q23,
  "outside_family_suggestion"  = dat$Q24,
  "outside_friend_discussion" = dat$Q25,
  "outside_friend_suggestion"  = dat$Q26,
  "neighbors_observation"     = dat$Q27,
  "neighbors_doing"       = dat$Q28,
  
  ## prev. experience and preparation
  "prev_exp"             = dat$Q31,
  "prev_exp_last_time"   = dat$Q32,
  "prev_exp_highest_cat" = dat$Q33,
  "prev_exp_notice"      = dat$Q34,
  "prev_exp_decision"    = dat$Q35,
  "prev_exp_same_choice" = dat$Q36,
  
  ## probability estimation 
  "safety_prob"   = dat$Q43,
  
  ## numerical estimation
  "flood_depth"             = dat$Q51,
  "electricity_lost_dur"    = dat$Q52,   ## Have to more fine gain
  "normal_condition_return" = dat$Q53,
  "flood_cost"              = as.numeric(dat$Q54),
  "electricity_cost"        = as.numeric(dat$Q55),
  "hurricane_category"      = dat$Q50,
  
  ## info section
  "received_TV_Radio"    = dat$Q77,
  "TVR_dmg_done"         = dat$Q78_1,
  "TVR_forecasted_dmg"   = dat$Q78_2,
  "TVR_causualities"     = dat$Q78_3,
  "TVR_traffic_jams"     = dat$Q78_4,
  "TVR_preparing"        = dat$Q78_5,
  "TVR_stay"             = dat$Q78_6,
  "TVR_evac"             = dat$Q78_7,
  
  "received_social_med"  = dat$Q79,
  "SM_dmg_done"          = dat$Q80_1,
  "SM_forecasted_dmg"    = dat$Q80_2,
  "SM_causualities"      = dat$Q80_3,
  "SM_traffic_jams"      = dat$Q80_4,
  "SM_preparing"         = dat$Q80_5,
  "SM_stay"              = dat$Q80_6,
  "SM_evac"              = dat$Q80_7,
  
  ## post section
  
  "emergency_request_before" = dat$Q105_1,
  "emergency_request_during" = dat$Q105_2,
  "emergency_request_after"  = dat$Q105_3,
  "first_hear_florence"      = dat$Q81,
  "official_notice"          = dat$Q83,
  "notice_type"              = as.character(dat$Q84),
  "notice_when"              = dat$Q85,
  "evac_decision"            = dat$Q86,
  "same_choice"              = dat$Q106,
  
  ## Post - evac
  "date_evac"   = dat$Q87,
  "time_evac"   = dat$Q88,
  "how_evac"    = dat$Q89,
  "where_evac"  = dat$Q90,
  "how_far_evac" = dat$Q91,
  "traveling_cost_evac" = as.numeric(dat$Q92),
  "place_cost_evac" = as.numeric(dat$Q93),
  "have_returned_home" = dat$Q94,
  "when_returned_home" = dat$Q95,
  
  ## Post - stay
  "prepare_stay"        = dat$Q100,
  "when_prepare_stay"   = dat$Q101,
  "amount_prepare_stay" = dat$Q102,
  "traveling_est_stay"  = as.numeric(dat$Q103),
  "place_cost_est_stay" = as.numeric(dat$Q104)
)

dat_michael$mandatory_evac_notice <- ifelse(grepl("Mandatory evacuation",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$voluntary_evac_notice <- ifelse(grepl("Voluntary (Suggested) evacuation",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$mandatory_stay_notice <- ifelse(grepl("Mandatory stay",dat_michael$notice_type,fixed=TRUE),1,0)
dat_michael$voluntary_stay_notice <- ifelse(grepl("Voluntary (Suggested) stay",dat_michael$notice_type,fixed=TRUE),1,0)

## Cleaning end date: date format YYYY:DD:MM HH:mm:ss
dat_michael$EndDateTime <- substring(dat_michael$EndDate,12,20)
dat_michael$EndDate <- substring(dat_michael$EndDate,1,10)


post_michael <- dat_michael
post_michael  <- post_michael[which(post_michael$state == "GA" | post_michael$state == "FL"),]

dim(post_michael) ## Should be 569


## Additional Data Processing ## 

## Demo Summary
# No change for
## age, gender, edu, income, house_structure, is_owned, years_in_resident
## distance_to_coast, num_vehicle, has_pet, family_size
## has_child, has_elder, special_need_med
post_florence$has_children  <- ifelse(post_florence$child_0_5 != 0, "yes","no")
post_michael$has_children   <- ifelse(post_michael$child_0_5 != 0, "yes","no")

post_florence$has_elder    <- ifelse(post_florence$elder_65_up != 0, "yes","no")
post_michael$has_elder     <- ifelse(post_michael$elder_65_up != 0, "yes","no")

## Model based
#"traveling_cost"            "safe_place_cost"          "safety_prob"               
#"flood_depth"               "electricity_lost_dur"     "normal_condition_return"  

#post florence
post_florence$traveling_cost <- ifelse(post_florence$evac_decision == "Yes", 
                                       post_florence$traveling_cost_evac, 
                                       post_florence$traveling_est_stay)
post_florence$safe_place_cost <- ifelse(post_florence$evac_decision == "Yes", 
                                        post_florence$place_cost_evac, 
                                        post_florence$place_cost_est_stay/7)   ##for stay it's for a week
#post michael
post_michael$traveling_cost <- ifelse(post_michael$evac_decision == "Yes", 
                                      post_michael$traveling_cost_evac, 
                                      post_michael$traveling_est_stay)




post_michael$safe_place_cost <- ifelse(post_michael$evac_decision == "Yes", 
                                       post_michael$place_cost_evac, 
                                       post_michael$place_cost_est_stay)   ## per day for michael


##Adjusting flood depth 
## 5 factors: None, <1, 1 -3 , 3 - 6, 6+
flooded_order <- c("Would not be flooded","1 foot or less","1 - 3 feet","3 - 6 feet","More than 6 feet")
levels(post_florence$flood_depth) <- flooded_order
levels(post_michael$flood_depth)  <- flooded_order

##Adjusting electricity lost duration
#electricity_order1 <- c("Will not lose electricity","Less than one week","1 - 2 weeks",
#                        "3 - 4 weeks","More than a month")
#electricity_lost_dur1 -> map 2 - 3 weeks to 3 - 4 weeks
post_florence$electricity_lost_dur1 <- mapvalues(post_florence$electricity_lost_dur,
                                                 from=c("More than a month"),
                                                 to = c("3 - 4 weeks"))

post_michael$electricity_lost_dur1 <- post_michael$electricity_lost_dur
post_michael$electricity_lost_dur1 <- mapvalues(post_michael$electricity_lost_dur1,
                                                from = c("2- 3 weeks","More than a month"),
                                                to = c("3 - 4 weeks","3 - 4 weeks")) 


## Electricity_lost_dur2 -> map 2 - 3 weeks to 1 - 2 weeks
post_florence$electricity_lost_dur2 <- mapvalues(post_florence$electricity_lost_dur,
                                                 from=c("More than a month"),
                                                 to = c("3 - 4 weeks"))

post_michael$electricity_lost_dur2 <- post_michael$electricity_lost_dur
post_michael$electricity_lost_dur2 <- mapvalues(post_michael$electricity_lost_dur2,
                                                from = c("2- 3 weeks","More than a month"),
                                                to = c("1 - 2 weeks","3 - 4 weeks")) 



post_florence$electricity_lost_dur <- post_florence$electricity_lost_dur1
post_michael$electricity_lost_dur <- post_michael$electricity_lost_dur1


##Adjusting normal condition return
#Normal condition return1 -> 2 -3 weeks to 3 -4 weeks
pre_florence$normal_condition_return1 <- pre_florence$normal_condition_return
post_florence$normal_condition_return1 <- post_florence$normal_condition_return

post_michael$normal_condition_return1 <- post_michael$normal_condition_return
post_michael$normal_condition_return1 <- mapvalues(post_michael$normal_condition_return1,
                                                   from = c("2 -3 weeks"),
                                                   to = c("3 -4 weeks"))


#Normal condition return2 -> 2 -3 weeks to 1 - 2 weeks
pre_florence$normal_condition_return2 <- pre_florence$normal_condition_return
post_florence$normal_condition_return2 <- post_florence$normal_condition_return

post_michael$normal_condition_return2 <- post_michael$normal_condition_return
post_michael$normal_condition_return2 <- mapvalues(post_michael$normal_condition_return2,
                                                   from = c("2 -3 weeks"),
                                                   to = c("1 - 2 weeks"))

post_florence$normal_condition_return <- post_florence$normal_condition_return1
post_michael$normal_condition_return  <- post_michael$normal_condition_return1

### Grouping and mapping
### Maping from questionnaire's answer to numerical value ##

### probability mapping ###
uniform_mapping <- c(0.125,0.25, 0.375,0.5,0.625,0.75,0.825)
normal_mapping  <- c(0.05 ,0.13, 0.32, 0.5,0.68, 0.87,0.95)
mixed_mapping   <- c(0.05 ,0.2125, 0.375, 0.5, 0.625, 0.7875, 0.95)

### Geometric series ##
### For calculating, discounted duration
Geometric_value <- function(discount, duration)
{
  if(discount == 1) { return(duration) }
  return( (1- (discount^duration))/(1 - discount)  )
}


Gen_prob_from_map <- function(prob_map, value)
{
  if(value == "Extremely unlikely")  { return(as.numeric(prob_map[1])) }
  if(value == "Moderately unlikely") { return(as.numeric(prob_map[2])) }
  if(value == "Slightly unlikely")   { return(as.numeric(prob_map[3])) }
  if(value == "Neither likely nor unlikely (50/50)") { return(as.numeric(prob_map[4]))  }
  if(value == "Slightly likely")     { return(as.numeric(prob_map[5])) }
  if(value == "Moderately likely")   { return(as.numeric(prob_map[6])) }
  if(value == "Extremely likely")    { return(as.numeric(prob_map[7])) }
}

### Flodd depth mapping ##
### (Note: missing 1 - 2 and 3 - 4) (Fix in Michael questionnaire)
Gen_flood_depth_mapping <- function(value)
{
  if(value == "Would not be flooded") {return(0)}
  if(value == "Will not be flooded") {return(0)}
  if(value == "1 foot or less")      {return(0.5)}
  if(value == "1 - 3 feet")          {return(2)}
  if(value == "2 - 3 feet")          {return(2.5)}
  if(value == "3 - 5 feet")          {return(4)}
  if(value == "4 - 5 feet")          {return(4.5)}
  if(value == "3 - 6 feet")          {return(4.5)}
  if(value == "More than 5 feet")    {return(6)}
  if(value == "More than 6 feet")    {return(6)}
}

### Duration Mapping ###
### This is different from other mapping, because of discount factor potentially.
### So, it returns a list instead. 
### Side effect: if discount becomes a param of the model, mapping has to occur inside validation.
### (Note: there is no 2-3 choice ... ) (Fix in Michael Questionnaire)
Gen_normal_duration_mapping <- function(discount = 1)
{
  temp_list <- list()
  temp_list[["Less than one week"]]  <- Geometric_value(discount, 3) 
  temp_list[["1 - 2 weeks"]]         <- Geometric_value(discount, 10.5)
  temp_list[["2 -3 weeks"]]          <- Geometric_value(discount, 17.5) 
  temp_list[["3 -4 weeks"]]          <- Geometric_value(discount, 24.5)       
  temp_list[["More than a month"]]   <- Geometric_value(discount, 30) 
  return(temp_list)
}
Gen_elec_duration_mapping <- function(discount = 1)
{
  temp_list <- list()
  temp_list[["Will not lose electricity"]] <- 0
  temp_list[["Less than one week"]]        <- Geometric_value(discount,3) 
  temp_list[["1 - 2 weeks"]]               <- Geometric_value(discount,10.5) 
  temp_list[["2- 3 weeks"]]                <- Geometric_value(discount,17.5) 
  temp_list[["3 - 4 weeks"]]               <- Geometric_value(discount,24.5)      
  temp_list[["More than a month"]]         <- Geometric_value(discount,30)
  return(temp_list)
}

family_size_group <- function(value)
{
  if(value >= 4) {return("4")}
  return(as.character(value))
}

age_group <- function(value)
{
  if(value >= 18 & value <= 30) { return("1") }
  if(value > 30 & value <= 45)  { return("2") }
  if(value > 45 & value <= 60)  { return("3") }
  if(value > 60) { return("4") }
}

z_norm <- function(x){ (x - mean(x))/sd(x) }


##Adding extra features based on the existing features
preprocessed_dat_for_brms <- function(df)
{
  ##Mapping Categorical answer to numerical
  df$safety_prob_num_uni     <- sapply(df$safety_prob, function(x) Gen_prob_from_map(uniform_mapping,x))
  df$safety_prob_num_normal  <- sapply(df$safety_prob, function(x) Gen_prob_from_map(normal_mapping,x))
  
  normal_dur_map <- Gen_normal_duration_mapping()
  df$normal_condition_return_num <- sapply(df$normal_condition_return, 
                                           function(x) normal_dur_map[[x]])
  
  electricity_duration_map <- Gen_elec_duration_mapping()
  df$electricity_lost_dur_num   <- sapply(df$electricity_lost_dur, 
                                          function(x) electricity_duration_map[[x]] )
  df$flood_depth_num <- sapply(df$flood_depth, 
                               function(x) Gen_flood_depth_mapping(x) )
  
  ## Transformation
  df$traveling_cost_z <- z_norm(df$traveling_cost)
  df$safe_place_cost_z <- z_norm(df$safe_place_cost)
  df$normal_condition_return_num_z <- z_norm(df$normal_condition_return_num)
  df$electricity_lost_dur_num_z    <- z_norm(df$electricity_lost_dur_num)
  df$flood_depth_num_z             <- z_norm(df$flood_depth_num)
  
  df$normal_condition_return_num_log  <-  log1p(df$normal_condition_return_num)
  df$electricity_lost_dur_num_log     <-  log1p(df$electricity_lost_dur_num)
  df$flood_depth_num_log              <-  log1p(df$flood_depth_num)
  df$traveling_cost_log               <-  log1p(df$traveling_cost)
  df$safe_place_cost_log              <-  log1p(df$safe_place_cost)
  
  df$normal_condition_return_num_log_z  <-  z_norm(log1p(df$normal_condition_return_num))
  df$electricity_lost_dur_num_log_z     <-  z_norm(log1p(df$electricity_lost_dur_num))
  df$flood_depth_num_log_z              <-  z_norm(log1p(df$flood_depth_num))
  df$traveling_cost_log_z               <-  z_norm(log1p(df$traveling_cost))
  df$safe_place_cost_log_z              <-  z_norm(log1p(df$safe_place_cost))
  
  #Total = depth * duration
  df$flood_total_z                <- z_norm(df$flood_depth_num*df$normal_condition_return_num)
  df$flood_total_log              <- df$flood_depth_num_log*df$normal_condition_return_num_log
  df$flood_total_log_z            <- z_norm(df$flood_total_log)
  #Total = cost x duration
  df$safe_place_cost_total_pure_z <- z_norm(df$safe_place_cost*df$normal_condition_return_num)
  df$safe_place_cost_total        <- df$safe_place_cost_log*df$normal_condition_return_num_log
  df$safe_place_cost_total_z      <- z_norm(df$safe_place_cost_log*df$normal_condition_return_num_log)
  df$safe_place_cost_total_log    <- log1p(df$safe_place_cost*df$normal_condition_return_num)
  df$safe_place_cost_total_log_z  <- z_norm(df$safe_place_cost_total_log)
  
  #Demo
  df$age_z <- z_norm(df$age)

  #Others (Filling rare cases with the most likely case)
  df$edu    <- mapvalues(df$edu, from = c("Some high school"), to = c("High school graduate"))
  df$gender <- mapvalues(df$gender, from = c("Other"), c("Female"))
  df$house_structure <- mapvalues(df$house_structure, from = c("Other"),
                                  to = c("Detached single family house"))
  df$income <- mapvalues(df$income, from = c("Over $150,000"),
                         to = c("$100,000 to $150,000")) 
  
  df$true_safety_prob   <- 1 - df$safety_prob_num_normal 
  df$safety_prob_num_normal_z <- z_norm(df$safety_prob_num_normal)
  df$true_safety_prob_z <- z_norm(df$true_safety_prob)
  
  ##prev 
  ## Fill in "NA"
  df$prev_exp_decision <- ifelse(df$prev_exp == "No","No experience", df$prev_exp_decision)

  ##Evac decision num
  df$evac_decision_num <- ifelse(df$evac_decision == "Yes",1,0)
  
  ##Group
  df$family_size_group <- sapply(df$family_size, function(x) family_size_group(x))
  df$age_group         <- sapply(df$age, function(x) age_group(x))
  df$evac_notice_group <- ifelse(df$mandatory_evac_notice == 1,"1",
                                 ifelse(df$voluntary_evac_notice == 1,"2","3"))
  df$income_group      <- mapvalues(df$income, from = c("Less than $20,000","$20,000 to $40,000","$40,000 to $60,000",
                                                        "$60,000 to $80,000","$80,000 to $100,000","$100,000 to $150,000"),
                                    to = c("poor","low","med","med","high","high"))
  
  return(df)
}

df_f <- preprocessed_dat_for_brms(post_florence)
df_m <- preprocessed_dat_for_brms(post_michael)

dim(df_f) ## Should still be 716
dim(df_m) ## should still be 569

## Exclusion ## 

## Minimal exclude all answer of any numerical questions (except family size) that are above 3 s.d.
## family size == 0 | family size >= 10 are exclude 
## duration spending on the questionnaire: exclude > one hour (3600 s) and under 3 mins (180 s)
## Only above because zero cost is the minimum lower bound
## List of numerical questions:
##   post ver: traveling_cost, safe_place_cost, family_size, 

exclusion1 <- function(dat, sd_threshold = 3, min_dur)
{
  to_exclude <- which(dat$traveling_cost >= 
                        (sd_threshold*sd(dat$traveling_cost) +
                           mean(dat$traveling_cost)) |
                        dat$safe_place_cost >= (sd_threshold*sd(dat$safe_place_cost) +
                                                  mean(dat$safe_place_cost)) |
                        dat$family_size >= 10 | 
                        dat$duration >= 3600 | dat$duration <= min_dur   )
  return(dat[-to_exclude,])
}


post_florence_exc <- exclusion1(post_florence,min_dur =180)
post_michael_exc  <- exclusion1(post_michael,min_dur =180)

df_f_exc     <- preprocessed_dat_for_brms(post_florence_exc)
df_m_exc     <- preprocessed_dat_for_brms(post_michael_exc)


dim(df_f_exc) ## Should be 684
dim(df_m_exc) ## should be 548

##save(df_f_exc,file =  "Data/post-proecess-post-florence_exc.Rda")
##save(df_m_exc, file = "Data/post-proecess-post-michael_exc.Rda")
load("Hurricane Survey Data/post-proecess-post-florence_exc.Rda")
load("Hurricane Survey Data/post-proecess-post-michael_exc.Rda")


## Data Analysis ##

## The results may be slightly different from the paper due to randomness 
## but all the trends should be the same

seed <- 7
prior_n <- c(prior(normal(0,5), class = b))

### Model declaration 

### Decision ~ Bel + Potential Confounders
### Potential Confounders: prev exp, evac notices, income, and distance to coast
m_prob   <- bf(evac_decision_num ~ 1 + true_safety_prob + prev_exp_decision + 
                 mandatory_evac_notice + voluntary_evac_notice + income + distance_to_coast)
m_flood  <- bf(evac_decision_num ~ 1 + flood_depth_num  + prev_exp_decision + 
                 mandatory_evac_notice + voluntary_evac_notice + income+ distance_to_coast)
m_outage <- bf(evac_decision_num ~ 1 + electricity_lost_dur_num + prev_exp_decision + 
                 mandatory_evac_notice + voluntary_evac_notice + income + distance_to_coast)
m_place  <- bf(evac_decision_num ~ 1 + safe_place_cost_log + prev_exp_decision + 
                 mandatory_evac_notice + voluntary_evac_notice + income + distance_to_coast)
m_travel <- bf(evac_decision_num ~ 1 + traveling_cost_log + prev_exp_decision + 
                 mandatory_evac_notice + voluntary_evac_notice + income + distance_to_coast)

m_list <- list(m_prob,m_flood,m_outage,m_place,m_travel)
m_list_name <- c("m_prob","m_flood","m_outage","m_place","m_travel")
plot_name <- c("prob safety","flood depth (feet)","outage duration (days)","safe place cost (log)","traveling cost (log)")

fit_coef <- function(dat,dat_name, models, model_name)
{
  f_list <- list() 
  for(m in 1:length(models))
  {
    f_list[[m]] <- brm(models[[m]], data = dat, prior = prior_n, family = bernoulli(), 
                       seed = seed, iter = 6000,
                       file = paste("model/",model_name[m],dat_name,sep=""), 
                       control = list(adapt_delta = 0.9))
  }
  return(f_list)
}

gen_coef_dat <- function(fit)
{
  dat_temp <- rbind(round(fixef(fit[[1]])[2,],2),
                    round(fixef(fit[[2]])[2,],2),
                    round(fixef(fit[[3]])[2,],2),
                    round(fixef(fit[[4]])[2,],2),
                    round(fixef(fit[[5]])[2,],2))
  row.names(dat_temp) <- c("prob safety","flood depth","outage duration","safe place cost","traveling cost")
  return(dat_temp)
}


f_post_flo_exc <- fit_coef(df_f_exc,"df_f_exc",m_list,m_list_name)
f_post_mic_exc <- fit_coef(df_m_exc,"df_m_exc",m_list,m_list_name)

coef_post_flo_exc <- gen_coef_dat(f_post_flo_exc)
coef_post_mic_exc <- gen_coef_dat(f_post_mic_exc)

coefs <- data.frame(coef_post_flo_exc,
                    coef_post_mic_exc)
coefs <- data.frame("F_Est" = coefs$Estimate,
                    "F_SE"  = coefs$Est.Error,
                    "F_95%Interval" = paste("[",as.character(coefs$Q2.5),", ",as.character(coefs$Q97.5),"]",sep=""),
                    "M_Est" = coefs$Estimate.1,
                    "M_SE"  = coefs$Est.Error.1,
                    "M_95%Interval" = paste("[",as.character(coefs$Q2.5.1),", ",as.character(coefs$Q97.5.1),"]",sep=""))
row.names(coefs) <- c("Safety prob","Flood depth","Outage duration","Safe place cost","Traveling cost")

View(coefs)

# kable(coefs)  %>%
#   kable_classic("striped", full_width = F) %>%
#   add_header_above(c(" " = 1, "Florence" = 3, "Michael" = 3, "Dorian" = 3)) %>%  
#   scroll_box(width = "100%")




## Accuracy Evaluation ##

### Helper functions for evacluation

### F-score
### true_positive = predict evac correctly, false negative = predict evac incorrectly (predict stay but actually evac)
### true_negative = predict stay correctly, false positive = predict stay incorrectly (predict evac but actually stay)
F_score <- function(true_positive, false_negative, true_negative, false_positive)
{
  ## Recall
  recall <- true_positive/(true_positive + false_negative)
  if(true_positive + false_negative == 0) { recall <- 0 }
  ## Precision
  precision <- true_positive/(true_positive + false_positive)
  if(true_positive + false_positive == 0) { precision <-0 }
  
  ## F score
  if(true_positive == 0| true_negative == 0) { f_score <- 0 }
  else { f_score <- (2*recall*precision)/(recall + precision) }
  
  
  return(list("recall" = recall, "precision" = precision, "f_score" = f_score))
}

plot_params <- function(x,type="areas", p = NA) 
{
  pplot <- mcmc_plot(x, pars = p,type=type,prob=0.95,prob_outer=1)
  pplot + geom_vline(xintercept=0)
}

loo_predictive_accuracy <- function(model, dat, predict)
{
  # Predicted probabilities
  preds <- posterior_linpred(model, transform=TRUE)
  pred <- colMeans(preds)
  pr <-  as.integer(pred >= 0.5)
  
  log_lik <- log_lik(model, parameter_name = "log_lik")
  #psis <- psislw(-log_lik)
  psis_temp <- psis(-log_lik)
  
  # LOO predictive probabilities
  #ploo <- colSums(preds*exp(psis$lw_smooth))
  ploo <- colSums(preds*exp( weights(psis_temp)))
  
  # LOO classification accuracy
  print(mean(xor(ploo>0.5, dat[[predict]])))
  
  tp <- sum(ploo[dat[[predict]]==1]>0.5)
  tn <- sum(ploo[dat[[predict]]==0]<0.5)
  fp <- sum(ploo[dat[[predict]]==0]>0.5)
  fn <- sum(ploo[dat[[predict]]==1]<0.5)
  
  acc <- (tp+tn)/(tp+tn+fp+fn)
  f_score <- F_score(tp, fn, tn,fp)
  return(c(acc,tp,tn,fp,fn, f_score$f_score))
}

predictive_accuracy_dat <- function(model,dat, predict)
{
  preds <- posterior_predict(model, newdata = dat)
  pred <- colMeans(preds)
  #print(mean((xor(pred>0.5, dat[[predict]]))))
  
  tp <- sum(pred[dat[[predict]]==1]>0.5)
  tn <- sum(pred[dat[[predict]]==0]<0.5)
  fp <- sum(pred[dat[[predict]]==0]>0.5)
  fn <- sum(pred[dat[[predict]]==1]<0.5)
  
  acc <- (tp+tn)/(tp+tn+fp+fn)
  f_score <- F_score(tp, fn, tn, fp)
  return(c(acc,tp,tn,fp,fn, f_score$f_score))
}

loo_pred_f <- function(dat,fit, predict)
{
  #LOO predict
  seed <- 7
  loo_pred <- brms::loo_predict(fit, seed = seed)
  loo_pred <- if_else(loo_pred > 0.5,1,0)
  print("Acc:")
  print(sum(loo_pred == dat[[predict]])/dim(dat)[1])
  
  print("F:")
  tp <- sum(loo_pred[dat[[predict]]==1]>0.5)
  tn <- sum(loo_pred[dat[[predict]]==0]<0.5)
  fp <- sum(loo_pred[dat[[predict]]==0]>0.5)
  fn <- sum(loo_pred[dat[[predict]]==1]<0.5)
  #print(tp,tn,fp,fn)
  print(table(loo_pred,dat[[predict]]))
  print(F_score(tp, fn, tn,fp))
}

loo_linp_f <- function(dat,fit, predict)
{
  #LOO predict
  seed <- 7
  loo_pred <- inv_logit_scaled(brms::loo_linpred(fit, seed = seed))
  loo_pred <- if_else(loo_pred > 0.5,1,0)
  acc <- (sum(loo_pred == dat[[predict]])/dim(dat)[1])
  
  tp <- sum(loo_pred[dat[[predict]]==1]>0.5)
  tn <- sum(loo_pred[dat[[predict]]==0]<0.5)
  fp <- sum(loo_pred[dat[[predict]]==0]>0.5)
  fn <- sum(loo_pred[dat[[predict]]==1]<0.5)
  
  f_temp <- (F_score(tp, fn, tn,fp))
  return(data.frame(acc = acc,
                    tp = tp,
                    fn = fn,
                    tn = tn,
                    fp = fp,
                    precision = f_temp$precision,
                    recall = f_temp$recall,
                    f_score = f_temp$f_score))
}

testing_pred <- function(dat,fit, predict)
{
  #Testing across group
  pred <- predict(fit, newdata= dat)
  pred <- if_else(pred[,1] > 0.5, 1, 0)
  table(pred,dat$evac_decision_num)
  #Acc
  acc <- (sum(pred  == dat[[predict]])/dim(dat)[1])
  
  tp <- sum(pred[dat[[predict]]==1]>0.5)
  tn <- sum(pred[dat[[predict]]==0]<0.5)
  fp <- sum(pred[dat[[predict]]==0]>0.5)
  fn <- sum(pred[dat[[predict]]==1]<0.5)
  
  f_temp <- (F_score(tp, fn, tn,fp))
  
  return(round(data.frame(acc = acc*100, ##percentage
                          precision = f_temp$precision,
                          recall = f_temp$recall,
                          f_score = f_temp$f_score),2))
}

gen_result_within <- function(models,models_name,dat,dat_name)
{
  results <- list()
  for(m in 1:length(models_name))
  {
    ## File name = ModelName_data
    file_name <- paste("model/",models_name[m],"_",dat_name,sep="")
    #f_temp <- brm(file = file_name)
    if(models_name[[m]] == "m_intercept") #No prior for intercept only
    {
      f_temp <- brm(models[[m]], data = dat, 
                  #prior = prior_n, 
                  family = bernoulli(), 
                  seed = seed, iter = 6000,
                  file = file_name, 
                  control = list(adapt_delta = 0.95))
    }else{
      f_temp <- brm(models[[m]], data = dat, 
                    prior = prior_n, 
                    family = bernoulli(), 
                    seed = seed, iter = 6000,
                    file = file_name, 
                    control = list(adapt_delta = 0.95))
    }
    loo_f_temp <- loo(f_temp)
    result <- loo_linp_f(dat,f_temp,"evac_decision_num")
    
    result_temp <- data.frame( dat = dat_name,
                               model = models_name[m], result, 
                               elpd_est = loo_f_temp$estimates[1,1],
                               elpd_se  = loo_f_temp$estimates[1,2])
    results <- rbind(results,result_temp)
  }
  return(results)
}


clean_up_within_result <- function(result_dat)
{
  result_dat$acc <- round(result_dat$acc*100,2)
  result_dat$precision <- round(result_dat$precision,2)
  result_dat$recall   <- round(result_dat$recall,2)
  result_dat$f_score <- round(result_dat$f_score,2)
  result_dat$elpd_est <- round(result_dat$elpd_est,2)
  result_dat$elpd_se <- round(result_dat$elpd_se,2)
  return(result_dat)
}

## Gen results 

m_intercept <- bf(evac_decision_num ~ 1)

m_demo <- bf(evac_decision_num ~ 1 + gender + age_z +  edu + income + house_structure 
             + is_owned + years_in_resident + distance_to_coast + num_vehicle  + has_pet + 
               family_size_z + has_children +  has_elder +  special_need_med)

m_demo_others <- bf(evac_decision_num ~  1 +
                      gender + age_z +  edu + income + house_structure 
                    + is_owned + years_in_resident + distance_to_coast 
                    + num_vehicle  + has_pet + family_size_z + has_children 
                    + has_elder +  special_need_med + 
                      prev_exp_decision +
                      mandatory_evac_notice + voluntary_evac_notice)

m_model_mo    <- bf(evac_decision_num ~ 1 + 
                        mo(safety_prob) + 
                        electricity_lost_dur_num_z +
                        flood_total_log_z + 
                        traveling_cost_log_z + 
                        safe_place_cost_log_z)


m_model_full  <- bf(evac_decision_num ~ 1 
                    
                    + (1 + true_safety_prob_z + flood_total_log_z|prev_exp_decision) 
                    + (1 + true_safety_prob_z + flood_total_log_z|age_group)
                    + (1 + safe_place_cost_log_z|income_group)
                    + (1 + true_safety_prob_z|distance_to_coast)
                    + (1 + traveling_cost_log_z|num_vehicle) 
                    + (1 + true_safety_prob_z|evac_notice_group)
                    
                    + true_safety_prob_z + electricity_lost_dur_num_z 
                    + flood_total_log_z
                    + traveling_cost_log_z + safe_place_cost_log_z)

m_to_build <- list(m_intercept,
                   m_demo,
                   m_demo_others,
                   m_model_mo,
                   m_model_full)

m_to_build_name <- c("m_intercept",
                     "m_demo",
                     "m_demo_others",
                     "m_model_mo",
                     "m_model_full")

## Run the model and save 
result_post_flo_exc <- gen_result_within(m_to_build, m_to_build_name, 
                                         df_f_exc, "post_flo_exc")
result_post_flo_exc <- clean_up_within_result(result_post_flo_exc)
View(result_post_flo_exc)


result_post_mic_exc <- gen_result_within(m_to_build,m_to_build_name, 
                                         df_m_exc, "post_mic_exc")
result_post_mic_exc <- clean_up_within_result(result_post_mic_exc)
View(result_post_mic_exc)


## Across

gen_result_across <- function(model_name, dat_name, predict_florence = TRUE, predict_michael = TRUE, predict_dorian = TRUE)
{
  results <- data.frame()
  for(m in 1:length(model_name))
  {
    file_name <- paste("model/",model_name[m],"_",dat_name,sep="")
    f_temp <- brm(file = file_name)
    result_list <- list()
    i <- 1
    if(predict_florence)
    {
      result_flo <- testing_pred(df_f_exc, f_temp, "evac_decision_num")
      colnames(result_flo) <- c("acc_f","precision_f","recall_f","F-score_f")
      result_list[[i]] <- result_flo 
      i <- i + 1
    }
    if(predict_michael)
    {
      result_mic <- testing_pred(df_m_exc, f_temp, "evac_decision_num")
      colnames(result_mic) <- c("acc_m","precision_m","recall_m","F-score_m")
      result_list[[i]] <- result_mic
      i <- i +1
    }
    if(i == 4) { result_temp <- data.frame(model = model_name[m], cbind(result_list[[1]],result_list[[2]],result_list[[3]])) }
    if(i == 3) { result_temp <- data.frame(model = model_name[m], cbind(result_list[[1]],result_list[[2]])) }
    if(i == 2) { result_temp <- data.frame(model = model_name[m], cbind(result_list[[1]])) }
    
    results <- rbind(results,result_temp)
  }
  return(results)
}


result_across_post_flo_exc <- gen_result_across(m_to_build_name,"post_flo_exc",predict_florence = FALSE)
View(result_across_post_flo_exc)

result_across_post_mic_exc <- gen_result_across(m_to_build_name,"post_mic_exc",predict_michael = FALSE)
View(result_across_post_mic_exc)
