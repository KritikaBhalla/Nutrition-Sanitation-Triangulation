library(tidyverse)
library(haven)
library(readxl)
library(vroom)

# setting the working directory for NFHS Variables
setwd("D:/R workshop/Nutrition Datasets/children's recode")

# bringing the dataset as an object
child = read_dta("IAKR7EFL.DTA")

colnames(child)  

# selecting important variables
child01 = child %>% select(v024, v025, sdist, v130, 957:986, sweight, v005)

# bringing the state code file and district code file as objects 
setwd("D:/R workshop/Nutrition Datasets")
state = read_xlsx("state codes.xlsx")
district = read_xlsx("district codes.xlsx")

# renaming some columns
child01 = child01 %>% rename(st_code = v024, sctr = v025, dist_code = sdist, mweight = v005)

# joining state names and district names 
child01 = child01 %>% left_join(state, by = "st_code") %>%
  left_join(district, by = "dist_code")

# Harmonizing NFHS and NSS datasets on district level by creating a  new variable - where first two digit represent state code and last one digit/two digit represnt the district code
child01 = child01 %>% mutate(state_dist = nss_state_code*100+nss_dist_code)

# assigning sector names 
child01 = child01 %>% mutate(sctr = case_when(sctr == 1 ~ "Urban",
                                              sctr == 2 ~ "Rural"))


##Stunting

summary(child01$hw70) 

# replacing missing values as NA and bringing the decimal
child01 = child01 %>% mutate(hw70 = ifelse(hw70 >= 601, NA,
                                           hw70),
                             hw70 = hw70/100)

# creating Categorical Variable for Stunting
child01 = child01 %>% mutate(stunted = case_when(hw70 <= -2 ~ 1,
                                                 is.na(hw70) ~ NA,
                                                 TRUE ~ 0))

##Wasting

summary(child01$hw72)  

# replacing missing values as NA and bringing the decimal
child01 = child01 %>% mutate(hw72 = ifelse(hw72 >= 601, NA,
                                           hw72),
                             hw72 = hw72/100)

# Creating categorical variable for wasting
child01 = child01 %>% mutate(wasted = case_when(hw72 <= -2 ~ 1,
                                                is.na(hw72) ~ NA,
                                                TRUE ~ 0))


# bringing the decimal point on child's sample weight variable
child01 = child01 %>% mutate(sweight = sweight/1000000)

library(srvyr)


# setting the weight to use survey design
svy.child = child01 %>% as_survey_design(weights = sweight)

## Prevalence of Stunting
svy.child %>% summarise(survey_mean(stunted, vartype = "ci", na.rm=T))
# overall approx 35% of children are stunted

# to get percentage of stunted children in both sectors
svy.child %>% group_by(sctr) %>%
  summarise(survey_mean(stunted, na.rm = T)) %>%
  ungroup()
# Rural   36.8 %
# Urban   29.6 % 

## NSS state-wise estimate of Stunting
s_state = svy.child %>% group_by(nss_state_code) %>%
  summarise(survey_mean(stunted, na.rm = T)) %>%
  ungroup()


## NSS district wise estimate of Stunting
s_district = svy.child %>% group_by(state_dist, nss_state_code, nss_dist_code) %>% 
  summarise(survey_mean(stunted, na.rm = T)) %>%
  ungroup()


## Prevalence of Wasting
svy.child %>% summarise(survey_mean(wasted, vartype = "ci", na.rm=T))
# approx 18.5% children are wasted in total

svy.child %>% group_by(sctr) %>%
  summarise(survey_mean(wasted, na.rm=T)) %>%
  ungroup()
#Rural   18.8 %
#Urban   17.7 %

## NSS state wise estimate of Wasting
w_state = svy.child %>% group_by(nss_state_code) %>%
  summarise(survey_mean(wasted, na.rm = T)) %>%
  ungroup()

## NSS district wise estimate of Wasting
w_district = svy.child %>% group_by(state_dist, nss_state_code,nss_dist_code) %>%
  summarise(survey_mean(wasted, na.rm = T)) %>%
  ungroup()



## setting working directory for NSS Variables

setwd("D:/R workshop/WASH Datasets/Data")

lvl01 = vroom_fwf(file = "R76120L01.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           sctr=c(15,15),
                           state=c(16,17),
                           dist=c(19,20),
                           nsc=c(127,129),
                           mlt=c(130,139)
                  ))


lvl02 = vroom_fwf(file = "R76120L02.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           psn=c(38,39),
                           gender=c(41,41),
                           edu=c(46,47),
                           usage=c(54,54),
                           type=c(55,56),
                           rsn=c(58,58)
                  ))

lvl03 = vroom_fwf(file = "R76120L03.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           hhsize=c(40,41),
                           rlgn=c(42,42),
                           grp=c(43,43),
                           mce=c(86,95)
                  ))

lvl05 = vroom_fwf(file = "R76120L05.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           source=c(40,41),
                           cover=c(72,72),
                           use=c(94,94),
                           typ=c(95,96),
                           child=c(110,110)
                  ))

lvl06 = vroom_fwf(file = "R76120L06.TXT",
                  fwf_cols(fsu=c(4,8),
                           sss=c(30,30),
                           hhno=c(31,32),
                           drain=c(58,58),
                           dispose=c(59,59),
                           place=c(60,60),
                           stag=c(65,65),
                           faeces=c(66,66),
                           flies=c(69,69)
                  ))

unique(lvl02$psn)

## getting the name of the states from the excel in data frame format

setwd("D:/R workshop/WASH Datasets")
st.code= read_xlsx("state_code 76round.xlsx")
dist.code = read_xlsx("dist codes.xlsx")


st.code= st.code %>% mutate(state = as.numeric(state))
#dist.code = dist.code %>% mutate(dist = as.numeric(dist))


## conversion of state to character
lvl01=lvl01 %>% mutate(state= as.numeric(state),
                       dist = as.numeric(dist))

# Harmonizing NFHS and NSS datasets on district level by generating code for state and dist - first two digit for state and last one/two digit for district
lvl01 = lvl01 %>% mutate(state_dist = state*100+dist)

#unique(lvl01$state_dist)

## setdiff(unique(lvl01$state_dist), unique(dist.code$state_dist)) 
## Result - 1922, 1923, 1920, 1921, 2428, 2430, 2431, 2433, 2427, 2429, 2432
## 11 extra districts are generated in joined dataset but are not presented in appendix & dist code excel file



lvl01 = lvl01 %>% 
  filter(state_dist %in% unique(dist.code$state_dist))
unique(lvl01$state_dist)


##joining st.code and dist.code with lvl01 
lvl01= lvl01 %>%  left_join(st.code, by = "state") %>%
  left_join(dist.code, by = c("state_dist", "state", "dist"))

unique(lvl01$state_dist)

joined = lvl01 %>% 
  #left_join(lvl02) %>% 
  left_join(lvl03) %>% 
  left_join(lvl05) %>% 
  left_join(lvl06)


hh.hd=lvl02 %>% filter(psn=="01") %>% 
  select(fsu, sss, hhno, psn, gender, edu)

joined = joined %>% left_join(hh.hd)

joined = joined %>% mutate(wt=mlt/100,
                           mce=as.numeric(mce))

joined = joined %>% mutate(sctr = case_when(sctr == 1 ~ "Rural",
                                            sctr == 2 ~ "Urban"))


#creating dummy variable for stagnant water
joined = joined %>% mutate(stag01=case_when(stag==1 ~ 1,
                                            stag==2 ~ 0,
                                            TRUE ~ NA_real_))

#creating dummy variable for human faeces
joined = joined %>% mutate(fecs01=case_when(faeces==1 ~ 1,
                                            faeces==2 ~ 0,
                                            TRUE ~ NA_real_))

#master variable
joined = joined %>% mutate(club=pmax(stag01, fecs01, na.rm = T))

unique(joined$club)
table(joined$club)


library(statar)

# creating indwt - indv level weight
joined = joined %>% mutate(indwt=wt*hhsize)


## data analysis using survey mode

library(srvyr)
svy.joined = joined %>% as_survey_design(weights = indwt)


## Overall Exposure to the problem stagnant water and faeces 
svy.joined %>% filter(!is.na(club)) %>% summarise(survey_mean(club, vartype = "ci"))
## 0.206 


## Exposure to the problem stagnant water and faeces by sector
tab1 = svy.joined %>% group_by(sctr) %>% 
  filter(!is.na(club)) %>%
  summarise(prop_ind = survey_mean(club)) %>%
  ungroup()

## Rural    0.224     0.00219
## Urban    0.166     0.00278




library(Hmisc)    # used for %nin% function

## Exposure to the problem stagnant water and faeces by state & sector 
tab2 = svy.joined %>% 
  group_by(stnm, sctr) %>% 
  filter(!is.na(club)) %>%
  summarise(prop_ind = survey_mean(club)) %>%
  #arrange(desc(prop_ind)) %>%
  ungroup()


## Exposure to the problem stagnant water and faeces by state
tab3 = svy.joined %>% 
  group_by(state, stnm) %>% 
  filter(!is.na(club)) %>% 
  summarise(prop_ind = survey_mean(club)) %>%
  #arrange(desc(prop_ind)) %>%
  ungroup()

## exposure to the problem of stagnant water and faeces by state and districts
tab4 = svy.joined %>% 
  group_by(state_dist, state, dist) %>%
  filter(!is.na(club)) %>% 
  summarise(survey_mean(club)) %>%
  ungroup()
tab4 = tab4 %>% rename(nss_state_code = state, nss_dist_code = dist)


## merging NFHS - WASH (Stunting)
stunt_wash = s_district %>% left_join(tab4, by = c("state_dist", "nss_state_code", "nss_dist_code"))

## merging NFHS - WASH (Wasting)
waste_wash = w_district %>% left_join(tab4, by = c("state_dist", "nss_state_code", "nss_dist_code"))



# Scatter plot for stunting vs WASH indicators
plot1 = stunt_wash %>%  
  filter(coef.y<=0.5) %>%
  ggplot(aes(x=coef.y, y=coef.x)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(color = "red") +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(title = "Figure 1: Stunting v/s WASH Indicator",
       x = "WASH Indicator",
       y = "Stunting") +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5)
  )
plot1

# Scatter plot for wasting vs WASH indicators
plot2 = waste_wash %>% 
  filter(coef.y<=0.5) %>%
  ggplot(aes(x=coef.y, y=coef.x)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(color = "red") +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  labs(title = "Figure 2: Wasting v/s WASH Indicator",
       x = "WASH Indicator",
       y = "Wasting") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )
plot2


tiff("Stunting.tif", width = 12, height = 5, units = "in", res = 600)
print(plot1)
dev.off()

tiff("Wasting.tif", width = 12, height = 5, units = "in", res = 600)
print(plot2)
dev.off()
