# install library
#install.packages("formattable")
library('plyr') #Splitting, Applying and Combining data2, files
library('tidyverse') # data2 wrangle
library('knitr') # Markdown
library('formattable') # number format
library('VIM') # Missing value check
library("skimr") # Missing value check
library('gridExtra') 


###################################################################################
#Check SIAL request

data2 %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), is.na(X_bs_id)) %>% 
  filter(!str_detect(assignment_grp, "SIAL")) %>% filter(!str_detect(X_service_component, "SIAL"))

data2 %>% filter(str_detect(assignment_grp, "SIAL") | str_detect(X_service_component, "SIAL")) %>% filter(is.na(X_bs_id)) %>% 
  group_by(yyyymm) %>%  dplyr::summarise(n = n()) #write_csv("bsid_sial.csv")

data2 %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), request_div != "IT") %>%  
  dplyr::summarise(Miss_BSD = sum(is.na(X_bs_id)), Not_Miss_BSD = sum(!is.na(X_bs_id)), 
                   all_biz_resolved_incidents = Miss_BSD+Not_Miss_BSD, Miss_BSD/(Miss_BSD+Not_Miss_BSD)*100)

##################################################################################################
#external employee 의 추의를 확인해 볼 필요가 있다... 중요한 idea!!!  지역 정보를 가지고 있고 대부분의 인사 정보를 가지고 있음
data %>% filter(Z_employeetype == "External")  #530,884 for Employee / 118,434 for External / total 650,610

##################################################################################################
#Regarding resolution lead time calculation in Service now there is some miscalculation when business_duration is equal to calendar_duration
#SNOW calculates a day with 24 hours, not eight work hours, and also include the weekend.
#We can obviosly see that when calendar_duration has a high number, i.e.,>= 60 hours (216000)

#data2 check for the case of business_duration != calendar_duration
data2 %>% filter(business_duration != calendar_duration, business_duration >= 216000 , !is.na(resolved), substr(Z_cmguserid,1,1) == "M") %>% 
  select(number, created_on, resolved, business_duration, calendar_duration) %>% arrange(desc(created_on)) %>%
  write_csv("correct_duration.csv")

data2 %>% filter(business_duration == calendar_duration, calendar_duration >= 216000 , !is.na(resolved)) %>% 
  select(number, created_on, resolved, calendar_duration) %>% arrange(desc(created_on)) %>% write_csv("wrong_duration.csv")

#Every month it has more than 7% out of created incidents which has wrong calculation
data2 %>% mutate(NG = ifelse(business_duration == calendar_duration, 1,0), 
                 yyyymm = as.Date(str_c(substr(created_on, 1,7), "-01"))) %>% 
  group_by(yyyymm) %>% dplyr::summarise(wrong_duration = sum(NG), total = n(), 
                                        percent = wrong_duration/total*100) %>% 
  arrange(desc(yyyymm))

####################################################################################
#function to get workdays only excluding weekends between dates
#options(error = recover)
#library(lubridate)
Nweekdays <- function(a,b)
{
  if(is.na(a) | is.na(b))
  { v_days <- 0  }
  else {
    v_start <- unclass(a)
    v_end <- unclass(b)
    v_dates <- seq(v_start, v_end, 1)
    v_days <- sum(v_dates%%7%in%c(2,3))
    v_check <- as.integer(b - a) - v_days
    v_days <- ifelse(v_check > 0, v_check, 0)
  }
  return(v_days)
}  

#Sample
Nweekdays(as.Date("2018-11-06"), as.Date("2018-11-07"))
####################################################################################
# Data3 Convert value calculation 
#
data2$created_on2 <- NULL
data2$resolved2 <- NULL
data2$weekdays <- NULL

data3 <- data2 %>% 
  mutate(created_on2 = as.Date(substr(created_on, 1,10)), 
         resolved2   = as.Date(substr(resolved, 1,10)))

args1 <- list(data3$created_on2, data3$resolved2)
args1 <-
  args1 %>% pmap(Nweekdays)
args1 <- unlist(args1)
data3$weekdays <- args1


#  mutate( LT_day = round( ifelse(business_duration == calendar_duration & calendar_duration >0, (business_duration / 3600 / 24) - weekdays, business_duration / 3600 / 8), digits = 2),
#          LT_hour = round( ifelse(business_duration == calendar_duration & calendar_duration >0, (business_duration / 3600 / 3) - weekdays * 8 , business_duration / 3600), digits = 2) ) %>%
#  mutate( LT_day = ifelse(business_duration == 0 & calendar_duration >0, round((calendar_duration / 3600 / 24) - weekdays, digits = 2), LT_day),
#          LT_hour = ifelse(business_duration == 0 & calendar_duration >0, round((calendar_duration / 3600 / 3) - weekdays* 8, digits = 2), LT_hour),
data3 <- data3 %>% 
  mutate( LT_day = round( business_duration / 3600 / 8, digits = 2),
          LT_hour = round( business_duration / 3600, digits = 2), 
          LT10_20 = ifelse(LT_day > 10 & LT_day <= 20 ,1,0),
          LT20_40 = ifelse(LT_day > 20 & LT_day <= 40 ,1,0),
          LT40_up  = ifelse(LT_day > 40,1,0),
          LT_LRT = ifelse(LT_day > 10, 1, 0),               # LRT > 10 days    
          LT_in_time = ifelse(LT_day < 5, 1, 0),   # resolved_in_time < 5 days
          LT_ATOP = ifelse(LT_day < 3, 1, 0),               # resolved_in_time < 3 days
          reopen = ifelse(reopen_count > 0, 1, ifelse(is.na(reopen_count),0,0)),
          reassign = ifelse(reassignment_cnt > 2, 1, ifelse(is.na(reassignment_cnt),0,0)),
          u_FTR2 = ifelse(is.na(u_FTR), 0, ifelse(u_FTR == "Yes", 1, 0)),
          backlog = ifelse(LT_day > 5 & state %in% c("Work In Progress", "Pending", "Assigned"), 1, 0), # >5 days & not resolved  
          state2 = ifelse(backlog == 1, "Backlog", ifelse(state %in% c("Open", "Work In Progress", "Pending", "Assigned"), "Progressing", 
                                                          ifelse(state %in% c("Resolved","Closed"), "Closed", state))),
          request_div = substr(Y_cmg_shortname,1,2), 
          yyyymm = as.Date(str_c(substr(created_on, 1,7), "-01")),
          yyyymm_resolved = as.Date(str_c(substr(resolved, 1,7), "-01")))

#assigned_to가 null 이면 calculation이 안됨 => 수정 조치 꼭 필요
data3 %>% filter(is.na(assigned_to)) %>% dplyr::count(yyyymm) 
filter(data3, LT_day < 0)
data3 %>% filter(business_duration == calendar_duration & calendar_duration >0) %>% dplyr::count(yyyymm) 
data3 %>% filter(business_duration == 0 & calendar_duration >0) %>% dplyr::count(yyyymm) 
data3 %>% filter(abs(weekdays - LT_day)>=10) %>% dplyr::count(yyyymm) 
#user의 관점에서 dashboard 필요

#data3 <- data3 %>% mutate( LT_day = round( business_duration / 3600 / 8, digits = 2),
#        LT_hour = round( business_duration / 3600, digits = 2) )

#data3 check Y_location_id as it has some characters in the value
data3 <-
  data3 %>% mutate( Y_location_id = substr(Y_location_id,1,6))
dim(data3)

data3 %>% write_csv("all_incidents.csv", na = "")

temp <-
  data3 %>% filter(assignment_grp == "AP_SOUTH KOREA-SEOUL_FIELDSUPPORT" | 
                     assignment_grp == "AP_SOUTH KOREA-SEOUL_LOCAL ADMIN") %>%
  group_by(X_bs_id) %>% dplyr::summarise(n = n())

temp <-
  data3 %>% filter(assignment_grp == "AP_SOUTH KOREA-SEOUL_FIELDSUPPORT" | 
                     assignment_grp == "AP_SOUTH KOREA-SEOUL_LOCAL ADMIN") %>% 
  group_by(X_bs_id) %>% dplyr::summarize(n=n())

####################################################################################
# Read BSID data3 downloaded on 2019-12-09, filtered by approved status only 

BS_Master <- read_csv("BSD_201912.csv")
BS_Master %>% distinct(u_bs_id)
temp <-
  data3 %>% left_join(BS_Master, by = c("X_bs_id" = "u_bs_id"))
rm(temp)

BS_data3 <- read_csv("u_cmdb_ci_service_2019v2.csv")

BS_data3 %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% dplyr::summarise(n = n()) %>%
  filter(n >= 2)

BS_data3 %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% top_n(1, u_version) %>% write_csv("BSD_201912.csv", na = "")

BS_data3 %>% filter(u_bs_id == "BS_2257")
####################################################################################

#power BI에서 특수문자의 경우 에러 발생, 확인 필요
#data3$short_desc2 <- str_replace_all(data32$short_desc, "[?<>+]", "99")
#data3 %>% write_csv("inc_data32.csv", na = "", locale = locale(encoding = "UTF-8"))

# data3 check Y_cmg_shortname and pick up first 2 digits -> request_div
data3 %>% group_by(Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n())

#data3 check with state, resolved date
data3 %>% distinct(state2)
data3 %>% filter(state2 == "Closed", is.na(resolved)) 
data3 %>% filter(state2 == "Progressing", !is.na(resolved)) 
data3 %>% filter(state2 == "Backlog", !is.na(resolved)) 

#There needs to check the case when Reassignment >= 3, reopen_count >= 2, as it is related to not clear service matirix as well as service lead time  
temp <-
  data3 %>% filter(number == "INC1760427" | number == "INC1713730")

data3 %>% filter(!is.na(resolved), substr(Z_cmguserid,1,1) == "M", reassignment_cnt >= 3) %>% 
  group_by(yyyymm) %>% dplyr::summarise(reassign=n()) %>% arrange(desc(yyyymm))

data3 %>% filter(!is.na(resolved), substr(Z_cmguserid,1,1) == "M", reopen_count >= 2) %>% 
  group_by(yyyymm) %>% dplyr::summarise(reopen=n()) %>% arrange(desc(yyyymm))  








####################################################################################

#Finally let's see characteristics of all the data3 created by Merck employee including IT
skimmed <- skim_to_wide(filter(data3, substr(Z_cmguserid,1,1) == "M", !is.na(resolved)))
skimmed[, c(1:5, 9:11, 13, 15:16)]
skimmed %>% filter(type == "character") %>% arrange(as.numeric(n_unique), desc(missing))
skimmed %>% filter(type == "numeric") %>% arrange(as.numeric(n_unique), desc(missing))
skimmed %>% filter(variable %in% c("LT_day","LT_hour")) %>% select(variable, mean,sd,starts_with("p"))

data3 %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved)) %>% 
  ggplot(aes(LT_day)) + geom_histogram(binwidth = 10) + xlim(0,50) 

#quantile(x, probs = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1),na.rm = FALSE)

####################################################################################

data3$LT10_20     <- as.factor(data3$LT10_20)
data3$LT20_40     <- as.factor(data3$LT20_40)
data3$LT40_up     <- as.factor(data3$LT40_up)
data3$LT_LRT      <- as.factor(data3$LT_LRT)
data3$LT_in_time  <- as.factor(data3$LT_in_time)
data3$LT_ATOP     <- as.factor(data3$LT_ATOP)
data3$reopen      <- as.factor(data3$reopen)
data3$reassign    <- as.factor(data3$reassign)
data3$u_FTR2      <- as.factor(data3$u_FTR2)
data3$backlog     <- as.factor(data3$backlog)
data3$state2      <- as.factor(data3$state2)
data3$request_div <- as.factor(data3$request_div)

str(data3)

Charcol <- names(data3[,sapply(data3, is.character)])
Numcol <- names(data3[,sapply(data3, is.numeric)])
Fatcol <- names(data3[,sapply(data3, is.factor)])

sum(table(data3$X_bs_id))

numericVars <- which(sapply(data3, is.numeric))
numericVarNames <- names(numericVars)
cat("There are", length(numericVars), "numeric variables")

all_numVar <- data3[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,2], decreasing = T)) #eng_business_duration3_sum
corHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)> 0.5)))
cor_numVar <- cor_numVar[corHigh,corHigh]
corrplot.mixed(cor_numVar, tl.col ="black",tl.pos="lt")




#data3 distribution review
data3 %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60))
summary(data3$LT_day)
data3 %>% quantile(data3$LT_day, probs = c(0.5, 0.75, 0.8, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99 ,1),na.rm = FALSE)

quantile( filter(data3, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day, probs = c(0.5, 0.75, 0.8, 0.9, 0.93, 0.95,0.96, 0.97, 0.98, 0.99 ,1))

qqnorm(filter(data3, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)
qqline(filter(data3, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)

qqnorm(filter(data3, !is.na(resolved), LT_day >10 & LT_day <60,  !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)
qqline(filter(data3, !is.na(resolved), LT_day >10 & LT_day <60,  !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)

#x 구간 나누어서 계산하기 break.......???
summary(cut(temp$LT_day, breaks = c(0, 0.75, 0.90, 0.95, 0.98, 0.99, 1), include.lowest = T))

#Every month it has more than 500 incidents taking more than 30 days
temp1 <-
  temp %>% filter(LT_day >= 30) %>% group_by(yyyymm) %>% dplyr::summarize(n = n())

s1 <-
  data3 %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) + 
  ggtitle("RLT distribution with features", subtitle = "Reassigned >=3")

s2 <-
  data3 %>% filter(reopen == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) +
  ggtitle("",subtitle = "Reopened")

s3 <-
  data3 %>% filter(u_FTR2 == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) + 
  ggtitle("",subtitle = "FTR supported")

grid.arrange(s1, s2, s3, widths=1)

data3 %>% nrow()
data3 %>% filter(abs(LT_day-weekdays) > 5) %>% nrow()

data3 %>% filter(abs(LT_day-weekdays) > 5) %>%
  ggplot() + geom_density(aes(x=LT_day-weekdays, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60, 300))


data3 %>% distinct(reassignment_cnt)
data3 %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot() + geom_density(aes(x=reassignment_cnt)) + scale_x_log10(breaks = c(0,1, 2,3,4,5, 10, 20, 30, 40, 60))

data3 %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot(aes(x = reassignment_cnt, y =LT_day )) + geom_point(alpha = 1/100) + geom_smooth() + ylim(0, 100) + xlim(0, 20)

data3 %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot(aes(x = reassignment_cnt, y =LT_day )) + geom_hex() 

library(randomForest)
set.seed(2018)

temp <-
  data3 %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>%
  select(LT_day, LT10_20, LT20_40, LT40_up, LT_LRT, reopen, reassign,u_FTR2,reassignment_cnt,reopen_count,request_div, Z_jobfamily, Y_country)

temp %>% filter(is.na(X_bs_id))
dim(temp)
quick_RF <- randomForest(x=temp[1:10000,-1], y=temp$LT_day[1:10000], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data3.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

str(data3)

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


data3 %>% filter(number == "INC1854118") %>% select(number, created_on2, resolved2, business_duration, weekdays)

data3 %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_histogram(binwidth = 1) + xlim(0, 40)
data3 %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(aes(color = BSID), binwidth = 1) + xlim(0, 40)
data3 %>% filter(!is.na(resolved), LT_hour >=10, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(binwidth = 5) + xlim(10, 80)
data3 %>% filter(!is.na(resolved), LT_hour >=0,  !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(ERP, LT_hour)) + geom_boxplot() + ylim(0, 240) #30 days

temp <-
  data3 %>% filter(str_detect(assignment_grp, "BT-LL_B")) 

NAcols

####################################################################################

temp1 <- temp %>% distinct(Z_employeetype)
rm(temp)
quantile(temp$LT_hour)

####################################################################################
