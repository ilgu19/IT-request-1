# install library
#install.packages("formattable")
library('tidyverse') # Data wrangle
library('plyr') #Splitting, Applying and Combining Data, files
library('knitr') # Markdown
library('formattable') # number format
library('VIM') # Missing value check

# read data file names
getwd()
file_name <- list.files(path="./files/", pattern = NULL)
file_path <- paste(getwd(),"/files/", file_name, sep="")

# read data from excel files in the folder
i<- 0
for (j in file_path)
{
  i <- i + 1
  assign(paste("file", i, sep = ""), read_csv(j, locale = locale(encoding = "UTF-8"),
                                              col_types = cols(
                                                .default = col_character(),
                                                sys_created_on = col_character(),
                                                resolved_by.user_name = col_character(),
                                                reassignment_count = col_number(),
                                                reopen_count = col_number(),
                                                business_duration = col_number(),
                                                calendar_duration = col_number(),
                                                due_date = col_character(),
                                                u_erp_related = col_character(),
                                                business_stc = col_number(),
                                                caller_id.u_cmgleavingdate = col_datetime(format = ""),
                                                caller_id.u_cmgjoiningdate = col_date(format = ""),
                                                location.u_location_id = col_character(),
                                                location.u_cmg.u_cmgcmgnumber = col_character()
                                              )))
}

n<-i
for (i in 2:n)
{
  if(i==2){
    data <- rbind.fill(file1, get(paste("file", i, sep = "")))
    rm(list = paste("file", 1, sep = ""))
  }
  else{
    data <- rbind.fill(data, get(paste("file", i, sep = "")))  
  } 
  rm(list = paste("file", i, sep = ""))
}

# Data review
data <- as_tibble(data)
knitr::kable(
  data[1:5,1:5], caption ="IT incident data"
)
summary(data)

# rename colum name
colnames(data)
data <- data %>% dplyr::rename(
  number             	=	number,                                                     
  created_by         	=	sys_created_by,                                             
  created_on         	=	sys_created_on,                                            
  reactivation_time  	=	u_reactivation_time,                                        
  closed_at          	=	closed_at,                                                  
  closed_by          	=	closed_by,                                                  
  state              	=	state,                                                      
  short_desc         	=	short_description,                                          
  assigned_to        	=	assigned_to,                                                
  assignment_grp     	=	assignment_group,                                           
  urgency            	=	urgency,                                                    
  opened_by          	=	opened_by,                                                  
  updated_on         	=	sys_updated_on,                                             
  updated_by         	=	sys_updated_by,                                            
  resolved           	=	resolved_at,                                                 
  resolved_by        	=	resolved_by,          
  u_FTR               = u_first_time_right_assignment_1,
  u_location          = u_ci_location,
  u_erp               = u_erp_related,
  resolved_by_id      = resolved_by.user_name,
  calendar_duration  	=	calendar_duration,                                          
  due_date           	=	due_date,                                                   
  business_duration  	=	business_duration,                                          
  reassignment_cnt   	=	reassignment_count,                                         
  X_service_component	=	u_service_component,                                        
  incident_type      	=	u_incident_type,                                            
  X_bs_id            	=	u_matrix.u_bs_id,                                           
  X_bs_name          	=	u_matrix.u_bs_name,                                         
  Y_country_code     	=	location.u_cmg.u_cmgcmgnumber,                                    
  Y_country          	=	location.u_cmg.u_cmgcountryname,                                           
  Y_location_id      	=	location.u_location_id,                                     
  Y_locationname    	=	location.name,                                              
  Y_cmg_shortname		=	caller_id.u_cmgglobalidentifiershortname,
  Y_cmg_longname 		=	caller_id.u_cmgglobalidentifierlongname, 
  Y_cmg_unitname   		=	caller_id.u_cmgorganizationalunitname,   
  Z_position         	=	caller_id.u_cmgposition,                 
  Z_jobfamily        	=	caller_id.u_cmgjobfamily,                
  Z_employeetype     	=	caller_id.u_employeetype,                
  Z_cmguserid        	=	caller_id.user_name,                   
  Z_user_name        	=	caller_id.name,                          
  Z_user_joindate    	=	caller_id.u_cmgjoiningdate,              
  Z_user_leavedate   	=	caller_id.u_cmgleavingdate,              
  Y_region           	=	location.parent.u_region,                                   
  Y_cmgnumber     		=	location.u_cmg.u_cmgcmgnumber,    
  Y_cmgofficialname		=	location.u_cmg.u_cmgcmgofficialname,
  Y_timezone         	=	location.parent.u_region)

#Delete the data auto-generated by the system
n1 <- nrow(data) #745,970 -> 587,766
data <- 
  data %>% 
  filter(created_by != "midserver" ) %>%
  filter(created_by != "guest"  )
n2 <- nrow(data)

#00110
cat("Incident data is creatd by midserver or guest:", comma(n1-n2,digits=0), 
    "out of", comma(n1,digits=0), ",", round((n1-n2)/n1*100), "%")

#00120
#If business_duration is equal to calendar_duration, SNOW calculates a day with 24 hours, not eight work hours, and also include the weekend.
#This is obvious when calendar_duration has a high number, i.e.,>= 60 hours (216000)
data %>% filter(business_duration == calendar_duration, calendar_duration >= 216000 , !is.na(resolved)) %>% 
  select(number, created_on, resolved, calendar_duration) %>% arrange(desc(created_on)) %>%
  write_csv("wrong_duration.csv")

#Every month it has more than 7% out of created incidents
data %>% mutate(NG = ifelse(business_duration == calendar_duration, 1,0), 
                yyyymm = as.Date(str_c(substr(created_on, 1,7), "-01"))) %>% 
  group_by(yyyymm) %>% dplyr::summarise(wrong_duration = sum(NG), total = n(), 
                                        percent = wrong_duration/total*100) %>% 
  arrange(desc(yyyymm))

#00130 
#Check missing values in the data
NAcol <- which(colSums(is.na(data))>0)
NAcols <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)

#locationname can provide missed country info(28,707), also same with the region(timezone, 151,592) info by country data
data %>% filter(is.na(Y_country)) %>% distinct(Y_locationname) %>% write_csv("country_missing.csv")
data %>% distinct(Y_country) %>% write_csv("country.csv")
country_location <- read_csv("Country_location_master.csv")

# country missing value is reduced to 10,869, this is due to no location info 
data <-
data %>% left_join(country_location, by = c("Y_locationname" = "Location")) %>%
  mutate(Y_country = ifelse(is.na(Country),Y_country,Country))
data$Country <- NULL

#Fill the missing region data with the most frequent value 
region <-
  data %>% group_by(Y_country, Y_timezone) %>% dplyr::summarise(n = n()) 

region <-
region %>% group_by(Y_country) %>% top_n(1, n)

region <-
region %>% mutate(Y_timezone = ifelse(Y_country %in% c("United States of America", "Canada"),"NAm",
                                      ifelse(Y_country %in% c("Kenya", "Nigeria", "Russian Federation"),"EMEA", 
                                             ifelse(Y_country %in% c("Pakistan", "Viet Nam"),"AP", 
                                                    ifelse(Y_country %in% c("Venezuela (Bolivarian Republic of)"),"LA", Y_timezone))))) %>%
  filter(!is.na(Y_timezone))

data <-
  data %>% left_join(region, by = "Y_country") 
data$Y_timezone.x <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_timezone = Y_timezone.y)
  
#Fill the missing jobfamily with the most frequent value of userid
data %>% group_by(Z_jobfamily) %>% distinct(Z_jobfamily)
data %>% group_by(Z_position) %>% distinct(Z_position)

jobfamily <- 
  data %>% group_by(Z_cmguserid, Z_jobfamily) %>% dplyr::summarise(n = n()) %>% write_csv("job.csv")

jobfamily <-
  jobfamily %>% group_by(Z_cmguserid) %>% top_n(1, n) %>% filter(!is.na(Z_jobfamily))

data <-
  data %>% left_join(jobfamily, by = "Z_cmguserid") %>%
  mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
data$Z_jobfamily.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)

NAcol <- which(colSums(is.na(data))>0)
NAcols2 <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)

#Fill the missing jobfamily with the most frequent value of position
position <- 
  data %>% group_by(Z_position, Z_jobfamily) %>% dplyr::summarise(n = n())

position <-
  position %>% group_by(Z_position) %>% top_n(1, n) %>% filter(!is.na(Z_jobfamily))

data <-
  data %>% left_join(position, by = "Z_position") %>%
  mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
data$Z_jobfamily.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)

##Missing value only for internal emplyee 
#Y_country 28707 -> 10869 -> 9559
#Y_timezone 151592 -> 10869 -> 9559
#Z_jobfamily 119922 -> 119865  -> 9021

temp <-
  data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved)) 

NAcol <- which(colSums(is.na(temp))>0)
NAcols3 <- sort(colSums(sapply(temp[NAcol],is.na)), decreasing = T)
n3 <- nrow(temp)
NAcols3
#temp %>% distinct(Y_cmg_shortname) #Y_cmg_shortname 활용 가능?

aggr(temp[,c("Y_locationname", "Y_location_id", "Y_country", "Y_cmgnumber", "Y_timezone", "location.u_cmg.u_cmgcmgofficialname")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#Fill the missing BSID with the most frequent value of assignment_Group
data %>% group_by(X_bs_id) %>% distinct(X_bs_id)
BSID <- 
  data %>% group_by(assignment_grp, X_bs_id) %>% dplyr::summarise(n = n())

BSID <-
  BSID %>% group_by(assignment_grp) %>% top_n(1, n) %>% filter(!is.na(X_bs_id))

data <-
  data %>% left_join(BSID, by = "assignment_grp") %>%
  mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
data$X_bs_id.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( X_bs_id = X_bs_id.x)

NAcol <- which(colSums(is.na(data))>0)
NAcols4 <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)
NAcols2 #83555 -> 42477 -> 38673 (for internal)

# 37580 -> 6782
data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), is.na(X_bs_id)) %>%  dplyr::summarise(n = n()) #write_csv("bsid_null.csv")

data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), is.na(X_bs_id)) %>% 
  filter(!str_detect(assignment_grp, "SIAL")) %>% filter(!str_detect(X_service_component, "SIAL"))
#34705
data %>% filter(str_detect(assignment_grp, "SIAL") | str_detect(X_service_component, "SIAL")) %>% filter(is.na(X_bs_id)) %>% 
  group_by(yyyymm) %>%  dplyr::summarise(n = n()) #write_csv("bsid_sial.csv")


#Fill the missing BSID with the most frequent value of IT engineer
BSID <- 
  data %>% group_by(resolved_by_id, X_bs_id) %>% dplyr::summarise(n = n())

BSID <-
  BSID %>% group_by(resolved_by_id) %>% top_n(1, n) %>% filter(!is.na(resolved_by_id)) %>% filter(!is.na(X_bs_id)) %>% filter(n > 1)

data <-
  data %>% left_join(BSID, by = "resolved_by_id") %>%
  mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
data$X_bs_id.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( X_bs_id = X_bs_id.x)

NAcol <- which(colSums(is.na(data))>0)
NAcols5 <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)
NAcols2 #83555 -> 42477 -> 38673 (for internal)

data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), request_div != "IT") %>%  
  dplyr::summarise(Miss_BSD = sum(is.na(X_bs_id)), Not_Miss_BSD = sum(!is.na(X_bs_id)), 
                   all_biz_resolved_incidents = Miss_BSD+Not_Miss_BSD, Miss_BSD/(Miss_BSD+Not_Miss_BSD)*100)

####################################################################################

Charcol <- names(data[,sapply(data, is.character)])
Numcol <- names(data[,sapply(data, is.numeric)])

sum(table(data$X_bs_id))

numericVars <- which(sapply(data3A, is.numeric))
numericVarNames <- names(numericVars)
cat("There are", length(numericVars), "numeric variables")

all_numVar <- data3A[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,2], decreasing = T)) #eng_business_duration3_sum
corHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)> 0.5)))
cor_numVar <- cor_numVar[corHigh,corHigh]
corrplot.mixed(cor_numVar, tl.col ="black",tl.pos="lt")

library("skimr")
skimmed <- skim_to_wide(temp)
skimmed[, c(1:5, 9:11, 13, 15:16)]
skimmed %>% arrange(type, desc(missing))
write_csv(skimmed, "data_skimmed.csv", na = "")

temp %>% distinct(incident_type)

table(temp$Y_cmg_shortname)

####################################################################################
#convert seconds to to hours, days
library(lubridate)

data <- data %>% 
  mutate( LT_day = round( ifelse(business_duration == calendar_duration & calendar_duration >0, business_duration / 3600 / 24, business_duration / 3600 / 8), digits = 2),
          LT_hour = round( ifelse(business_duration == calendar_duration & calendar_duration >0, business_duration / 3600 / 3, business_duration / 3600), digits = 2) ) %>%
  mutate( LT_day = ifelse(business_duration == 0 & calendar_duration >0, round(calendar_duration / 3600 / 24, digits = 2), LT_day),
          LT_hour = ifelse(business_duration == 0 & calendar_duration >0, round(calendar_duration / 3600 / 3, digits = 2), LT_hour),
          LT10_20 = ifelse(LT_day > 10 & LT_day <= 20 ,1,0),
          LT20_40 = ifelse(LT_day > 20 & LT_day <= 40 ,1,0),
          LT40_up  = ifelse(LT_day > 40,1,0),
          LT_LRT = ifelse(LT_day > 10, 1, 0),               # LRT > 10 days    
          LT_in_time = ifelse(LT_day < 5, 1, 0),   # resolved_in_time < 5 days
          LT_ATOP = ifelse(LT_day < 3, 1, 0),               # resolved_in_time < 3 days
          reopen = ifelse(reopen_count > 0, 1, ifelse(is.na(reopen_count),0,0)),
          u_FTR2 = ifelse(is.na(u_FTR), 0, ifelse(u_FTR == "Yes", 1, 0)),
          backlog = ifelse(LT_day > 5 & state %in% c("Work In Progress", "Pending", "Assigned"), 1, 0), # >5 days & not resolved  
          state2 = ifelse(backlog == 1, "Backlog", ifelse(state %in% c("Open", "Work In Progress", "Pending", "Assigned"), "Progressing", 
                                                          ifelse(state %in% c("Resolved","Closed"), "Closed", state))),
          request_div = substr(Y_cmg_shortname,1,2), 
          yyyymm = as.Date(str_c(substr(created_on, 1,7), "-01")),
          yyyymm_resolved = as.Date(str_c(substr(resolved, 1,7), "-01")))
          
# data check Y_cmg_shortname and pick up first 2 digits -> request_div
data %>% group_by(Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n())

#data check with state, resolved date
data %>% distinct(state2)
data %>% filter(state2 == "Closed", is.na(resolved)) 
data %>% filter(state2 == "Progressing", !is.na(resolved)) 
data %>% filter(state2 == "Backlog", !is.na(resolved)) 

colnames(data)

data %>% select(-reactivation_time, -Y_location_id) %>% write_csv("all_incidents.csv", na = "")
#power BI에서 특수문자의 경우 에러 발생, 확인 필요
#data$short_desc2 <- str_replace_all(data2$short_desc, "[?<>+]", "99")
#data %>% write_csv("inc_data2.csv", na = "", locale = locale(encoding = "UTF-8"))

data %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_histogram(binwidth = 1) + xlim(0, 40)
data %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(aes(color = BSID), binwidth = 1) + xlim(0, 40)
data %>% filter(!is.na(resolved), LT_hour >=10, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(binwidth = 5) + xlim(10, 80)
data %>% filter(!is.na(resolved), LT_hour >=0,  !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(ERP, LT_hour)) + geom_boxplot() + ylim(0, 240) #30 days

temp <-
data %>% filter(number == "INC2080286")

####################################################################################
# BSID data

BS_data <- read_csv("u_cmdb_ci_service_2019.csv")

BS_data %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% dplyr::summarise(n = n()) %>%
  filter(n >= 2)

BS_data %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% top_n(1, u_version) %>% write_csv("BSD_201912.csv")

####################################################################################

temp1 <- temp %>% distinct(Z_employeetype)
rm(temp)
quantile(temp$LT_hour)
