# install library
#install.packages("formattable")
library('plyr') #Splitting, Applying and Combining Data, files
library('tidyverse') # Data wrangle
library('knitr') # Markdown
library('formattable') # number format
library('VIM') # Missing value check
library("skimr") # Missing value check
library('gridExtra') 
# read data file names
getwd()
file_name <- list.files(path="./files/", pattern = NULL)
file_path <- paste(getwd(),"/files/", file_name, sep="")
#file_path <- file_path[19]

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
c1 <- ncol(data)

data <- 
  data %>% 
  filter(created_by != "midserver" ) %>%
  filter(created_by != "guest"  )
n2 <- nrow(data)

#00110
cat("Incident data is creatd by midserver or guest:", comma(n1-n2), 
    "out of", comma(n1,digits=0), ",", round((n1-n2)/n1*100), "%")
comma(n1-n2, digits = 0)
#00130 
#Check missing values in the data
NAcol <- which(colSums(is.na(data))>0)
NAcols <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)

aggr(data[,c("Y_locationname", "Y_location_id", "Y_country", "Y_cmgnumber", "Y_timezone", "Y_cmg_shortname", "Y_cmg_longname", "Y_cmg_unitname" ,"Z_cmguserid")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 


###################################################################################
#Check SIAL request

data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), is.na(X_bs_id)) %>% 
  filter(!str_detect(assignment_grp, "SIAL")) %>% filter(!str_detect(X_service_component, "SIAL"))

data %>% filter(str_detect(assignment_grp, "SIAL") | str_detect(X_service_component, "SIAL")) %>% filter(is.na(X_bs_id)) %>% 
  group_by(yyyymm) %>%  dplyr::summarise(n = n()) #write_csv("bsid_sial.csv")

data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved), request_div != "IT") %>%  
  dplyr::summarise(Miss_BSD = sum(is.na(X_bs_id)), Not_Miss_BSD = sum(!is.na(X_bs_id)), 
                   all_biz_resolved_incidents = Miss_BSD+Not_Miss_BSD, Miss_BSD/(Miss_BSD+Not_Miss_BSD)*100)

###################################################################################

# Fill the missing country with the most frequent value of locationname, after imputation it reduced from 30220 to 11576 
#data %>% filter(is.na(Y_country)) %>% distinct(Y_locationname) %>% write_csv("country_missing.csv")
#data %>% distinct(Y_country) %>% write_csv("country.csv")
country_location <- read_csv("Country_location_master.csv")

data <-
  data %>% left_join(country_location, by = c("Y_locationname" = "Location")) %>%
  mutate(Y_country = ifelse(is.na(Y_country),Country,Y_country))
data$Country <- NULL


# Fill the missing country with the most frequent value of business user, after imputation it reduced from 11576 to 1253 
country <- 
  data %>% group_by(Z_cmguserid, Y_country) %>% dplyr::summarise(n = n()) 

country <-
  country %>% group_by(Z_cmguserid) %>% filter(!is.na(Y_country)) %>% top_n(1, n) 

country <- country[!duplicated(country$Z_cmguserid), ]

data <-
  data %>% left_join(country, by = "Z_cmguserid") %>%
  mutate(Y_country.x = ifelse(is.na(Y_country.x), Y_country.y, Y_country.x))
data$Y_country.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_country = Y_country.x)

#Fill the missing region data with the most frequent value of country, after imputation it reduced from 160,523 to 1,867
region <-
  data %>% group_by(Y_country, Y_timezone) %>% dplyr::summarise(n = n()) 

region <-
  region %>% group_by(Y_country) %>% top_n(1, n)

region <-
  region %>% mutate(Y_timezone = ifelse(Y_country %in% c("United States of America", "Canada"),"NA",
                                        ifelse(Y_country %in% c("Kenya", "Nigeria", "Russian Federation"),"EMEA", 
                                               ifelse(Y_country %in% c("Pakistan", "Viet Nam"),"AP", 
                                                      ifelse(Y_country %in% c("Venezuela (Bolivarian Republic of)"),"LA", Y_timezone)))))

region[!duplicated(region$Y_country), ]

data <-
  data %>% left_join(region, by = "Y_country") %>% 
  mutate(Y_timezone.x = ifelse(is.na(Y_timezone.x), Y_timezone.y, Y_timezone.x))
data$Y_timezone.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_timezone = Y_timezone.x)

# Again, fill the missing region with the most frequent value of business user, after imputation it reduced from 1,867 to 1,345
region <- 
  data %>% group_by(Z_cmguserid, Y_timezone) %>% dplyr::summarise(n = n()) 

region <-
  region %>% group_by(Z_cmguserid) %>% filter(!is.na(Y_timezone)) %>% top_n(1, n)

region <- region[!duplicated(region$Z_cmguserid), ]

data <-
  data %>% left_join(region, by = "Z_cmguserid") %>%
  mutate(Y_timezone.x = ifelse(is.na(Y_timezone.x), Y_timezone.y, Y_timezone.x))
data$Y_timezone.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_timezone = Y_timezone.x)

data %>% distinct(Y_timezone, Y_country) %>% filter(is.na(Y_timezone))

#Fill the missing Y_cmg_short/long name data with the most frequent value of Y_cmg_unitname, after imputation it reduced from 115,228 to 19,155
cmg_name <- 
  data %>% group_by(Y_cmg_unitname, Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n()) 

cmg_name <-
  cmg_name %>% group_by(Y_cmg_unitname) %>% filter(!is.na(Y_cmg_shortname) & !is.na(Y_cmg_longname)) %>% top_n(1, n) 
#cmg_name %>% filter(Y_cmg_unitname == "T-IOW-S EUS Switzerland West")

data <-
  data %>% left_join(cmg_name, by = "Y_cmg_unitname") %>%
  mutate(Y_cmg_shortname.x = ifelse(is.na(Y_cmg_shortname.x), Y_cmg_shortname.y, Y_cmg_shortname.x),
         Y_cmg_longname.x = ifelse(is.na(Y_cmg_longname.x), Y_cmg_longname.y, Y_cmg_longname.x))
data$Y_cmg_shortname.y <- NULL
data$Y_cmg_longname.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_cmg_shortname = Y_cmg_shortname.x,
                                Y_cmg_longname = Y_cmg_longname.x)


# Again, fill the missing Y_cmg_short/long name with the most frequent value of Y_locationname, after imputation it reduced from 19,155 to 12
cmg_name <- 
  data %>% group_by(Y_locationname, Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n()) 

cmg_name <-
  cmg_name %>% group_by(Y_locationname) %>% filter(!is.na(Y_cmg_shortname) & !is.na(Y_cmg_longname)) %>% top_n(1, n) 
#cmg_name %>% filter(Y_cmg_unitname == "T-IOW-S EUS Switzerland West")

data <-
  data %>% left_join(cmg_name, by = "Y_locationname") %>%
  mutate(Y_cmg_shortname.x = ifelse(is.na(Y_cmg_shortname.x), Y_cmg_shortname.y, Y_cmg_shortname.x),
         Y_cmg_longname.x = ifelse(is.na(Y_cmg_longname.x), Y_cmg_longname.y, Y_cmg_longname.x))
data$Y_cmg_shortname.y <- NULL
data$Y_cmg_longname.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Y_cmg_shortname = Y_cmg_shortname.x,
                                Y_cmg_longname = Y_cmg_longname.x)

##################################################################################################

aggr(data[,c("Z_jobfamily", "Z_position", "Y_country", "Z_cmguserid")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#Fill the missing position with the most frequent value of userid, , after imputation it reduced from 114,759 to 114,753
position <- 
  data %>% group_by(Z_cmguserid, Z_position) %>% dplyr::summarise(n = n())

position <-
  position %>% group_by(Z_cmguserid) %>% filter(!is.na(Z_position)) %>% top_n(1, n) 

data <-
  data %>% left_join(position, by = "Z_cmguserid") %>%
  mutate(Z_position.x = ifelse(is.na(Z_position.x), Z_position.y, Z_position.x))
data$Z_position.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Z_position = Z_position.x)


#Fill the missing jobfamily with the most frequent value of userid, after imputation it reduced from 126,304 to 126,027
#Still many null data in jobfamily, position this is because incidents were requested by XUID externals who don't have jobfamily, position in HR suites
#data %>% filter(substr(Z_cmguserid,1,1) != "M" | is.na(resolved)) 
data %>% group_by(Z_jobfamily) %>% distinct(Z_jobfamily)
data %>% group_by(Z_position) %>% distinct(Z_position)

jobfamily <- 
  data %>% group_by(Z_cmguserid, Z_jobfamily) %>% dplyr::summarise(n = n()) 

jobfamily <-
  jobfamily %>% group_by(Z_cmguserid) %>% filter(!is.na(Z_jobfamily)) %>% top_n(1, n)

data <-
  data %>% left_join(jobfamily, by = "Z_cmguserid") %>%
  mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
data$Z_jobfamily.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)

NAcol <- which(colSums(is.na(data))>0)
NAcols2 <- sort(colSums(sapply(data[NAcol],is.na)), decreasing = T)

#Fill the missing jobfamily with the most frequent value of position
# position <- 
#   data %>% group_by(Z_position, Z_jobfamily) %>% dplyr::summarise(n = n())
# 
# position <-
#   position %>% group_by(Z_position) %>% filter(!is.na(Z_jobfamily)) %>% top_n(1, n) 
# 
# data <-
#   data %>% left_join(position, by = "Z_position") %>%
#   mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
# data$Z_jobfamily.y <- NULL
# data$n <- NULL
# data <- data %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)


##################################################################################################

aggr(data[,c("resolved_by_id", "resolved_by", "X_bs_id", "assignment_grp")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#Fill the missing resolved_by_id with the most frequent value of resolved_by, after imputation it reduced from 99,968 to 9,589
resolver <- 
  data %>% group_by(resolved_by, resolved_by_id) %>% dplyr::summarise(n = n())

resolver <-
  resolver %>% group_by(resolved_by) %>% filter(!is.na(resolved_by_id)) %>% top_n(1, n) 

data <-
  data %>% left_join(resolver, by = "resolved_by") %>%
  mutate(resolved_by_id.x = ifelse(is.na(resolved_by_id.x), resolved_by_id.y, resolved_by_id.x))
data$resolved_by_id.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( resolved_by_id = resolved_by_id.x)

temp <-
data %>% filter(is.na(resolved_by_id))

temp <-
data %>% filter(resolved_by == "Dinesh Kambam")

#Fill the missing BSID with the most frequent value of assignment_Group, after imputation it reduced from 82,261 to 68
data %>% group_by(X_bs_id) %>% distinct(X_bs_id)

BSID <- 
  data %>% group_by(assignment_grp, X_bs_id) %>% dplyr::summarise(n = n())

BSID <-
  BSID %>% group_by(assignment_grp) %>% filter(!is.na(X_bs_id)) %>% arrange(assignment_grp, desc(n)) %>% top_n(1, n)

BSID %>% filter(assignment_grp == "BT-LL_BREL LIMS")

data <-
  data %>% left_join(BSID, by = "assignment_grp") %>%
  mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
data$X_bs_id.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( X_bs_id = X_bs_id.x)

#Fill the missing BSID with the most frequent value of resolved_by_id, after imputation it reduced from 68 to 36
BSID <- 
  data %>% group_by(resolved_by_id, X_bs_id) %>% dplyr::summarise(n = n())

BSID <-
  BSID %>% group_by(resolved_by_id) %>% filter(!is.na(X_bs_id)) %>% arrange(resolved_by_id, desc(n)) %>% top_n(1, n) 

data <-
  data %>% left_join(BSID, by = "resolved_by_id") %>%
  mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
data$X_bs_id.y <- NULL
data$n <- NULL
data <- data %>% dplyr::rename( X_bs_id = X_bs_id.x)

##################################################################################################
#final result after null imputation
NAcol2 <- which(colSums(is.na(data))>0)
NAcols2 <- sort(colSums(sapply(data[NAcol2],is.na)), decreasing = T)

#only for incidents requested by merck imployee
temp <-
  data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved)) 

NAcol3 <- which(colSums(is.na(temp))>0)
NAcols3 <- sort(colSums(sapply(temp[NAcol3],is.na)), decreasing = T)

rm(country, country_location, jobfamily, position, region, resolver, BSID, BS_data, temp, cmg_name, skimmed, args1, Charcol, Numcol, file_name)

##################################################################################################
##################################################################################################
#Regarding resolution lead time calculation in Service now there is some miscalculation when business_duration is equal to calendar_duration
#SNOW calculates a day with 24 hours, not eight work hours, and also include the weekend.
#We can obviosly see that when calendar_duration has a high number, i.e.,>= 60 hours (216000)

#data check for the case of business_duration != calendar_duration
data %>% filter(business_duration != calendar_duration, business_duration >= 216000 , !is.na(resolved), substr(Z_cmguserid,1,1) == "M") %>% 
  select(number, created_on, resolved, business_duration, calendar_duration) %>% arrange(desc(created_on)) %>%
  write_csv("correct_duration.csv")

data %>% filter(business_duration == calendar_duration, calendar_duration >= 216000 , !is.na(resolved)) %>% 
  select(number, created_on, resolved, calendar_duration) %>% arrange(desc(created_on)) %>% write_csv("wrong_duration.csv")

#Every month it has more than 7% out of created incidents which has wrong calculation
data %>% mutate(NG = ifelse(business_duration == calendar_duration, 1,0), 
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

data$created_on2 <- NULL
data$resolved2 <- NULL
data$weekdays <- NULL

data <- data %>% 
  mutate(created_on2 = as.Date(substr(created_on, 1,10)), 
         resolved2   = as.Date(substr(resolved, 1,10)))

args1 <- list(data$created_on2, data$resolved2)
args1 <-
  args1 %>% pmap(Nweekdays)
args1 <- unlist(args1)
data$weekdays <- args1


#  mutate( LT_day = round( ifelse(business_duration == calendar_duration & calendar_duration >0, (business_duration / 3600 / 24) - weekdays, business_duration / 3600 / 8), digits = 2),
#          LT_hour = round( ifelse(business_duration == calendar_duration & calendar_duration >0, (business_duration / 3600 / 3) - weekdays * 8 , business_duration / 3600), digits = 2) ) %>%
#  mutate( LT_day = ifelse(business_duration == 0 & calendar_duration >0, round((calendar_duration / 3600 / 24) - weekdays, digits = 2), LT_day),
#          LT_hour = ifelse(business_duration == 0 & calendar_duration >0, round((calendar_duration / 3600 / 3) - weekdays* 8, digits = 2), LT_hour),
data <- data %>% 
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
data %>% filter(is.na(assigned_to)) %>% dplyr::count(yyyymm) 
filter(data, LT_day < 0)
data %>% filter(business_duration == calendar_duration & calendar_duration >0) %>% dplyr::count(yyyymm) 
data %>% filter(business_duration == 0 & calendar_duration >0) %>% dplyr::count(yyyymm) 
data %>% filter(abs(weekdays - LT_day)>=10) %>% dplyr::count(yyyymm) 
#user의 관점에서 dashboard 필요

#data <- data %>% mutate( LT_day = round( business_duration / 3600 / 8, digits = 2),
#        LT_hour = round( business_duration / 3600, digits = 2) )

#data check Y_location_id as it has some characters in the value
data <-
  data %>% mutate( Y_location_id = substr(Y_location_id,1,6))
dim(data)

data %>% write_csv("all_incidents.csv", na = "")

temp <-
data %>% filter(assignment_grp == "AP_SOUTH KOREA-SEOUL_FIELDSUPPORT" | 
                  assignment_grp == "AP_SOUTH KOREA-SEOUL_LOCAL ADMIN") %>%
  group_by(X_bs_id) %>% dplyr::summarise(n = n())

temp <-
data %>% filter(assignment_grp == "AP_SOUTH KOREA-SEOUL_FIELDSUPPORT" | 
                    assignment_grp == "AP_SOUTH KOREA-SEOUL_LOCAL ADMIN") %>% 
  group_by(X_bs_id) %>% dplyr::summarize(n=n())
####################################################################################

#power BI에서 특수문자의 경우 에러 발생, 확인 필요
#data$short_desc2 <- str_replace_all(data2$short_desc, "[?<>+]", "99")
#data %>% write_csv("inc_data2.csv", na = "", locale = locale(encoding = "UTF-8"))

# data check Y_cmg_shortname and pick up first 2 digits -> request_div
data %>% group_by(Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n())

#data check with state, resolved date
data %>% distinct(state2)
data %>% filter(state2 == "Closed", is.na(resolved)) 
data %>% filter(state2 == "Progressing", !is.na(resolved)) 
data %>% filter(state2 == "Backlog", !is.na(resolved)) 

#There needs to check the case when Reassignment >= 3, reopen_count >= 2, as it is related to not clear service matirix as well as service lead time  
temp <-
  data %>% filter(number == "INC1760427" | number == "INC1713730")

data %>% filter(!is.na(resolved), substr(Z_cmguserid,1,1) == "M", reassignment_cnt >= 3) %>% 
  group_by(yyyymm) %>% dplyr::summarise(reassign=n()) %>% arrange(desc(yyyymm))

data %>% filter(!is.na(resolved), substr(Z_cmguserid,1,1) == "M", reopen_count >= 2) %>% 
  group_by(yyyymm) %>% dplyr::summarise(reopen=n()) %>% arrange(desc(yyyymm))  








####################################################################################

#Finally let's see characteristics of all the data created by Merck employee including IT
skimmed <- skim_to_wide(filter(data, substr(Z_cmguserid,1,1) == "M", !is.na(resolved)))
skimmed[, c(1:5, 9:11, 13, 15:16)]
skimmed %>% filter(type == "character") %>% arrange(as.numeric(n_unique), desc(missing))
skimmed %>% filter(type == "numeric") %>% arrange(as.numeric(n_unique), desc(missing))
skimmed %>% filter(variable %in% c("LT_day","LT_hour")) %>% select(variable, mean,sd,starts_with("p"))

data %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved)) %>% 
  ggplot(aes(LT_day)) + geom_histogram(binwidth = 10) + xlim(0,50) 
  
#quantile(x, probs = c(0.05, 0.06, 0.07, 0.08, 0.09, 0.1),na.rm = FALSE)

####################################################################################

data$LT10_20     <- as.factor(data$LT10_20)
data$LT20_40     <- as.factor(data$LT20_40)
data$LT40_up     <- as.factor(data$LT40_up)
data$LT_LRT      <- as.factor(data$LT_LRT)
data$LT_in_time  <- as.factor(data$LT_in_time)
data$LT_ATOP     <- as.factor(data$LT_ATOP)
data$reopen      <- as.factor(data$reopen)
data$reassign    <- as.factor(data$reassign)
data$u_FTR2      <- as.factor(data$u_FTR2)
data$backlog     <- as.factor(data$backlog)
data$state2      <- as.factor(data$state2)
data$request_div <- as.factor(data$request_div)

str(data)

Charcol <- names(data[,sapply(data, is.character)])
Numcol <- names(data[,sapply(data, is.numeric)])
Fatcol <- names(data[,sapply(data, is.factor)])

sum(table(data$X_bs_id))

numericVars <- which(sapply(data, is.numeric))
numericVarNames <- names(numericVars)
cat("There are", length(numericVars), "numeric variables")

all_numVar <- data[, numericVars]
cor_numVar <- cor(all_numVar, use = "pairwise.complete.obs")

cor_sorted <- as.matrix(sort(cor_numVar[,2], decreasing = T)) #eng_business_duration3_sum
corHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)> 0.5)))
cor_numVar <- cor_numVar[corHigh,corHigh]
corrplot.mixed(cor_numVar, tl.col ="black",tl.pos="lt")




#Data distribution review
data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60))
summary(data$LT_day)
data %>% quantile(data$LT_day, probs = c(0.5, 0.75, 0.8, 0.9, 0.93, 0.95, 0.97, 0.98, 0.99 ,1),na.rm = FALSE)

quantile( filter(data, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day, probs = c(0.5, 0.75, 0.8, 0.9, 0.93, 0.95,0.96, 0.97, 0.98, 0.99 ,1))

qqnorm(filter(data, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)
qqline(filter(data, !is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)

qqnorm(filter(data, !is.na(resolved), LT_day >10 & LT_day <60,  !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)
qqline(filter(data, !is.na(resolved), LT_day >10 & LT_day <60,  !is.na(Y_cmg_shortname) | request_div != "IT")$LT_day)

#x 구간 나누어서 계산하기 break.......???
summary(cut(temp$LT_day, breaks = c(0, 0.75, 0.90, 0.95, 0.98, 0.99, 1), include.lowest = T))

#Every month it has more than 500 incidents taking more than 30 days
temp1 <-
temp %>% filter(LT_day >= 30) %>% group_by(yyyymm) %>% dplyr::summarize(n = n())

s1 <-
data %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) + 
  ggtitle("RLT distribution with features", subtitle = "Reassigned >=3")

s2 <-
data %>% filter(reopen == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) +
  ggtitle("",subtitle = "Reopened")
  
s3 <-
data %>% filter(u_FTR2 == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60)) + 
  ggtitle("",subtitle = "FTR supported")

grid.arrange(s1, s2, s3, widths=1)

data %>% nrow()
data %>% filter(abs(LT_day-weekdays) > 5) %>% nrow()

data %>% filter(abs(LT_day-weekdays) > 5) %>%
  ggplot() + geom_density(aes(x=LT_day-weekdays, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60, 300))


data %>% distinct(reassignment_cnt)
data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot() + geom_density(aes(x=reassignment_cnt)) + scale_x_log10(breaks = c(0,1, 2,3,4,5, 10, 20, 30, 40, 60))

data %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot(aes(x = reassignment_cnt, y =LT_day )) + geom_point(alpha = 1/100) + geom_smooth() + ylim(0, 100) + xlim(0, 20)

data %>% filter(reassign == 1) %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% 
  ggplot(aes(x = reassignment_cnt, y =LT_day )) + geom_hex() 

library(randomForest)
set.seed(2018)

temp <-
data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>%
  select(LT_day, LT10_20, LT20_40, LT40_up, LT_LRT, reopen, reassign,u_FTR2,reassignment_cnt,reopen_count,request_div, Z_jobfamily, Y_country)

temp %>% filter(is.na(X_bs_id))
dim(temp)
quick_RF <- randomForest(x=temp[1:10000,-1], y=temp$LT_day[1:10000], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

str(data)

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + coord_flip() + theme(legend.position="none")


data %>% filter(number == "INC1854118") %>% select(number, created_on2, resolved2, business_duration, weekdays)

data %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_histogram(binwidth = 1) + xlim(0, 40)
data %>% filter(!is.na(resolved), LT_hour >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(aes(color = BSID), binwidth = 1) + xlim(0, 40)
data %>% filter(!is.na(resolved), LT_hour >=10, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(binwidth = 5) + xlim(10, 80)
data %>% filter(!is.na(resolved), LT_hour >=0,  !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(ERP, LT_hour)) + geom_boxplot() + ylim(0, 240) #30 days

temp <-
data %>% filter(str_detect(assignment_grp, "BT-LL_B")) 

NAcols
####################################################################################
# Read BSID data downloaded on 2019-12-09, filtered by approved status only 

BS_data <- read_csv("u_cmdb_ci_service_2019v2.csv")

BS_data %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% dplyr::summarise(n = n()) %>%
  filter(n >= 2)

BS_data %>% filter(!is.na(u_bs_id), substr(u_approval_status, 1,8) == "approved") %>% 
  group_by(u_bs_id) %>% top_n(1, u_version) %>% write_csv("BSD_201912.csv", na = "")

BS_data %>% filter(u_bs_id == "BS_2257")

####################################################################################

temp1 <- temp %>% distinct(Z_employeetype)
rm(temp)
quantile(temp$LT_hour)

####################################################################################
### Data update
temp <- read_csv("C:/temp/Rtest/IT_request/update/incident/incident_update_201809_201910.csv", 
                 locale = locale(encoding = "UTF-8"),
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
                   location.u_cmg.u_cmgcmgnumber = col_character()))
colnames(temp)

temp <- temp %>% dplyr::rename(
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

temp <- as_tibble(temp)

temp <- 
  temp %>% 
  filter(created_by != "midserver" ) %>%
  filter(created_by != "guest"  )

temp1 <- temp %>% select(number)

NROW(data)
data <- data %>% anti_join(temp1) 
data <- rbind.fill(data, temp) #rbind를 수행할때 중복 데이터가 생기는 에러가 발생함
data <- data %>% distinct(number, .keep_all = TRUE)  #data 중복 삭제
rm(temp1, temp)

#data duplication check
data %>% group_by(number) %>% summarize(n = n()) %>% filter(n >= 2)

####################################################################################
### new Data insert