# install library
#install.packages("formattable")
library('tidyverse') # Data wrangle
library('plyr') #Splitting, Applying and Combining Data, files
library('knitr') # Markdown
library('formattable') # number format

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
  
########
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

########
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


temp <-
data %>% filter(substr(Z_cmguserid,1,1) == "M")

NAcol <- which(colSums(is.na(temp))>0)
NAcols2 <- sort(colSums(sapply(temp[NAcol],is.na)), decreasing = T)

temp %>% distinct(Y_cmg_shortname) #Y_cmg_shortname 활용 가능?

#Y_country 28707 -> 10869 -> 9559
#Y_timezone 151592 -> 10869 -> 9559
#Z_jobfamily 119922 -> 119865  -> 10653 , 9021