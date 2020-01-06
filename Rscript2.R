# read data file names
getwd()
file_name <- list.files(path="./files/", pattern = NULL)
file_path <- paste(getwd(),"/files/", file_name, sep="")
#file_path <- file_path[19]

# read data2 from excel files in the folder
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

rm(file_name, file_path, i,j, n)

# data2 review
data <- as_tibble(data)

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

#Delete the data2 auto-generated by the system
n1 <- nrow(data) #745,970 -> 587,766
c1 <- ncol(data)

#Store the original data
data2 <- 
  data %>% 
  filter(created_by != "midserver" ) %>%
  filter(created_by != "guest"  )
n2 <- nrow(data2)

#00110
cat("Incident data2 is creatd by midserver or guest:", comma(n1-n2), 
    "out of", comma(n1,digits=0), ",", round((n1-n2)/n1*100), "%")
comma(n1-n2, digits = 0)
#00130 
################################################################################################################################################ 

### data update for previous data
temp <- read_csv("C:/temp/Rtest/IT_request/update/incident/incident_update_201809_201911.csv", 
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

NROW(data2)
NROW(temp1)
data2 <- data2 %>% anti_join(temp1) 
data2 <- rbind.fill(data2, temp) #rbind를 수행할때 중복 데이터가 생기는 에러가 발생한 적이 있어서 check

temp1 <- data2 %>% select(number)
temp1 %>% count(number) %>% filter(n >= 2)
#data2 <- data2 %>% distinct(number, .keep_all = TRUE)  #data2 중복 삭제
rm(temp1, temp)

#data2 duplication check
#data2 %>% group_by(number) %>% summarize(n = n()) %>% filter(n >= 2)