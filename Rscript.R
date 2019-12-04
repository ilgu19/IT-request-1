# install library
#install.packages("formattable")
library('tidyverse') # Data wrangle
library('plyr') #Splitting, Applying and Combining Data
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
  Y_timezone         	=	location.parent.u_region)

#Delete the data generated by system
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
#There is data having no creation date, 21872, it needs to remove the data 
data %>% mutate(NG = ifelse(business_duration == calendar_duration, 1,0), 
                yyyymm = as.Date(str_c(substr(created_on, 1,7), "-01"))) %>% 
  group_by(yyyymm) %>% dplyr::summarise(wrong_duration = sum(NG), total = n(), 
                                        percent = wrong_duration/total*100) %>% 
  arrange(desc(yyyymm))

