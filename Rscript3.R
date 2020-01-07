
################################################################################################################################################ 
## Missing value handling ##

#Check missing values in the data
NAcol <- which(colSums(is.na(data2))>0)
NAcols <- sort(colSums(sapply(data2[NAcol],is.na)), decreasing = T)

aggr(data2[data2$Z_employeetype == "Employee", c("Y_locationname", "Y_country", "Y_timezone", "Z_cmguserid")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

##country
# Fill the missing country with the most frequent value of locationname, after imputation it reduced from 30220 to 11576 
country_location <- read_csv("Country_location_master.csv")

data2 <-
  data2 %>% left_join(country_location, by = c("Y_locationname" = "Location")) %>%
  mutate(Y_country = ifelse(is.na(Y_country),Country,Y_country))
data2$Country <- NULL

# Fill the missing country with the most frequent values of business user, after imputation it reduced from 11576 to 1253 

# country <- 
#   data2 %>% group_by(Z_cmguserid, Y_country) %>% summarize(n = n()) %>% arrange(Z_cmguserid, desc(n))
# country[order(country$Z_cmguserid, desc(country$n)),]
# data2 %>% filter(!is.na(Y_country)) %>% arrange(Z_cmguserid, desc(number)) %>% !duplicated(data2$Z_cmguserid)

temp <-
  data2 %>% filter(!is.na(Y_country)) %>% group_by(Z_cmguserid, Y_country) %>% summarize(n = n(), number = max(number)) %>% 
  arrange(Z_cmguserid, desc(n), desc(number)) 

country <- temp[!duplicated(temp$Z_cmguserid), ]
country$n <- NULL  
country$number <- NULL
# temp1 %>% filter(Z_cmguserid %in% c("M133964","M156839", "M140815", "M172011"))
# temp2 %>% filter(Z_cmguserid %in% c("M133964","M156839", "M140815", "M172011"))
# 
# country <-
#   country %>% filter(!is.na(Y_country)) %>% group_by(Z_cmguserid) %>% top_n(1, n) 
# 
# temp <- country[duplicated(country$Z_cmguserid), ]
# temp$Y_country <- NULL
# temp$n <- NULL
# country <- country %>% anti_join(temp, by = "Z_cmguserid" ) 
# 
# temp1 <-
#   temp %>% inner_join(data2, by = "Z_cmguserid" ) %>% arrange(desc(created_on)) %>% group_by(Z_cmguserid, Y_country) %>% top_n(1) 
# 
# country %>% filter(Z_cmguserid %in% c("M133964","M156839")) 
# 
# select(Z_cmguserid, Y_country)
# 
# country %>% inner_join(temp, by = "Z_cmguserid" )
# country <- country[!duplicated(country$Z_cmguserid), ]
# 
# country <- country %>% anti_join(temp, by = "Z_cmguserid" ) 
# country %>% filter(Z_cmguserid %in% c("M133964","M156839"))
# temp %>% filter(Z_cmguserid %in% c("M133964","M156839"))

# data2 <-
#   data2 %>% left_join(country, by = "Z_cmguserid") %>%
#   mutate(Y_country.x = ifelse(is.na(Y_country.x), Y_country.y, Y_country.x))
# data2$Y_country.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Y_country = Y_country.x)

idx <- which(is.na(data2$Y_country))
data2[idx,] <-
  data2[idx,] %>% left_join(country, by = "Z_cmguserid") %>%
  mutate(Y_country.x = Y_country.y)

#data2$Y_country.y <- NULL
#data2 <- data2 %>% dplyr::rename( Y_country = Y_country.x)

rm(idx, temp)

##Region  
#Fill the missing region data2 with the most frequent value of country, after imputation it reduced from 160,523 to 1,867

# region <-
#   data2 %>% group_by(Y_country, Y_timezone) %>% dplyr::summarise(n = n()) 
# 
# region <-
#   region %>% group_by(Y_country) %>% top_n(1, n)

temp <-
  data2 %>% group_by(Y_country, Y_timezone) %>% summarize(n = n(), number = max(number)) %>% 
  arrange(Y_country, desc(n), desc(number)) 

region <- temp[!duplicated(temp$Y_country), ]
region$n <- NULL  
region$number <- NULL

region <-
  region %>% mutate(Y_timezone = ifelse(Y_country %in% c("United States of America", "Canada"),"NA",
                                        ifelse(Y_country %in% c("Kenya", "Nigeria", "Russian Federation"),"EMEA", 
                                               ifelse(Y_country %in% c("Pakistan", "Viet Nam"),"AP", 
                                                      ifelse(Y_country %in% c("Venezuela (Bolivarian Republic of)", "Panama"),"LA", Y_timezone))))) %>%
  filter(!is.na(Y_timezone))

idx <- which(is.na(data2$Y_timezone))
data2[idx,] <-
  data2[idx,] %>% left_join(region, by = "Y_country") %>%
  mutate(Y_timezone.x = Y_timezone.y)

rm(idx, temp)

# # Again, fill the missing region with the most frequent value of business user, after imputation it reduced from 1,867 to 1,345
# region <- 
#   data2 %>% group_by(Z_cmguserid, Y_timezone) %>% dplyr::summarise(n = n()) 
# 
# region <-
#   region %>% group_by(Z_cmguserid) %>% filter(!is.na(Y_timezone)) %>% top_n(1, n)
# 
# region <- region[!duplicated(region$Z_cmguserid), ]
# 
# data2 <-
#   data2 %>% left_join(region, by = "Z_cmguserid") %>%
#   mutate(Y_timezone.x = ifelse(is.na(Y_timezone.x), Y_timezone.y, Y_timezone.x))
# data2$Y_timezone.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Y_timezone = Y_timezone.x)
# 
# data2 %>% distinct(Y_timezone, Y_country) %>% filter(is.na(Y_timezone))

## Store Master data as excel file
country %>% write_csv("country2019.csv", na = "")
region %>% write_csv("region2019.csv", na = "")
rm(country, region, country_location)

##Y_cmg_unitname, Y_cmg_shortname, Y_cmg_longname  
#Fill the missing cmg_shortname, cmg_longname with the most frequent value of locationname, after imputation it reduced from 120313 to 1651

aggr(data2[c("Y_cmg_unitname", "Y_cmg_shortname", "Y_cmg_longname", "Y_locationname")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

data2 %>% filter(!is.na(Y_locationname) & is.na(Y_cmg_shortname)) %>% nrow()

temp <-
  data2 %>% filter(!is.na(Y_locationname)) %>% group_by(Y_locationname, Y_cmg_shortname, Y_cmg_longname) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(Y_cmg_shortname)) %>% arrange(Y_locationname, desc(n), desc(number)) 

cmg <- temp[!duplicated(temp$Y_locationname), ]
cmg$n <- NULL  
cmg$number <- NULL

idx <- which(!is.na(data2$Y_locationname) & is.na(data2$Y_cmg_shortname))
data2[idx,] <-
  data2[idx,] %>% left_join(cmg, by = "Y_locationname") %>%
  mutate(Y_cmg_shortname.x = Y_cmg_shortname.y,
         Y_cmg_longname.x = Y_cmg_longname.y)


#Fill the missing cmg_unitname with the most frequent value of locationname, after imputation it reduced from 13400 to 1210
aggr(data2[c("Y_cmg_unitname", "Y_cmg_shortname", "Y_cmg_longname", "Y_locationname")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

temp <-
  data2 %>% filter(!is.na(Y_locationname)) %>% group_by(Y_locationname, Y_cmg_unitname) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(Y_cmg_unitname)) %>% arrange(Y_locationname, desc(n), desc(number)) 

cmgunit <- temp[!duplicated(temp$Y_locationname), ]
cmgunit$n <- NULL  
cmgunit$number <- NULL


idx <- which(!is.na(data2$Y_locationname) & is.na(data2$Y_cmg_unitname))
data2[idx,] <-
  data2[idx,] %>% left_join(cmgunit, by = "Y_locationname") %>%
  mutate(Y_cmg_unitname.x = Y_cmg_unitname.y)

rm(cmg, cmgunit, temp)
# #Fill the missing Y_cmg_short/long name data2 with the most frequent value of Y_cmg_unitname, after imputation it reduced from 115,228 to 19,155
# cmg_name <- 
#   data2 %>% group_by(Y_cmg_unitname, Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n()) 
# 
# cmg_name <-
#   cmg_name %>% group_by(Y_cmg_unitname) %>% filter(!is.na(Y_cmg_shortname) & !is.na(Y_cmg_longname)) %>% top_n(1, n) 
# #cmg_name %>% filter(Y_cmg_unitname == "T-IOW-S EUS Switzerland West")
# 
# data2 <-
#   data2 %>% left_join(cmg_name, by = "Y_cmg_unitname") %>%
#   mutate(Y_cmg_shortname.x = ifelse(is.na(Y_cmg_shortname.x), Y_cmg_shortname.y, Y_cmg_shortname.x),
#          Y_cmg_longname.x = ifelse(is.na(Y_cmg_longname.x), Y_cmg_longname.y, Y_cmg_longname.x))
# data2$Y_cmg_shortname.y <- NULL
# data2$Y_cmg_longname.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Y_cmg_shortname = Y_cmg_shortname.x,
#                                   Y_cmg_longname = Y_cmg_longname.x)
# 
# 
# # Again, fill the missing Y_cmg_short/long name with the most frequent value of Y_locationname, after imputation it reduced from 19,155 to 12
# cmg_name <- 
#   data2 %>% group_by(Y_locationname, Y_cmg_shortname, Y_cmg_longname) %>% dplyr::summarise(n = n()) 
# 
# cmg_name <-
#   cmg_name %>% group_by(Y_locationname) %>% filter(!is.na(Y_cmg_shortname) & !is.na(Y_cmg_longname)) %>% top_n(1, n) 
# #cmg_name %>% filter(Y_cmg_unitname == "T-IOW-S EUS Switzerland West")
# 
# data2 <-
#   data2 %>% left_join(cmg_name, by = "Y_locationname") %>%
#   mutate(Y_cmg_shortname.x = ifelse(is.na(Y_cmg_shortname.x), Y_cmg_shortname.y, Y_cmg_shortname.x),
#          Y_cmg_longname.x = ifelse(is.na(Y_cmg_longname.x), Y_cmg_longname.y, Y_cmg_longname.x))
# data2$Y_cmg_shortname.y <- NULL
# data2$Y_cmg_longname.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Y_cmg_shortname = Y_cmg_shortname.x,
#                                   Y_cmg_longname = Y_cmg_longname.x)


##Z_jobfamily, Z_position  
#Fill the missing Z_jobfamily, Z_position with the most frequent value of Z_cmguserid, after imputation it reduced from 131637 to 131147
aggr(data2[,c("Z_jobfamily", "Z_position", "Z_cmguserid")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#Z_jobfamily
temp <-
  data2 %>% filter(!is.na(Z_cmguserid)) %>% group_by(Z_cmguserid, Z_jobfamily) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(Z_jobfamily)) %>% arrange(Z_cmguserid, desc(n), desc(number)) 

jobfamily <- temp[!duplicated(temp$Z_cmguserid), ]
jobfamily$n <- NULL  
jobfamily$number <- NULL


idx <- which(!is.na(data2$Z_cmguserid) & is.na(data2$Z_jobfamily))
data2[idx,] <-
  data2[idx,] %>% left_join(jobfamily, by = "Z_cmguserid") %>%
  mutate(Z_jobfamily.x = Z_jobfamily.y)

#Z_position
temp <-
  data2 %>% filter(!is.na(Z_cmguserid)) %>% group_by(Z_cmguserid, Z_position) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(Z_position)) %>% arrange(Z_cmguserid, desc(n), desc(number)) 

position <- temp[!duplicated(temp$Z_cmguserid), ]
position$n <- NULL  
position$number <- NULL


idx <- which(!is.na(data2$Z_cmguserid) & is.na(data2$Z_position))
data2[idx,] <-
  data2[idx,] %>% left_join(position, by = "Z_cmguserid") %>%
  mutate(Z_position.x = Z_position.y)

#Still many null data2 in jobfamily, position this is because incidents were requested by XUID externals who don't have jobfamily, position in HR suites
#excluding external ID it has only 40 missing figure in position but still many for jobfamily
data2 %>% filter(substr(Z_cmguserid,1,1) == "M" & !is.na(resolved)) %>% filter(is.na(Z_position)) %>% nrow() #40
data2 %>% filter(substr(Z_cmguserid,1,1) == "M" & !is.na(resolved)) %>% filter(is.na(Z_jobfamily)) %>% nrow() #11208
rm(jobfamily, position, temp)

#Let's do again missing jobfamily with position
data2 %>% filter(substr(Z_cmguserid,1,1) == "M" & !is.na(resolved)) %>% distinct(Z_jobfamily) %>% nrow()

temp <-
  data2 %>% filter(!is.na(Z_position)) %>% group_by(Z_position, Z_jobfamily) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(Z_jobfamily)) %>% arrange(Z_position, desc(n), desc(number)) 

jobfamily <- temp[!duplicated(temp$Z_position), ]
jobfamily$n <- NULL  
jobfamily$number <- NULL

idx <- which(!is.na(data2$Z_position) & is.na(data2$Z_jobfamily))
data2[idx,] <-
  data2[idx,] %>% left_join(jobfamily, by = "Z_position") %>%
  mutate(Z_jobfamily.x = Z_jobfamily.y)

#Now it has reduced from 11208 to 2639 for merck employee, this is good result!!!
data2 %>% filter(substr(Z_cmguserid,1,1) == "M" & !is.na(resolved)) %>% filter(is.na(Z_jobfamily)) %>% nrow() #2639

rm(jobfamily, temp)

# aggr(data2[,c("Z_jobfamily", "Z_position", "Y_country", "Z_cmguserid")], 
#      prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 
# 
# #Fill the missing position with the most frequent value of userid, , after imputation it reduced from 114,759 to 114,753
# position <- 
#   data2 %>% group_by(Z_cmguserid, Z_position) %>% dplyr::summarise(n = n())
# 
# position <-
#   position %>% group_by(Z_cmguserid) %>% filter(!is.na(Z_position)) %>% top_n(1, n) 
# 
# data2 <-
#   data2 %>% left_join(position, by = "Z_cmguserid") %>%
#   mutate(Z_position.x = ifelse(is.na(Z_position.x), Z_position.y, Z_position.x))
# data2$Z_position.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Z_position = Z_position.x)
# 
# 
# #Fill the missing jobfamily with the most frequent value of userid, after imputation it reduced from 126,304 to 126,027
# #Still many null data2 in jobfamily, position this is because incidents were requested by XUID externals who don't have jobfamily, position in HR suites
# #data2 %>% filter(substr(Z_cmguserid,1,1) != "M" | is.na(resolved)) 
# data2 %>% group_by(Z_jobfamily) %>% distinct(Z_jobfamily)
# data2 %>% group_by(Z_position) %>% distinct(Z_position)
# 
# jobfamily <- 
#   data2 %>% group_by(Z_cmguserid, Z_jobfamily) %>% dplyr::summarise(n = n()) 
# 
# jobfamily <-
#   jobfamily %>% group_by(Z_cmguserid) %>% filter(!is.na(Z_jobfamily)) %>% top_n(1, n)
# 
# data2 <-
#   data2 %>% left_join(jobfamily, by = "Z_cmguserid") %>%
#   mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
# data2$Z_jobfamily.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)
# 
# NAcol <- which(colSums(is.na(data2))>0)
# NAcols2 <- sort(colSums(sapply(data2[NAcol],is.na)), decreasing = T)
# 
# #Fill the missing jobfamily with the most frequent value of position
# # position <- 
# #   data2 %>% group_by(Z_position, Z_jobfamily) %>% dplyr::summarise(n = n())
# # 
# # position <-
# #   position %>% group_by(Z_position) %>% filter(!is.na(Z_jobfamily)) %>% top_n(1, n) 
# # 
# # data2 <-
# #   data2 %>% left_join(position, by = "Z_position") %>%
# #   mutate(Z_jobfamily.x = ifelse(is.na(Z_jobfamily.x), Z_jobfamily.y, Z_jobfamily.x))
# # data2$Z_jobfamily.y <- NULL
# # data2$n <- NULL
# # data2 <- data2 %>% dplyr::rename( Z_jobfamily = Z_jobfamily.x)


#Fill the missing resolved_by_id with the most frequent value of resolved_by, after imputation it reduced from 105174 to 16253
aggr(data2[,c("resolved_by_id", "resolved_by")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#resolved_by_id
temp <-
  data2 %>% filter(!is.na(resolved_by)) %>% group_by(resolved_by, resolved_by_id) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(resolved_by_id)) %>% arrange(resolved_by, desc(n), desc(number)) 

resolver <- temp[!duplicated(temp$resolved_by), ]
resolver$n <- NULL  
resolver$number <- NULL

idx <- which(!is.na(data2$resolved_by) & is.na(data2$resolved_by_id))
data2[idx,] <-
  data2[idx,] %>% left_join(resolver, by = "resolved_by") %>%
  mutate(resolved_by_id.x = resolved_by_id.y)


#Fill the missing X_bs_id with the most frequent value of assignment_grp, after imputation it reduced from 85337 to 75
aggr(data2[,c("resolved_by_id", "resolved_by", "X_bs_id", "assignment_grp")], 
     prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 

#X_bs_id
temp <-
  data2 %>% filter(!is.na(assignment_grp)) %>% group_by(assignment_grp, X_bs_id) %>% summarize(n = n(), number = max(number)) %>% 
  filter(!is.na(X_bs_id)) %>% arrange(assignment_grp, desc(n), desc(number)) 

BSID <- temp[!duplicated(temp$assignment_grp), ]
BSID$n <- NULL  
BSID$number <- NULL

idx <- which(!is.na(data2$assignment_grp) & is.na(data2$X_bs_id))
data2[idx,] <-
  data2[idx,] %>% left_join(BSID, by = "assignment_grp") %>%
  mutate(X_bs_id.x = X_bs_id.y)

BSID %>% write_csv("BSID_Assignment_GR2019.csv", na = "")
rm(resolver, temp, BSID)
# aggr(data2[,c("resolved_by_id", "resolved_by", "X_bs_id", "assignment_grp")], 
#      prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE) 
# 
# #Fill the missing resolved_by_id with the most frequent value of resolved_by, after imputation it reduced from 99,968 to 9,589
# resolver <- 
#   data2 %>% group_by(resolved_by, resolved_by_id) %>% dplyr::summarise(n = n())
# 
# resolver <-
#   resolver %>% group_by(resolved_by) %>% filter(!is.na(resolved_by_id)) %>% top_n(1, n) 
# 
# data2 <-
#   data2 %>% left_join(resolver, by = "resolved_by") %>%
#   mutate(resolved_by_id.x = ifelse(is.na(resolved_by_id.x), resolved_by_id.y, resolved_by_id.x))
# data2$resolved_by_id.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( resolved_by_id = resolved_by_id.x)
# 
# temp <-
#   data2 %>% filter(is.na(resolved_by_id))
# 
# temp <-
#   data2 %>% filter(resolved_by == "Dinesh Kambam")
# 
# #Fill the missing BSID with the most frequent value of assignment_Group, after imputation it reduced from 82,261 to 68
# data2 %>% group_by(X_bs_id) %>% distinct(X_bs_id)
# 
# BSID <- 
#   data2 %>% group_by(assignment_grp, X_bs_id) %>% dplyr::summarise(n = n())
# 
# BSID <-
#   BSID %>% group_by(assignment_grp) %>% filter(!is.na(X_bs_id)) %>% arrange(assignment_grp, desc(n)) %>% top_n(1, n)
# 
# BSID %>% filter(assignment_grp == "BT-LL_BREL LIMS")
# 
# data2 <-
#   data2 %>% left_join(BSID, by = "assignment_grp") %>%
#   mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
# data2$X_bs_id.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( X_bs_id = X_bs_id.x)
# 
# #Fill the missing BSID with the most frequent value of resolved_by_id, after imputation it reduced from 68 to 36
# BSID <- 
#   data2 %>% group_by(resolved_by_id, X_bs_id) %>% dplyr::summarise(n = n())
# 
# BSID <-
#   BSID %>% group_by(resolved_by_id) %>% filter(!is.na(X_bs_id)) %>% arrange(resolved_by_id, desc(n)) %>% top_n(1, n) 
# 
# data2 <-
#   data2 %>% left_join(BSID, by = "resolved_by_id") %>%
#   mutate(X_bs_id.x = ifelse(is.na(X_bs_id.x), X_bs_id.y, X_bs_id.x))
# data2$X_bs_id.y <- NULL
# data2$n <- NULL
# data2 <- data2 %>% dplyr::rename( X_bs_id = X_bs_id.x)


####final result after null imputation
NAcol2 <- which(colSums(is.na(data2))>0)
NAcols2 <- sort(colSums(sapply(data2[NAcol2],is.na)), decreasing = T)

#only for incidents requested by business users
temp <-
  data2 %>% filter(substr(Z_cmguserid,1,1) == "M", !is.na(resolved)) 

NAcol3 <- which(colSums(is.na(temp))>0)
NAcols3 <- sort(colSums(sapply(temp[NAcol3],is.na)), decreasing = T)

rm(temp)

#data2 null value imputation