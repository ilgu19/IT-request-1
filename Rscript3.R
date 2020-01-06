
################################################################################################################################################ 
## Missing value handling ##

#Check missing values in the data
NAcol <- which(colSums(is.na(data2))>0)
NAcols <- sort(colSums(sapply(data2[NAcol],is.na)), decreasing = T)

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

