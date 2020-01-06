Charcol <- names(data[,sapply(data, is.character)])
Numcol <- names(data[,sapply(data, is.numeric)])

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

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


data %>% filter(number == "INC2019483") %>% select(row, number)

temp <-
data %>% group_by(number) %>% summarize(n = n()) %>% filter(n >= 2)

data1 <- data %>% group_by(number) %>% top_n(1, n) 

country %>% group_by(Z_cmguserid) %>% filter(!is.na(Y_country)) %>% top_n(1, n) 


temp1 <-
temp %>% inner_join(data1, by = "number") 
temp1$n <- NULL

data <- data %>% distinct(number, .keep_all = TRUE)


data <- data1

rm(s1, s2, s3, s4)
data <- data %>% anti_join(temp) 

nrow(data)
nrow(temp1)
data <- rbind.fill(data, temp1)

colnames(temp1)
colnames(data)


getmode(data$Z_position)

temp<-
sort(table(data$request_div, data$Z_position), decreasing = T)

sort(table(data$Z_jobfamily(filter(data$request_div =='LS')), decreasing = T)

data[data$request_div =='LS', c(Z_jobfamily)]  

temp <- sort(table(filter(data, request_div =='LS')$Z_jobfamily) , decreasing = T)
temp <- sort(table(filter(data, request_div =='LS')$Z_position) , decreasing = T)     
table(match(data$Z_position, unique(data$Z_position)))

# helper function for plotting categoric data for easier data visualization
plot.categoric <- function(cols, df){
  for (col in cols) {
    order.cols <- names(sort(table(data[,col]), decreasing = TRUE))
    
    num.plot <- qplot(df[,col]) +
      geom_bar(fill = 'cornflowerblue') +
      geom_text(aes(label = ..count..), stat='count', vjust=-0.5) +
      theme_minimal() +
      scale_y_continuous(limits = c(0,max(table(df[,col]))*1.1)) +
      scale_x_discrete(limits = order.cols) +
      xlab(col) +
      theme(axis.text.x = element_text(angle = 30, size=12))
    
    print(num.plot)
  }
}

plot.categoric('Z_position', data)


data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot() + geom_density(aes(x=LT_day, ..scaled..)) + scale_x_log10(breaks = c(0, 1, 2, 3,5, 10,20, 40, 60))
data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(log(LT_day+1))) + geom_histogram(binwidth = 1) + xlim(0, 40)
data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT") %>% ggplot(aes(LT_day)) + geom_freqpoly(aes(color = request_div), binwidth = 1) + xlim(0, 30)

summary(data$LT_day)
temp <-filter(data, LT_day < 0)

temp %>% write_csv("check_leadtime.csv")

dim(data)

data %>% filter(!is.na(resolved), LT_day >0, !is.na(Y_cmg_shortname) | request_div != "IT")