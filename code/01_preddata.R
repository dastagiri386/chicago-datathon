rm(list =ls())


setwd("~/Dropbox/Citadel/code")
source("Head_file.R")
source("function.R")
# dat <- read.csv("../data/jobs.csv", colClasses = c("character"))
# table(dat$country, exclude = NULL)
# dim(dat)

sector <- read.csv("../data/sectors.csv", stringsAsFactors = F)
sector <- sector[, 1:207]
sector1 <- sector[sector$statistic =="Number of Employees (thousands)",]

st <- unique(sector1$state); sec = unique(sector1$sector)
dates <- names(sector1)[4:207]
dates1 <- gsub('^X(\\d+)\\.(\\d+?)', '\\1-\\2',dates)
emp_state_sector <- NULL
for(i in st){
    for( j in sec){
        #i = st[2]
        #j = sec[1]
        #idx <- which((sector1$state== i) *(sector1$sector == j) >0 )
        idx <- which((sector1$state == i) *(sector1$sector == j) >0 )
        if (length(idx) >0){
            emp_state_sector <- rbind(emp_state_sector,
                  data.frame(state = rep(i,204),
                       sector = rep(j, 204),
                       time = dates1,
                       number_emp_k = as.numeric(sector1[idx,4:207]))
                  )
        }
        cat(i, j, nrow(emp_state_sector), "\n")
    }
}
emp_state_sector<- data.frame(emp_state_sector)
emp_state_sector$time <- as.character(emp_state_sector$time)
emp_state_sector$time <- paste0(emp_state_sector$time, "-01")
emp_state_sector$time <- as.Date(emp_state_sector$time)

write.csv(emp_state_sector, file ="../data/emp_state_sector.csv", row.names = F)
selected <- st[1]
tmp <- subset(subset(emp_state_sector,sector== "Retail Trade"), 
              state %in% selected)
ggplot(tmp, aes(time, number_emp_k))+
    geom_line()

selected <- st
ggplot(subset(emp_state_sector,sector== "Retail Trade"), 
              aes(time, number_emp_k))+
          geom_line(aes(group = state, color =state))

ggplot(emp_state_sector, aes(time, number_emp_k))+geom_line(aes(group = sector, color = sector))


sector <- sector[,1:207]
headers <- names(sector)
times <- seq(as.Date('2000-01-01'), as.Date('2016-12-01'), by = 'month')
times <- gsub("-01$","", times)








education <- read.csv("../data/education.csv",  colClasses = c("character"))

econ_state <- read.csv("../data/econ_state.csv")
names(econ_state)

#2007 --400 
#1980 316

plot(as.numeric(econ_state[econ_state$state=='WI',400:519]))
plot(as.numeric(econ_state[econ_state$state=='WI',277:315]))
plot(as.numeric(econ_state[econ_state$state=='CA',277:315])

