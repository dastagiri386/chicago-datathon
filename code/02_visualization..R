

emp_state_sector <- read.csv("../data/emp_state_sector.csv")
emp_state_sector$time <- as.Date(emp_state_sector$time)

#write.csv(emp_state_sector, file ="../data/emp_state_sector.csv", row.names = F)
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
     
     