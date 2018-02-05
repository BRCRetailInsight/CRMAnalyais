
setwd("C:/Users/rlund/Documents/RData/")
require(plyr)

contacts=read.csv("contacts.csv")

deduped.contacts=contacts[!duplicated(contacts$Full.Name),]
contacts=deduped.contacts

x<- count(contacts, c('Company.Name')) 

string=c("Chairman","Chief Executive|CEO|Managing Director","CFO|Chief Financial Officer|Finance Director","Director","Corporate Affairs|Public Affairs|Communications|Comms|press","Counsel|legal", "Human Resources|HR|People|Reward","Marketing","Analyst","Insight|Intelligence","operations","risk","Technology","Information|Data","ecommerce")

for (i in 1:length(string)){

y<- lapply(contacts$Job.Title, grepl, pattern=string[i],ignore.case=TRUE)

z <-data.frame(matrix(unlist(y), nrow=nrow(contacts), byrow=T))

cols <- sapply(z, is.logical)

z[,cols] <- sapply(z[,cols], as.numeric)

c=cbind(contacts[,c(1,6)],z)


#a=ddply(contacts, .(Company.Name), summarize, Sum=sum("Chief Executive|CEO|Managing Director"))

b=aggregate(c[,3]~Company.Name,data=contacts,FUN="sum")

x=cbind(x,b[,2])
names(x)[ncol(x)]=string[i]

}

write.csv(x,"analysis.csv")

