### Script to pull out each of the scores for each imputed dataset
library(psych)
library(mice)
library(abind)

wd <- ("U:/Documents/ACE_Data/Thesis_Analysis/MI_FA/")
setwd(wd)
domains <- read.csv('domains.csv', header = FALSE)
domain_code <- read.csv('domain_code.csv')
types <- read.csv('types.csv')
uni_domains <- unique(domains)
rownames(uni_domains)<- uni_domains[,1]

data_imp <- readRDS('U:/Documents/ACE_Data/Thesis_Analysis/data_imp_scaled.RData')
#Get MI data 
mydata <- NULL
#Get the imputed datasets and scale
for(imp in 1:5){
  mydata[[imp]] <- complete(data_imp, imp)
  mydata[[imp]][,c(2,25,37,38)] <- (mydata[[imp]][,c(2,25,37,38)]-1) #Set all dichotomous columns to 0/1 coding: Child.eats.breakfast.everyday,Mother.smoked.in.pregnancy, Most.common.method.of.praise.given.to.child, Handedness
  mydata[[imp]]$How.often.child.sees.cousins[mydata[[imp]]$How.often.child.sees.cousins==22] <- 2 #Correct mistake in data
  mydata[[imp]]$Parent.occupation.av <- -1*mydata[[imp]]$Parent.occupation.av #recode so high is high occupation score
  for(j in 1:ncol(mydata[[imp]])){
    if(types[1,j]==1){ #Correct any dichotomous data that has been imputed to 2, down to 1
      mydata[[imp]][,j][mydata[[imp]][,j]==2] <- 1
    }
  #Rename single measures
  colnames(mydata[[imp]]) <- c(colnames(mydata[[imp]][,1:3]),"Equivalised.income","Caregiver.education.av","Caregiver.occupation.av","Subjective.SES","Number.caregivers","Number.siblings","Primary.caregiver.hours.work","Primary.caregiver.skills" , colnames(mydata[[imp]][,12:134]),"Vocabulary","Matrix.Reasoning" ,"Phonological.processing","Reading.ability","Maths.ability" , colnames(mydata[[imp]][,140:ncol(mydata[[imp]])]))
  }
}

wd <- ("U:/Documents/ACE_Data/Thesis_Analysis/MI_FA/FA_results/")
setwd(wd)

mydata_fa <- NULL
mydata_fa_new<- NULL
dom_name <- NULL
for(imp in 1:5){
  k <- 1
  for(j in c(10:17,19:25,31:32)){
    dom_name <- uni_domains[j,1]
    fname <- paste(wd, '/', imp, '_', dom_name, '_factor_scores.csv', sep='' )
    data_temp <- read.csv(fname)
    if(k==1){
      mydata_fa[[imp]] <- data.frame(data_temp[,2])
      colnames(mydata_fa[[imp]]) <- dom_name
    }
    else {
      mydata_fa[[imp]][,k] <- data.frame(data_temp[,2])
      colnames(mydata_fa[[imp]])[k] <- as.character(dom_name)
    }
    k <- k + 1
  }
  mydata_fa[[imp]] <- cbind(mydata[[imp]][,4:11],mydata_fa[[imp]][,1:15],mydata[[imp]][,135:139],mydata_fa[[imp]][,16:17])
}

saveRDS(mydata_fa, file='mydata_fa.RData') #Save dataset
#Scale over all MI sets so that any data that is not missing has the same score across sets
mydata_long <- abind(mydata_fa, along=1)
mydata_scaled <- scale(mydata_long, center=TRUE, scale=TRUE)
n <- nrow(mydata_fa[[1]])
mydata_fa_scaled <- NULL
for(imp in 1:5){
  mydata_fa_scaled[[imp]] <- mydata_scaled[(((imp-1)*n) + 1):(imp*n),]
  
}
saveRDS(mydata_fa_scaled, file='mydata_fa_scaled.RData') #Save dataset


#Make into a MIDS object (note, only the imputations will be correct...)
mydata_fa <- readRDS('~/PhD/MI_FA/MI_FA/FA_results/mydata_fa_scaled.RData')
mydata_long <- abind(mydata_fa, along=1)
fake_miss <- mydata_fa[[1]]
fake_miss[1,1] <- NA
fake_miss[5,2] <- NA

mydata_long <- rbind(fake_miss, mydata_long) #add 'non-imputed'set, note just setting to first dataset
stacked <- complete(data_imp, "long", include=TRUE) #to get .id and .imp coulmns
stacked <- cbind(stacked[,1:2], mydata_long)
data_imp_scaled <- as.mids(stacked, .imp=1)
