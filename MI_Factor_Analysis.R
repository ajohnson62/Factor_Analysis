#####MI FA
library(ggplot2)
library(MCMCpack)
library(psych)
library("xtable")
library("psych") #Note, works with 1.5.8, doesn't with 1.7.8
library("reshape2")
library("ggplot2")
library("grid") #for adjusting plot margins
library("gridExtra")
library(cowplot)
library(mice)

#Getting MEG connectome from adjacency matrices
wd <- "U:/Documents/ACE_Data/Thesis_Analysis/MI_FA/"
setwd(wd)

#load data
data_imp <- readRDS('U:/Documents/ACE_Data/Thesis_Analysis/data_imp_not_scaled.RData')

domains <- read.csv('domains.csv', header = FALSE)
domain_code <- read.csv('domain_code.csv')
types <- read.csv('types.csv')

lookup_table <- cbind(domains, t(domain_code),  t(types))

#Get MI data 
mydata <- NULL

#Get the imputed datasets and scale
for(i in 1:5){
  mydata[[i]] <- complete(data_imp, i)
  mydata[[i]][,c(2,25,37,38)] <- (mydata[[i]][,c(2,25,37,38)]-1) #Set all dichotomous columns to 0/1 coding: Child.eats.breakfast.everyday,Mother.smoked.in.pregnancy, Most.common.method.of.praise.given.to.child, Handedness
  mydata[[i]]$How.often.child.sees.cousins[mydata[[i]]$How.often.child.sees.cousins==22] <- 2 #Correct mistake in data
  for(j in 1:ncol(mydata[[i]])){
    if(types[1,j]==1){ #Correct any dichotomous data that has been imputed to 2, down to 1
      mydata[[i]][,j][mydata[[i]][,j]==2] <- 1
    }
  }
  #Rename single measures
  colnames(mydata[[i]][,4:11]) <- c("Equivalised.income","Caregiver.education.av","Caregiver.occupation.av","Subjective.SES","Number.caregivers","Number.siblings","Primary.caregiver.hours.work","Primary.caregiver.skills" )
  colnames(mydata[[i]][,135:139]) <- c("Vocabulary","Matrix.Reasoning" ,"Phonological.processing","Reading.ability","Maths.ability" )
}

wd <- ("U:/Documents/ACE_Data/Thesis_Analysis/MI_FA/FA_results/")
setwd(wd)

View(lookup_table) #Use to find specific domain code

exp_var_all <- NULL
  
for(code in c(10:17,19:25, 31:32)){ 
  fa <- NULL
  exp_var <- NULL
  av_load <- NULL
  
  for(imp in 1:5){
    indeces <- which(domain_code==code)
    dom_name <- as.character(domains[indeces[1],1])
    
    data_temp <- mydata[[imp]][,indeces]
    d <- NULL
    p <- NULL
    x <- NULL
    d <- data.frame(data_temp[,which(types[,indeces]==1)])
    p <- data.frame(data_temp[,which(types[,indeces]==2)])
    x <- data.frame(data_temp[,which(types[,indeces]==3)])
    #### Put whichever columns you want to drop in here
    if(code ==12){ #Attitude child ed
      p$Importance.of.profession.to.caregiver <- NULL #not really child ed
      p$Importance.of.caregiver.own.educational.level <- NULL #not really child ed
      p$School.code <- NULL #not attitude
      d$Desigted.homework.space <- NULL
      
    }
    if(code ==13){ #Child health
      p$Childq2.attitude.appearance <- NULL #These load quite high but aren't really measures of child health
      p$Childq7.attitude.overall.wellbeing <- NULL
      #d$Ideal.family.day.out.scoring.active <- NULL
    }
    if(code ==14){ #parent health
      colnames(d) <- 'Caregiver.finds.life.stressful.on.average'
      #Removed the following as the factor was almost entirely parent stress and wellbeing, probably more meaningful for child development 
      x$Age.primary.caregiver <- NULL
      x$Number.of.alcohol.units <- NULL
      p$Primary.alcohol.consumption <- NULL
      p$Caregiver.participation.in.religion <- NULL #low loading, not sure what this shows
      p$Caregiver.conflict.with.partner <- NULL
      p <- cbind(p,p) #small hack to get around fact that error thrown if only one p column 

    }
    if(code ==15){ #time with family and friends
      #removed these as high correlation dominated factor yet aren't very meaningful
      d$Child.has.set.dinner.time <- NULL
      d$Child.sits.at.table.for.dinner <- NULL
    }
    if(code ==17){ #childcare
      d$Care.by.other.relative.16years.or.over <- NULL #only one that survives the 90% freq criteria of this question- not meaningful
      colnames(x) <- 'Age.child.started.regular.childcare'
    }
    if(code ==20){ #Discipline
      p$Discipline.by.shouting <- p$Disciplin.by.shouting
      p$Disciplin.by.shouting <- NULL
    }
    if(code ==21){ #language names
      colnames(p) <- 'Amount.Time.English.used.at.home'
      p <- cbind(p,p) #small hack to get around fact that error thrown if only one p column 
    }
    if(code ==22){ #reading names
      colnames(d) <- 'Daily.newspaper'
      colnames(x) <- 'Use.of.local.library'
    }
    if(code ==24){ #rules names
      colnames(p) <- 'Always.know.where.child.is.going.out'
      d$Child.bathes.themselves <- NULL
      d$Child.not.allowed.to.go.out.unsupervised <- NULL
      p$Always.know.where.child.is.going.out <- NULL
    }
    if(code ==25){
      x$WM.MX.Proc.std <- NULL #as just measured in main MX
      data_temp$WM.MX.Proc.std <- NULL #as just measured in main MX
    }
    if((ncol(d)>0) & (ncol(p)>0) & (ncol(x)>0)){
      data_corr <- mixed.cor( p=p, d=d, x=x, global = FALSE, smooth = TRUE, ncat=8)
      if(code ==14){#Hack to remove repeated variable incaregiver wellbeing
        data_corr$rho <- data_corr$rho[c(1:3,5),c(1:3,5)]
        p <- data.frame(p[,1])
        colnames(p) <- 'Caregiver.gets.enough.support'
      }
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- cbind(x,p,d)
    }
    if(ncol(d)==0 & ncol(p)>0 & ncol(x)>0){
      data_corr <- mixed.cor( p=p,x=x, global = FALSE, smooth = TRUE, ncat=8)
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- cbind(x,p)
    }
    if(ncol(d)>0 & ncol(p)==0 & ncol(x)>0){
      data_corr <- mixed.cor( d=d, x=x, global = FALSE, smooth = TRUE, ncat=8)
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- cbind(x,d)
    }
    if(ncol(d)>0 & ncol(p)>0 & ncol(x)==0){
      data_corr <- mixed.cor( p=p, d=d, global = FALSE, smooth = TRUE, ncat=8)
      if(code ==21){ #Hack to remove repeated variable in language use
        data_corr$rho <- data_corr$rho[2:4,2:4]
        p <- data.frame(p[,1])
        colnames(p) <- 'Amount.Time.English.used.at.home'
        }
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- cbind(p,d)
      
    }
    if(ncol(d)==0 & ncol(p)==0 & ncol(x)>0){
      data_corr$rho <- cor(x)
      data_for_scores <- x
    }
    if(ncol(d)==0 & ncol(p)>0 & ncol(x)==0){
      data_corr <- mixed.cor( p=p, global = FALSE, smooth = TRUE, ncat=8)
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- p
    }
    if(ncol(d)>0 & ncol(p)==0 & ncol(x)==0){
      data_corr <- mixed.cor( d=d, global = FALSE, smooth = TRUE, ncat=8)
      data_corr$rho <- xtable(unclass(data_corr$rho))
      data_for_scores <- d
    }
    if(code ==14){ #parent health hack to remove repeated p column
      
    }
    fa[[imp]] <- fa(data_corr$rho, nfactors=1, rotate = 'promax', fm = 'pa')
    if(code ==14){ #parent health
      fa[[imp]]$loadings <- -1*fa[[imp]]$loadings
      fa[[imp]]$weights <- -1*fa[[imp]]$weights
      
    }
    if(code ==31){ #BRIEF
      fa[[imp]]$loadings <- -1*fa[[imp]]$loadings
      fa[[imp]]$weights <- -1*fa[[imp]]$weights
      
    }
    if(code ==32){ #SDQ
      fa[[imp]]$loadings <- -1*fa[[imp]]$loadings
      fa[[imp]]$weights <- -1*fa[[imp]]$weights
      
    }
    print(fa[[imp]]$loadings,cut=.3)
    
    if(imp == 1){
      fa.parallel(data_corr$rho, n.obs= nrow(mydata[[1]]), fa="both", n.iter=100)
      fname <- paste(wd, '/',  dom_name, '_fa_parallel.jpeg', sep='' )
      dev.copy(jpeg,filename=fname);
      dev.off ();
      av_load <- fa[[imp]]$loadings
      fa[[imp]]$scores <- predict(fa[[imp]],data_for_scores)
      exp_var <- fa[[imp]]$values[1]/length(fa[[imp]]$values) #Save proportion of variance explained by first factor
    }
    if(imp >1){ #Rotate the loadings onto the first imputation
      fa[[imp]]$loadings <- procrustes(fa[[imp]]$loadings, fa[[1]]$loadings, translation=TRUE, dilation = TRUE)$X.new
      fa[[imp]]$scores <- predict(fa[[imp]],data_for_scores)
      fa[[imp]]$scores <- procrustes(fa[[imp]]$scores, fa[[1]]$scores, translation=TRUE, dilation = TRUE)$X.new
      av_load <- av_load + fa[[imp]]$loadings
      exp_var <- exp_var + fa[[imp]]$values[1]/length(fa[[imp]]$values) #Save proportion of variance explained by first factor
      
    }
    
    fname <- paste(wd, '/', imp, '_', dom_name, '_factor_scores.csv', sep='' )
    colnames(fa[[imp]]$scores) <- dom_name
    write.csv(fa[[imp]]$scores, file = fname)
    colnames(fa[[imp]]$loadings) <- dom_name
    fname <- paste(wd,  '/', imp, '_', dom_name, '_factor_loadings.csv', sep='' )
    write.csv(fa[[imp]]$loadings, file = fname)
    
  }
  
  #Save average loadings
  av_load <- av_load/length(fa)
  fname <- paste(wd,  '/', dom_name, '_average_factor_loadings.csv', sep='' )
  write.csv(av_load, file = fname)
  
  #Save proportion of variance explained by first factor
  exp_var <- exp_var/length(fa)
  fname <- paste(wd,  '/', dom_name, '_prop_variance_explained.csv', sep='' )
  write.csv(exp_var, file = fname)
  if(code==10){
    exp_var_all <- data.frame(dom_name, exp_var)
    colnames(exp_var_all) <- c('domain', 'Proprtion.Explained.Variance')
    exp_var_all$domain <- as.character(exp_var_all$domain)
    exp_var_all$Proprtion.Explained.Variance <- as.numeric(exp_var_all$Proprtion.Explained.Variance)
    
  }
  if(code > 10) {
    exp_var_all <- rbind(exp_var_all, c(dom_name, exp_var))
  }
}
#Sace proprtion of variance explained table
fname <- paste(wd,  '/all_prop_variance_explained.csv', sep='' )
write.csv(exp_var_all, file = fname)


### Make plots of loadings ###
loadings <- av_load[order(abs(av_load), decreasing = TRUE),]
fac_names <- colnames(loadings)
Var1 <- rownames(loadings)
loadings <- cbind(loadings, Var1)
loadings.m <- melt(loadings, id="Var1", 
                   measure=fac_names, 
                   variable.name="Factor", value.name="Loading")
loadings.m$Var1 <- factor(loadings.m$Var1, levels = rev(loadings.m$Var1)) #maintains order

#Plot the correlations in a heatmap
sort_corr <- mat.sort(data_corr$rho, fa[[1]])
sort_corr <- data.frame(sort_corr)
Var1 <- rownames(loadings)
sort_corr <- cbind(sort_corr, Var1)
sort_corr$Var1 <- factor(sort_corr$Var1, levels = rev(sort_corr$Var1)) #maintains order
corrs.m <- melt(sort_corr, id="Var1", variable.name="Var2", value.name="Correlation")

#place the tests on the x- and y-axes, 
#fill the elements with the strength of the correlation
hjust <- 1.5
if(code==10){marg <- 31}
if(code==11){marg <- 77}
if(code==12){
  marg <- 89
  hjust = c(rep(1.5,9), 0,0,0)}
if(code==13){marg <- 65}
if(code==14){marg <- 78}
if(code==15){marg <- 80}
if(code==16){marg <- 88}
if(code==17){marg <- 86}
if(code==19){marg <- 82}
if(code==20){marg <- 73}
if(code==21){ marg <- 71}
if(code==22){ 
  marg <- 67.5
  hjust = c(rep(1.5,8), 1.1)}
if(code==23){ marg <- 85}
if(code==24){ marg <- 83}
if(code==25){ marg <- 28}
if(code==31){ marg <- 49}
if(code==32){ marg <- 50}





p1 <- ggplot(corrs.m, aes(Var2, Var1, fill=abs(Correlation))) + 
  geom_tile(height=0.9,width = 0.9) + #rectangles for each correlation
  #add actual correlation value in the rectangle
  geom_text(aes(label = round(Correlation, 2)), size=3.5) + 
  theme_minimal(base_size=15) + #black and white theme with set font size
  #rotate x-axis labels so they don't overlap, 
  #get rid of unnecessary axis titles
  #adjust plot margins
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90,hjust=1,vjust=0.4), 
        axis.text.y = element_text(hjust=1,vjust=0.4),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        plot.margin = unit(c(2, 0, 5, 5), "mm")) +
  #set correlation fill gradient
  scale_fill_gradient(low="white", high="#ec6c20", limits=c(0,1))  + 
  guides(fill=FALSE) + #omit unnecessary gradient legend 
  labs(title="Correlation")
#Plot a bar chart of these loadings
p2 <- ggplot(loadings.m, aes(Var1, abs(Loading), fill=Loading)) + 
  #facet_wrap(~ Var2, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity",aes(width = 0.9)) + #make the bars
  geom_text(aes(label = round(Loading, 2)),  hjust = hjust,size=3.5) +
  coord_flip() + #flip the axes so the test names can be horizontal 
  #define the fill color gradient: blue=positive, red=negative
  scale_fill_gradient2(name = "Loading", 
                       high = "#ec6c20", mid = "white", low = "#0b81cc", 
                       midpoint=0, guide=FALSE, limits=c(-1,1)) +
  theme_minimal(base_size=15) + #use a black-and0white theme with set font size
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), 
        axis.title.y = element_blank(),axis.title.x = element_blank(),
        plot.margin = unit(c(2, 2, marg, -6), "mm")) + #c(top, right, bottom, left) 
  labs(title="Loading")
#Combine these two plots, tweak the 3rd element in p2 plot margin to align
grid.arrange(p1, p2, ncol=2, widths=c(2.8, 1))





