
############################################################################
############################################################################

#From,

#      Gokul Kaisaravalli Bhojraj
#      Id: 80789
#      Business Intelligence

############################################################################
############################################################################

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

data<-read.csv(file="C:/Users/Gokul/Desktop/lab excercise 1/OhioSchool.csv",
               header=T,sep = ',',dec = '.')
summary(data)

# 1)

set.seed(100)

# 1) a

attach(data)
ran_row<-c()
sam_1<-0
avg<-c()
for(i in 1:10)
{
ran_row<-sample(1:nrow(data),size=100,replace = F)
sam_1<-data[ran_row,c("build_irn","teachers")]
avg[i]<-mean(sam_1$teachers)
}
summary(sam_1$teachers)
par(mfrow=c(1,2))
hist(avg)
plot(avg,type='b')
par(mfrow=c(1,1))

sam_average_no_teachers<-mean(avg)
sam_average_no_teachers

population_mean<-mean(data$teachers)
population_mean

summary(avg)
sd(avg)
var(avg)

# 1) b

Row_div<-length(data$ZIP)/100
ceiling(Row_div)

frame_1<-cbind(row=c(1:nrow(data),1:33),seq=rep(1:20,100))

select<- sample(1:20,1)
sample_2<-frame_1[frame_1[,2]==select,]
head(sample_2)
sys1<-data[sample_2[,1],c("build_irn","teachers")]
summary(sys1)
nrow(sys1)

avg_teachers<-mean(sys1$teachers)
avg_teachers

population_mean<-mean(data$teachers)
population_mean

#or

data
nrow(data)
summary(data)

data_sim<-data[c("build_irn","teachers")]
chosen_row<-sample(1:nrow(data_sim),1,replace=F)
data_sim[chosen_row,]
new_data<-data_sim
new_data
nrow(new_data)
summary(new_data)

for( i in 1:33)
{
  new_data<-rbind(new_data,data_sim[i,])
}
new_data
nrow(new_data)
summary(new_data)

selected_after_every_set<-c()
k<-0
ss<-c()
k<-sample(1:20,1)
  for(i in 1:nrow(new_data))
  {
    if(k<=2000)
    {
        selected_after_every_set<-rbind(selected_after_every_set,new_data[k,])
        ss[i]<-k
        k<-k+20
    }  
    else
        break()
       
  }
nrow(selected_after_every_set)

avg_systemmatic<-mean(selected_after_every_set$teachers)
avg_systemmatic

population_mean<-mean(data$teachers)
population_mean

# 1) c

data
ss<-unique(data$dist_irn)
dist_chosen<-sample(ss,30,replace=F)
cluster_dist<-c()
for(i in 1:30)
{
  cluster_dist<-rbind(cluster_dist,data[data$dist_irn==dist_chosen[i],])
}

avg_cluster<-mean(cluster_dist$teachers)
avg_cluster

population_mean<-mean(data$teachers)
population_mean

# 1) d

head(data)
summary(data)
nrow(data)

quantile(data$poverty,c(0.25,0.50,0.75,1))
q_25<-0.0611905
q_50<-0.1192420
q_75<-0.1935360
q_100<-1.0000000 

poverty_below_25  <- data[data$poverty<=q_25,]
nrow(poverty_below_25)

poverty_below_50_a <- subset(data,data$poverty>q_25)
nrow(poverty_below_50_a)
poverty_below_50  <- subset(poverty_below_50_a,poverty_below_50_a$poverty<=q_50)
nrow(poverty_below_50)

poverty_below_75_a  <- subset(data,data$poverty>q_50)
nrow(poverty_below_75_a)
poverty_below_75  <- subset(poverty_below_75_a,poverty_below_75_a$poverty<=q_75)
nrow(poverty_below_75)

poverty_below_100 <- subset(data,data$poverty>q_75)
nrow(poverty_below_100)

nrow(poverty_below_25)+nrow(poverty_below_50)+nrow(poverty_below_75)+nrow(poverty_below_100)

drawing_size<-(nrow(poverty_below_25)/nrow(data))*100
All_rows<-seq(1,length(poverty_below_25$dist_irn),1)
picked_row_strata_1<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_1<-data.frame(poverty_below_25[picked_row_strata_1,])

drawing_size<-(nrow(poverty_below_50)/nrow(data))*100
All_rows<-seq(1,length(poverty_below_50$dist_irn),1)
picked_row_strata_2<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_2<-data.frame(poverty_below_50[picked_row_strata_2,])

drawing_size<-(nrow(poverty_below_75)/nrow(data))*100
All_rows<-seq(1,length(poverty_below_75$dist_irn),1)
picked_row_strata_3<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_3<-data.frame(poverty_below_75[picked_row_strata_3,])

drawing_size<-(nrow(poverty_below_100)/nrow(data))*100
All_rows<-seq(1,length(poverty_below_25$dist_irn),1)
picked_row_strata_4<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_4<-data.frame(poverty_below_100[picked_row_strata_4,])

all_sample<-rbind(picked_data_strata_1,picked_data_strata_2,
                  picked_data_strata_3,picked_data_strata_4)

avg_teachers<-mean(all_sample$teachers)
avg_teachers

population_mean<-mean(data$teachers)
population_mean

# 2

population_sd <- 46
error <- 4 
confidence_interval<- 95
z <- 1.96 
sample_size <- z^2*(population_sd^2/error^2)
round(sample_size)

# 3

# Algorithm 1
# Stratified Random Sampling Plans:
# Considering the whole data as a single stratum

All_rows<-seq(1,length(data$dist_irn),1)
picked_rows<-sample(All_rows,size=30,replace=FALSE)
picked_data<-data.frame(data[picked_rows,])
nrow(picked_data)

avg_teachers<- mean(picked_data$teachers)
avg_teachers

population_mean<-mean(data$teachers)
population_mean


# Algorithm 2
# Segmentized Sampling Plans:

quantile(data$poverty,c(0.25,0.50,0.75,1))
q_25<-0.0611905
q_50<-0.1192420
q_75<-0.1935360
q_100<-1.0000000 

poverty_below_25  <- data[data$poverty<=q_25,]
nrow(poverty_below_25)

poverty_below_50_a <- subset(data,data$poverty>q_25)
nrow(poverty_below_50_a)
poverty_below_50  <- subset(poverty_below_50_a,poverty_below_50_a$poverty<=q_50)
nrow(poverty_below_50)

poverty_below_75_a  <- subset(data,data$poverty>q_50)
nrow(poverty_below_75_a)
poverty_below_75  <- subset(poverty_below_75_a,poverty_below_75_a$poverty<=q_75)
nrow(poverty_below_75)

poverty_below_100 <- subset(data,data$poverty>q_75)
nrow(poverty_below_100)

nrow(poverty_below_25)+nrow(poverty_below_50)+nrow(poverty_below_75)+nrow(poverty_below_100)


drawing_size<-(nrow(poverty_below_25)/nrow(data))*30
All_rows<-seq(1,length(poverty_below_25$dist_irn),1)
picked_row_strata_1<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_1<-data.frame(poverty_below_25[picked_row_strata_1,])

drawing_size<-(nrow(poverty_below_50)/nrow(data))*30
All_rows<-seq(1,length(poverty_below_50$dist_irn),1)
picked_row_strata_2<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_2<-data.frame(poverty_below_50[picked_row_strata_2,])

drawing_size<-(nrow(poverty_below_75)/nrow(data))*30
All_rows<-seq(1,length(poverty_below_75$dist_irn),1)
picked_row_strata_3<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_3<-data.frame(poverty_below_75[picked_row_strata_3,])

drawing_size<-(nrow(poverty_below_100)/nrow(data))*30
All_rows<-seq(1,length(poverty_below_25$dist_irn),1)
picked_row_strata_4<-sample(All_rows,size=round(drawing_size),replace=FALSE)
picked_data_strata_4<-data.frame(poverty_below_100[picked_row_strata_4,])

all_sample<-rbind(picked_data_strata_1,picked_data_strata_2,
                  picked_data_strata_3,picked_data_strata_4)
nrow(all_sample)
avg_teachers<-mean(all_sample$teachers)
avg_teachers

population_mean<-mean(data$teachers)
population_mean

# Algorithm 3
# Systematic Sampling:

Row_div<-length(data$ZIP)/30
ceiling(Row_div)

frame_1<-cbind(row=c(1:nrow(data)),seq=rep(1:66,30))

select<- sample(1:66,1)
sample_2<-frame_1[frame_1[,2]==select,]
sys1<-data[sample_2[,1],c("build_irn","teachers")]
nrow(sys1)

avg_teachers<-mean(sys1$teachers)
avg_teachers

population_mean<-mean(data$teachers)
population_mean


##########################################################################

##########################################################################