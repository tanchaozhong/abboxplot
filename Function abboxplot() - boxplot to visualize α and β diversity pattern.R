### function abboxplot() -  boxplot to show ¦Á and ¦Â diversity pattern based on a given dataset and a group factor  ###
# this function is to calculate and visualized ¦Á and ¦Â diversity pattern

# data required:
  # rowname - sites
  # colname - species
  # grp

abboxplot <- function(data,grp=grp,method="chao"){
  
#alpha diversity
  data.binary <- data
  data.binary[data.binary>0]=1
  alpha <- rowSums(data.binary)
  
#beta diversity
  library(vegan)
  level <- levels(grp)

  o=0;beta.grp<-NULL;beta<-NULL
  repeat{
    o=o+1
  # deal with one dataset at a time
  data.o <- data[grp==level[o],]
  
  commu.dis <- vegdist(data.o,method = method)
  beta.vector <- as.vector(commu.dis)
  
# document the results and grp
  title <- level[o]
  beta.grp <- c(beta.grp,rep(title,length(beta.vector)))
  beta <- c(beta,beta.vector)
    if(o==length(level))break
  }
  
# plot the alpha and beta diversity based on grps
  {
    par(mfrow=c(1,2))
      boxplot(alpha~grp,main=expression(alpha~" diversity"))
      boxplot(beta~beta.grp,main=expression(beta~" diversity"))
    par(mfrow=c(1,1))
  }
}

