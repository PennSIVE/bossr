library(dplyr)
#'

########## MOBP3-Image1 ##########
########## DIM: 1057 x 1066 ##########
x_lower <- 1057 * 0
x_upper <- 1057 * 1

y_lower <- 1066 * 0
y_upper <- 1066 * 1

setwd("~/Dropbox/Oligo/BOSS_Counting")
mobp4_image1 <- read.csv("MOBP3_Image1_1-105_num.csv")[,-1]
## start here: dplyr::filter >>>
boundary.point <- unique(mobp4_image1$index[which(mobp4_image1$X < x_lower)])
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$X > x_upper)]))

boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y < y_lower)]))
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y > y_upper)]))

boundary.index <- unique(boundary.point)

mobp4_image1 <- mobp4_image1[which(!mobp4_image1$index %in% boundary.point),]
dat <- mobp4_image1
# <<< 
index <- sort(unique(dat$index))

result.index <- c()
# r = 30, z.r = 10
# combine neighboring indices into same cell >>>
for(i in index){
  index.X <- dat[which(dat$index==i),"X"]
  index.Y <- dat[which(dat$index==i),"Y"]
  index.Z <- dat[which(dat$index==i),"Z"]
  
  new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
    filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
    filter(Z <= max(index.Z)+10 & Z >= min(index.Z)-10) 
  
  # assign new.dat$index to current `index` here
  # skip >>>
  if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
    print(new.dat)
    index.t <- c()
    for(j in unique(new.dat$index)){
      index.t <- c(index.t, dat[which(dat$index == j),"T"])
    }
    
    if(length(index.t) == length(unique(index.t))){
      result.index<- rbind(result.index, sort(unique(new.dat$index)))}
    else{
      print("Good")
    }
    
  }else{
    print("Good")
  }
  # <<<
  
}

result.index <- result.index[which(duplicated(result.index)==FALSE),]

for(i in 1:nrow(result.index)){
  dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
}



###



###
# Deletion
t = 8
all.dat.del <- dat %>% filter(Z < 105)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(T >i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(T==i)
  all.del.count[i] <- sum(!(all.dat.del.tmp2$index %in% all.dat.del.tmp$index))
}
all.del.count


########## MOBP3-Image2 ##########
########## DIM: 1064 x 1053 ##########
x_lower <- 1064 * 0
x_upper <- 1064 * 1

y_lower <- 1053 * 0
y_upper <- 1053 * 1

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP3_Image2_1-105_num.csv")[,-1]
boundary.point <- unique(mobp4_image1$index[which(mobp4_image1$X < x_lower)])
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$X > x_upper)]))

boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y < y_lower)]))
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y > y_upper)]))

boundary.index <- unique(boundary.point)

mobp4_image1 <- mobp4_image1[which(!mobp4_image1$index %in% boundary.point),]
dat <- mobp4_image1

###
#Extra

index <- sort(unique(dat$index))

result.index <- c()
for(i in index){
  index.X <- dat[which(dat$index==i),"X"]
  index.Y <- dat[which(dat$index==i),"Y"]
  index.Z <- dat[which(dat$index==i),"Z"]
  
  new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
    filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
    filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 
  
  if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
    print(new.dat)
    index.t <- c()
    for(j in unique(new.dat$index)){
      index.t <- c(index.t, dat[which(dat$index == j),"T"])
    }
    
    if(length(index.t) == length(unique(index.t))){
      result.index<- rbind(result.index, sort(unique(new.dat$index)))}
    else{
      print("Good")
    }
    
  }else{
    print("Good")
  }
  
}

result.index <- result.index[which(duplicated(result.index)==FALSE),]

#for(i in 1:nrow(result.index)){
#  dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
#}

dat$index[which(dat$index==result.index[2])] <- result.index[1]

###
# Deletion
t = 7
all.dat.del <- dat %>% filter(Z < 105)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(T >i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(T==i)
  all.del.count[i] <- sum(!(all.dat.del.tmp2$index %in% all.dat.del.tmp$index))
}
all.del.count


########## MOBP4-Image1 ##########
########## DIM: 1087 x 1081 ##########
x_lower <- 1087 * 0
x_upper <- 1087 * 1

y_lower <- 1081 * 0
y_upper <- 1087 * 1

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP4_Image1_1-105_num.csv")[,-1]
boundary.point <- unique(mobp4_image1$index[which(mobp4_image1$X < x_lower)])
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$X > x_upper)]))

boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y < y_lower)]))
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y > y_upper)]))

boundary.index <- unique(boundary.point)

mobp4_image1 <- mobp4_image1[which(!mobp4_image1$index %in% boundary.point),]
dat <- mobp4_image1

## Extra

index <- sort(unique(dat$index))

result.index <- c()
for(i in index){
  index.X <- dat[which(dat$index==i),"X"]
  index.Y <- dat[which(dat$index==i),"Y"]
  index.Z <- dat[which(dat$index==i),"Z"]
  
  new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
    filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
    filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 
  
  
  if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
    print(new.dat)
    
    index.t <- c()
    for(j in unique(new.dat$index)){
      index.t <- c(index.t, dat[which(dat$index == j),"T"])
    }
    
    if(length(index.t) == length(unique(index.t))){
    result.index<- rbind(result.index, sort(unique(new.dat$index)))}
    else{
      print("Good")
    }
  }else{
    print("Good")
  }
  
}

result.index <- result.index[which(duplicated(result.index)==FALSE),]

for(i in 1:nrow(result.index)){
  dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
}


###


###
# Deletion
t = 7
all.dat.del <- dat %>% filter(Z < 105)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(T >i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(T==i)
  all.del.count[i] <- sum(!(all.dat.del.tmp2$index %in% all.dat.del.tmp$index))
}
all.del.count


########## MOBP4-Image2 ##########
########## DIM: 1109 x 1053 ##########
x_lower <- 1109 * 0.1
x_upper <- 1109 * 0.9

y_lower <- 1053 * 0.1
y_upper <- 1053 * 0.9

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP4_Image2_1-105_num.csv")[,-1]
boundary.point <- unique(mobp4_image1$index[which(mobp4_image1$X < x_lower)])
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$X > x_upper)]))

boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y < y_lower)]))
boundary.point <- c(boundary.point, unique(mobp4_image1$index[which(mobp4_image1$Y > y_upper)]))

boundary.index <- unique(boundary.point)

mobp4_image1 <- mobp4_image1[which(!mobp4_image1$index %in% boundary.point),]
dat <- mobp4_image1


## Extra
annot <- function(dat, t, minZ = 0, maxZ = 105){
  index <- sort(unique(dat$index))
  
  result.index <- c()
  for(i in index){
    index.X <- dat[which(dat$index==i),"X"]
    index.Y <- dat[which(dat$index==i),"Y"]
    index.Z <- dat[which(dat$index==i),"Z"]
    
    new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
      filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
      filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 
    
    if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
      #print(new.dat)
      index.t <- c()
      for(j in unique(new.dat$index)){
        index.t <- c(index.t, dat[which(dat$index == j),"T"])
      }
      
      if(length(index.t) == length(unique(index.t))){
        result.index<- rbind(result.index, sort(unique(new.dat$index)))}
      else{
        #print("Good")
      }
      
    }else{
      #print("Good")
    }
    
  }
  
  result.index <- result.index[which(duplicated(result.index)==FALSE),]
  if(length(result.index) !=2){
  for(i in 1:nrow(result.index)){
    dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
  }
  }else{
    dat$index[which(dat$index==result.index[2])] <- result.index[1]
  }
  
  ## Impute the cells that are missing in the middle of the timepoint #
  ## filling in t's in middle (copy rows)
  for(i in dat$index){
    
    cell.dat <- dat %>% filter(index ==i)
    
    if(length(unique(cell.dat$T)) != (max(cell.dat$T) - min(cell.dat$T) +1)){
      sequence.T <- min(cell.dat$T):max(cell.dat$T)
      missing.T <- sequence.T[which(! sequence.T %in% cell.dat$T)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.dat$size), mean(cell.dat$X), mean(cell.dat$Y), mean(cell.dat$Z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(dat)
      #print(impute.rows)
      dat <- rbind(dat, impute.rows)
      
    }else{
      #print("Good")
    }
  }
  
  
  ###
  
  ###
  # Counting
  all.dat.count <- dat %>% filter(Z <= maxZ) %>% filter(Z > minZ)
  
  ## Impute the cells that are missing in the middle of the timepoint #
  
  for(i in all.dat.count$index){
    
    cell.dat <- all.dat.count %>% filter(index ==i)
    
    if(length(unique(cell.dat$T)) != (max(cell.dat$T) - min(cell.dat$T) +1)){
      sequence.T <- min(cell.dat$T):max(cell.dat$T)
      missing.T <- sequence.T[which(! sequence.T %in% cell.dat$T)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.dat$size), mean(cell.dat$X), mean(cell.dat$Y), mean(cell.dat$Z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(dat)
      #print(impute.rows)
      all.dat.count <- rbind(all.dat.count, impute.rows)
      
    }else{
      #print("Good")
    }
  }
  
  ### >>> 
  all.count <- rep(NA, t)
  for(j in 1:t){
    all.count[j] <- as.numeric(all.dat.count %>% filter(T==j) %>% summarize(n = n()))
  }
  print("Count")
  print(all.count)
  ##3 <<<

  ### >>>
  # Addition
  all.dat.add <- dat %>% filter(Z <= maxZ) %>% filter(Z > minZ)
  
  ## Impute the cells that are missing in the middle of the timepoint #
  
  for(i in all.dat.add$index){
    
    cell.dat <- all.dat.add %>% filter(index ==i)
    
    if(length(unique(cell.dat$T)) != (max(cell.dat$T) - min(cell.dat$T) +1)){
      sequence.T <- min(cell.dat$T):max(cell.dat$T)
      missing.T <- sequence.T[which(! sequence.T %in% cell.dat$T)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.dat$size), mean(cell.dat$X), mean(cell.dat$Y), mean(cell.dat$Z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(dat)
      #print(impute.rows)
      all.dat.add <- rbind(all.dat.add, impute.rows)
      
    }else{
      #print("Good")
    }
  }
  
  all.add.count <- rep(NA, t)
  for(j in 1:t){
    roi.j <- all.dat.add %>% filter(T== j)
    roi.j1 <- all.dat.add %>% filter(T == j+1)
    
    all.add.count[j] <- length(roi.j1$index) - sum(roi.j1$index %in% roi.j$index)
  }
  print("Addition")
  print(all.add.count)
  
  # Deletion
  all.dat.del <- dat %>% filter(Z <= maxZ) %>% filter(Z > minZ)
  
  ## Impute the cells that are missing in the middle of the timepoint #
  
  for(i in all.dat.del$index){
    
    cell.dat <- all.dat.del %>% filter(index ==i)
    
    if(length(unique(cell.dat$T)) != (max(cell.dat$T) - min(cell.dat$T) +1)){
      sequence.T <- min(cell.dat$T):max(cell.dat$T)
      missing.T <- sequence.T[which(! sequence.T %in% cell.dat$T)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.dat$size), mean(cell.dat$X), mean(cell.dat$Y), mean(cell.dat$Z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(dat)
      #print(impute.rows)
      all.dat.del <- rbind(all.dat.del, impute.rows)
      
    }else{
      #print("Good")
    }
  }
  
  all.del.count <- rep(NA, t)
  for(i in 1:t){
    all.dat.del.tmp <- all.dat.del %>% filter(T >i)
    all.dat.del.tmp2 <- all.dat.del %>% filter(T==i)
    all.del.count[i] <- sum(!(all.dat.del.tmp2$index %in% all.dat.del.tmp$index))
  }
  print("Deletion")
  print(all.del.count)
  
  # Survivor
  all.dat.survival <- dat %>% filter(Z <= maxZ) %>% filter(Z > minZ)
  
  ## Impute the cells that are missing in the middle of the timepoint #
  
  for(i in all.dat.survival$index){
    
    cell.dat <- all.dat.survival %>% filter(index ==i)
    
    if(length(unique(cell.dat$T)) != (max(cell.dat$T) - min(cell.dat$T) +1)){
      sequence.T <- min(cell.dat$T):max(cell.dat$T)
      missing.T <- sequence.T[which(! sequence.T %in% cell.dat$T)]
      
      impute.rows <- c()   
      for(tt in missing.T){ 
        impute.row <- cbind(i, mean(cell.dat$size), mean(cell.dat$X), mean(cell.dat$Y), mean(cell.dat$Z), tt)
        impute.rows <- rbind(impute.rows, impute.row)
      }
      
      colnames(impute.rows) <- colnames(dat)
      #print(impute.rows)
      all.dat.survival <- rbind(all.dat.survival, impute.rows)
      
    }else{
      #print("Good")
    }
  }
  
  all.survival.count <- rep(NA, t)
  for(i in 1:t){
    all.dat.survivor.tmp <- all.dat.survival %>% filter(T ==i+1)
    all.dat.survivor.tmp2 <- all.dat.survival %>% filter(T==1)
    all.survival.count[i] <- sum(all.dat.survivor.tmp2$index %in% all.dat.survivor.tmp$index)
  }
  print("Survivor")
  print(all.survival.count)
  
}



annot.dat <- function(dat, t, out){
  index <- sort(unique(dat$index))
  
  result.index <- c()
  for(i in index){
    index.X <- dat[which(dat$index==i),"X"]
    index.Y <- dat[which(dat$index==i),"Y"]
    index.Z <- dat[which(dat$index==i),"Z"]
    
    new.dat <- dat %>% filter(X <= max(index.X)+30 & X >= min(index.X)-30) %>% 
      filter(Y <= max(index.Y)+30 & Y >= min(index.Y)-30) %>%
      filter(Z <= max(index.Z)+5 & Z >= min(index.Z)-5) 
    
    if(length(unique(new.dat$index)) >1 && length(unique(new.dat$T)) == nrow(new.dat)){
      print(new.dat)
      index.t <- c()
      for(j in unique(new.dat$index)){
        index.t <- c(index.t, dat[which(dat$index == j),"T"])
      }
      
      if(length(index.t) == length(unique(index.t))){
        result.index<- rbind(result.index, sort(unique(new.dat$index)))}
      else{
        print("Good")
      }
      
    }else{
      print("Good")
    }
    
  }
  
  result.index <- result.index[which(duplicated(result.index)==FALSE),]
  for(i in 1:nrow(result.index)){
    dat$index[which(dat$index==result.index[i,2])] <- result.index[i,1]
  }
  
  
  ###
  
  ###
  write.csv(dat, file = out)
  
  
  
}

##################################
###### Another control mouse #####
# MOBP3-Image1
setwd("~/Dropbox/Oligo/BOSS_Counting")
dat <- read.csv("MOBP3_Image1_1-105_num.csv")[,-1]
annot(dat = dat, t = 10)
annot(dat = dat, t = 10, maxZ=35)
annot(dat = dat, t = 10, minZ=35, maxZ=70)
annot(dat = dat, t = 10, minZ=70, maxZ=105)

#annot.dat(dat = dat, t = 10, out = "~/Dropbox/Oligo/BOSS_Counting/Image/MOBP3_Image1.csv")

# MOBP3-Image2
dat <- read.csv("MOBP3_Image2_1-105_num.csv")[,-1]
annot(dat = dat, t = 9) # Error! Run the function
annot(dat = dat, t = 9, maxZ=35)
annot(dat = dat, t = 9, minZ=35, maxZ=70)
annot(dat = dat, t = 9, minZ=70, maxZ=105)

#annot.dat(dat = dat, t = 9, out = "~/Dropbox/Oligo/BOSS_Counting/Image/MOBP3_Image2.csv")


# MOBP4-Image1 # New dat
#dat <- read.csv("MOBP4_Image1_1-105_num.csv")[,-1]
setwd("~/Dropbox/Oligo/BOSS_Counting/Sensitivity_counting/")
dat <- read.csv("MOBP4_Image1_1-105_num.csv")[,-1]
annot(dat = dat, t = 10)
annot(dat = dat, t = 10, maxZ=35)
annot(dat = dat, t = 10, minZ=35, maxZ=70)
annot(dat = dat, t = 10, minZ=70, maxZ=105)

#annot.dat(dat = dat, t = 10, out = "~/Dropbox/Oligo/BOSS_Counting/Image/MOBP4_Image1.csv")

# MOBP4-Image2 # Modified dat
setwd("~/Dropbox/Oligo/BOSS_Counting")
dat <- read.csv("MOBP4_Image2_1-105_num.csv")[,-1]
annot(dat = dat, t = 9)
annot(dat = dat, t = 9, maxZ=35)
annot(dat = dat, t = 9, minZ=35, maxZ=70)
annot(dat = dat, t = 9, minZ=70, maxZ=105)

#annot.dat(dat = dat, t = 9, out = "~/Dropbox/Oligo/BOSS_Counting/Image/MOBP4_Image2.csv")

# Control 89

setwd("~/Dropbox/Oligo/BOSS_Counting")
dat <- read.csv("Control_89_1-105_num.csv")[,-1]
annot(dat = dat, t = 10) # Remove the timepoint 5 in counting, Remove the timepoint 4 in add/del/survivor.  Check the result_postprocess_Mar10
annot(dat = dat, t = 10, maxZ = 35)
annot(dat = dat, t = 10, minZ = 35, maxZ = 70)
annot(dat = dat, t = 10, minZ = 70, maxZ = 105)

#annot.dat(dat = dat, t = 10, out = "~/Dropbox/Oligo/BOSS_Counting/Image/C2_89.csv")

# Control 115 # New dat
#dat <- read.csv("Control_115_1-105_num.csv")[,-1]
setwd("~/Dropbox/Oligo/BOSS_Counting/Sensitivity_counting/")
dat <- read.csv("C2_115_1-105_num.csv")[,-1]
annot(dat = dat, t = 10) # Remove the timepoint 5 in counting, Remove the timepoint 4 in add/del/survivor.  Check the result_postprocess_Mar10
annot(dat = dat, t = 10, maxZ = 35)
annot(dat = dat, t = 10, minZ = 35, maxZ = 70)
annot(dat = dat, t = 10, minZ = 70, maxZ = 105)
#annot.dat(dat = dat, t = 9, out = "~/Dropbox/Oligo/BOSS_Counting/Image/C2_115.csv")

# Control 186 # New dat
#dat <- read.csv("Control_186_1-105_num.csv")[,-1]
dat <- read.csv("C2_186_1-105_num.csv")[,-1]
annot(dat = dat, t = 10) # Remove the timepoint 5 in counting, Remove the timepoint 4 in add/del/survivor.  Check the result_postprocess_Mar10
annot(dat = dat, t = 10, maxZ = 35)
annot(dat = dat, t = 10, minZ = 35, maxZ = 70)
annot(dat = dat, t = 10, minZ = 70, maxZ = 105)

#annot.dat(dat = dat, t = 9, out = "~/Dropbox/Oligo/BOSS_Counting/Image/C2_186.csv")



########################

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP3_Image1_manual.csv")

dat <- mobp4_image
###
# Deletion
t = 8
all.dat.del <- dat %>% filter(Z < 15)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(FRAME ==i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(FRAME==(i-1))
  all.del.count[i] <- sum(!(all.dat.del.tmp2$SERIES %in% all.dat.del.tmp$SERIES))
}
all.del.count

#################

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP3_Image2_manual.csv")

dat <- mobp4_image1
###
# Deletion
t = 8
all.dat.del <- dat %>% filter(Z < 15)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(FRAME ==i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(FRAME==(i-1))
  all.del.count[i] <- sum(!(all.dat.del.tmp2$SERIES %in% all.dat.del.tmp$SERIES))
}
all.del.count


###################

setwd("~/Desktop/")
mobp4_image1 <- read.csv("MOBP4_manual2.csv")

dat <- mobp4_image1
###
# Deletion
t = 8
all.dat.del <- dat %>% filter(Z < 15)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(FRAME ==i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(FRAME==(i-1))
  all.del.count[i] <- sum(!(all.dat.del.tmp2$SERIES %in% all.dat.del.tmp$SERIES))
}
all.del.count

mobp4_image1 <- read.csv("MOBP4_manual1.csv")

dat <- mobp4_image1
###
# Deletion
t = 8
all.dat.del <- dat %>% filter(Z < 15)
all.del.count <- rep(NA, t)
for(i in 1:t){
  all.dat.del.tmp <- all.dat.del %>% filter(FRAME ==i)
  all.dat.del.tmp2 <- all.dat.del %>% filter(FRAME==(i-1))
  all.del.count[i] <- sum(!(all.dat.del.tmp2$SERIES %in% all.dat.del.tmp$SERIES))
}
all.del.count







################################################################################
################################################################################
## Cuprizone Mouse ##
################################################################################
################################################################################
setwd("~/Dropbox/Oligo/BOSS_Counting")
# Cuprizone 30
dat <- read.csv("Cupri_30_1-105_num.csv")[,-1]
annot(dat = dat, t = 12)
annot(dat = dat, t = 12, maxZ=35)
annot(dat = dat, t = 12, minZ=35, maxZ=70)
annot(dat = dat, t = 12, minZ=70, maxZ=105)
# Cuprizone 37
dat <- read.csv("Cupri_37_1-105_num.csv")[,-1]
annot(dat = dat, t = 12)
annot(dat = dat, t = 12, maxZ=35)
annot(dat = dat, t = 12, minZ=35, maxZ=70)
annot(dat = dat, t = 12, minZ=70, maxZ=105)
# Cuprizone 90
dat <- read.csv("Cupri_90_1-105_num.csv")[,-1]
annot(dat = dat, t = 12)
annot(dat = dat, t = 12, maxZ=35)
annot(dat = dat, t = 12, minZ=35, maxZ=70)
annot(dat = dat, t = 12, minZ=70, maxZ=105)
# Cuprizone 97
dat <- read.csv("Cupri_97_1-105_num.csv")[,-1]
annot(dat = dat, t = 10)
annot(dat = dat, t = 10, maxZ=35)
annot(dat = dat, t = 10, minZ=35, maxZ=70)
annot(dat = dat, t = 10, minZ=70, maxZ=105)

# Cuprizone 99
dat <- read.csv("Cupri_99_1-105_num.csv")[,-1]
annot(dat = dat, t = 9)
annot(dat = dat, t = 9, maxZ=35)
annot(dat = dat, t = 9, minZ=35, maxZ=70)
annot(dat = dat, t = 9, minZ=70, maxZ=105)





