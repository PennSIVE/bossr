####
#### eLIFE C2-30
######################
options(bitmapType='cairo')
######################
########Library#######
######################
library(tiff)
library(oro.nifti)
library(dplyr)

##########################
#######Array value########
##########################
a1 <- as.integer(Sys.getenv('LSB_JOBINDEX'))

########################
####### Code ###########
########################
setwd("/home/ecbae/Jenn/")
control.mouse <- readTIFF("C2-30.tif", all=TRUE, info = TRUE)


tiff.info <- attr(control.mouse[[1]], "description")
Z.info <- unlist(strsplit(tiff.info, split = "\n"))[3]
Z <- as.numeric(unlist(strsplit(unlist(strsplit(Z.info, split="="))[2], split=","))[3])

T.info <- unlist(strsplit(tiff.info, split = "\n"))[4]
T <- as.numeric(unlist(strsplit(unlist(strsplit(Z.info, split="="))[2], split=","))[4])

# Resoultion information will be used later
X.resolution.info <- attr(control.mouse[[1]], "x.resolution")
Y.resolution.info <- attr(control.mouse[[1]], "y.resolution")

## Rotate
rotate <- function(x) t(apply(x, 2, rev))

nifti1 <- array(NA, dim=c(rev(dim(control.mouse[[1]])[1:2]), Z))
m <- 0
for(i in (Z*(a1-1) +1):(Z*a1)){
  m <- m+1
  # a1 = T
  # Note that we need to rotate 90 degree clockwise
  # to get the same image as FIJI
  nifti1[,,m] <- rotate(control.mouse[[i]])
}

rm(control.mouse)

control.nifti1 <- as.nifti(nifti1)



################################################
########## Retrieve ROI ##########
################################################
#setwd("/home/ecbae/Jenn/C2-30/")
setwd("/home/ecbae/Jenn/Sensitivity_analysis_Feb21/Image")
ROI_dat <- read.csv("C2_30.csv")

# for t = 1
mask.dat <- array(NA, dim = dim(control.nifti1))

if(a1 == 1){
  # t = 1
  print(a1)
  
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255
  }
  ROI.INDEX <- ROI$index
  
  ROI <- ROI_dat %>% filter(T == (a1+1))
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    
    if(ROI$index[i] %in% ROI.INDEX){
    }else{
    mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
    }
  }
  
  mask.dat <- as.nifti(mask.dat)
  # For ROI.txt
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$Z <- round(ROI$Z)
  
  setwd(paste0("./Images/",a1))
  for(i in 1:105){
    tiff(paste0("Overlayed_Images_",i,"_",a1,".tiff"), width = dim(control.nifti1)[1], height = dim(control.nifti1)[2])
    overlay.nifti(control.nifti1, mask.dat, z = i, plot.type = 'single')
    
    ROI.txt <- ROI %>% filter(Z < i+4 & Z > i-4)
    if(nrow(ROI.txt)>0){
    for(j in 1:nrow(ROI.txt)){
      text(x = ROI.txt$X[j]/dim(control.nifti1)[1], y = ROI.txt$Y[j]/dim(control.nifti1)[1], ROI.txt$index[j], col = "red")
    }
  }
    
    dev.off()
  }
}else if(a1 == T){
  # for t = T
  print(a1)
  
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
  }
  ROI.INDEX <- ROI$index
  
  ROI <- ROI_dat %>% filter(T == (a1-1))
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    if(ROI$index[i] %in% ROI.INDEX){
    }else{
      mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
    }
  }
  
  mask.dat <- as.nifti(mask.dat)
  # For ROI.txt
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$Z <- round(ROI$Z)
  
  setwd(paste0("./Images/",a1))
  for(i in 1:105){
    tiff(paste0("Overlayed_Images_",i,"_",a1,".tiff"), width = dim(control.nifti1)[1], height = dim(control.nifti1)[2])
    overlay.nifti(control.nifti1, mask.dat, z = i, plot.type = 'single')
    
    ROI.txt <- ROI %>% filter(Z < i+4 & Z > i-4)
    if(nrow(ROI.txt)>0){
    for(j in 1:nrow(ROI.txt)){
      text(x = ROI.txt$X[j]/dim(control.nifti1)[1], y = ROI.txt$Y[j]/dim(control.nifti1)[1], ROI.txt$index[j], col = "red")
    }
    }
    
    dev.off()
  }
}else{
  # for t = 2, .. T-1
  print(a1)
  
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
  }
  ROI.INDEX <- ROI$index
  
  
  ROI <- ROI_dat %>% filter(T == (a1+1))
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    if(ROI$index[i] %in% ROI.INDEX){
    }else{
      mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
    }
  }
  
  ROI <- ROI_dat %>% filter(T == (a1-1))
  ROI$X <- round(ROI$X)
  ROI$Y <- round(ROI$Y)
  ROI$Z <- round(ROI$Z)
  
  for(i in 1:nrow(ROI)){
    X.ROI <- (ROI$X[i]-5):(ROI$X[i]+5)
    Y.ROI <- (ROI$Y[i]-5):(ROI$Y[i]+5)
    Z.ROI <- (ROI$Z[i]-3):(ROI$Z[i]+3)
    #
    if(ROI$index[i] %in% ROI.INDEX){
    }else{
      mask.dat[X.ROI, Y.ROI, Z.ROI] <- 255 
    }
  }
  
  mask.dat <- as.nifti(mask.dat)
  # For ROI.txt
  ROI <- ROI_dat %>% filter(T == a1)
  ROI$Z <- round(ROI$Z)
  
  setwd(paste0("./Images/",a1))
  for(i in 1:105){
    tiff(paste0("Overlayed_Images_",i,"_",a1,".tiff"), width = dim(control.nifti1)[1], height = dim(control.nifti1)[2])
    overlay.nifti(control.nifti1, mask.dat, z = i, plot.type = 'single')
    
    ROI.txt <- ROI %>% filter(Z < i+4 & Z > i-4)
    if(nrow(ROI.txt)>0){
      for(j in 1:nrow(ROI.txt)){
        text(x = ROI.txt$X[j]/dim(control.nifti1)[1], y = ROI.txt$Y[j]/dim(control.nifti1)[1], labels= ROI.txt$index[j], col = "red")
      }
    }
    
    dev.off()
  }
}

