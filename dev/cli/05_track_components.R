library(dplyr)

roi_labels <- readRDS('results/roi_labels.xyzt')

final.index <- c()
for(k in 1:12){
  if(k <12){
    for(i in 1:105){ # <-------Q? Check continguity in T dimension
      index <- unique(as.vector(roi_labels[,,i,k]))
      index.next <- unique(as.vector(roi_labels[,,i,k+1]))
      #index.next.next <- unique(as.vector(my.roi[,,i+2]))
      for(j in index){
        if(j %in% index.next){
          final.index <- c(final.index, j)
        }else{
          final.index <- final.index
        }
      }
    }
  }else{  # <-------Q?
    for(i in 1:105){
      index <- unique(as.vector(roi_labels[,,i,k]))
      final.index <- c(final.index, index)
    }
  }
}

final.index <- unique(final.index)
unique.value <- final.index[-1]


######

k_vec1 <- c()
roi.array.final1 <- c()
i = 12

for(k in 1:i){
  l = 1:35
  final.index2 <- unique(as.vector(roi_labels[,,l,k]))[unique(as.vector(roi_labels[,,l,k])) %in% unique.value]
  
  
  my.roi <- roi_labels[,,l,k]
  
  final.index1 <- c()
  
  for(i in 1:(length(l)-1)){ # <-------Q? Check continguity in Z dimension. Stop at 2nd-to-last
    index <- unique(as.vector(my.roi[,,i]))
    index.next <- unique(as.vector(my.roi[,,i+1]))
    #index.next.next <- unique(as.vector(my.roi[,,i+2]))
    for(j in index){
      if(j %in% index.next){
        final.index1 <- c(final.index1, j)
      }else{
        final.index1 <- final.index1
      }
    }
  }
  
  
  final.index1 <- unique(final.index1)
  
  final.index <- final.index1[final.index1 %in% final.index2]
  
  # Now count
  size <- rep(0, length(final.index))
  index.x <- rep(0, length(final.index))
  index.y <- rep(0, length(final.index))
  index.z <- rep(0, length(final.index))
  
  for(i in 1:length(final.index)){
    size[i] <- length(which(my.roi==final.index[i]))
    index.xyz <- which(my.roi==final.index[i],arr.ind = TRUE)
    xyz <- apply(index.xyz, 2, mean)
    index.x[i] <- xyz[1]
    index.y[i] <- xyz[2]
    index.z[i] <- xyz[3]
  }
  
  my.roi.table <- data.frame(index = final.index, size = size, X = index.x, Y = index.y, Z = index.z, T = k)
  my.roi.table <- my.roi.table[my.roi.table$size>10,]
  
  roi.array <- c()
  roi.array <- cbind(my.roi.table[,1],k)
  roi.array.final1 <- rbind(roi.array.final1, roi.array)
  
  k_vec1 <- bind_rows(k_vec1, my.roi.table)
  
}

roi.add1 <- c()
roi.del1 <- c()
roi.array.final1 <- as.data.frame(roi.array.final1)

for(j in 1:11){ #<--- iterate over timepoits
  roi.j <- roi.array.final1 %>% filter(k== j)
  roi.j1 <- roi.array.final1 %>% filter(k == j+1)
  
  roi.add1[j] <- length(roi.j1$V1) - sum(roi.j1$V1 %in% roi.j$V1)
  roi.del1[j] <- length(roi.j$V1) - sum(roi.j$V1 %in% roi.j1$V1)
}

write.csv(k_vec1,"1-35_num.csv")
write.csv(roi.add1,"1-35_add.csv")
write.csv(roi.del1,"1-35_del.csv")


#####


k_vec2 <- c()
roi.array.final2 <- c()
i = 12

for(k in 1:i){
  l = 36:70
  final.index2 <- unique(as.vector(roi_labels[,,l,k]))[unique(as.vector(roi_labels[,,l,k])) %in% unique.value]
  
  
  my.roi <- roi_labels[,,l,k]
  
  final.index1 <- c()
  
  for(i in 1:(length(l)-1)){
    index <- unique(as.vector(my.roi[,,i]))
    index.next <- unique(as.vector(my.roi[,,i+1]))
    #index.next.next <- unique(as.vector(my.roi[,,i+2]))
    for(j in index){
      if(j %in% index.next){
        final.index1 <- c(final.index1, j)
      }else{
        final.index1 <- final.index1
      }
    }
  }
  
  
  final.index1 <- unique(final.index1)
  
  final.index <- final.index1[final.index1 %in% final.index2]
  
  
  size <- rep(0, length(final.index))
  index.x <- rep(0, length(final.index))
  index.y <- rep(0, length(final.index))
  index.z <- rep(0, length(final.index))
  
  for(i in 1:length(final.index)){
    size[i] <- length(which(my.roi==final.index[i]))
    index.xyz <- which(my.roi==final.index[i],arr.ind = TRUE)
    xyz <- apply(index.xyz, 2, mean)
    index.x[i] <- xyz[1]
    index.y[i] <- xyz[2]
    index.z[i] <- xyz[3]
  }
  
  my.roi.table <- data.frame(index = final.index, size = size, X = index.x, Y = index.y, Z = index.z, T = k)
  my.roi.table <- my.roi.table[my.roi.table$size>10,]
  
  roi.array <- c()
  roi.array <- cbind(my.roi.table[,1],k)
  roi.array.final1 <- rbind(roi.array.final1, roi.array)
  
  k_vec2 <- bind_rows(k_vec2, my.roi.table)
  
}

roi.add2 <- c()
roi.del2 <- c()
roi.array.final2 <- as.data.frame(roi.array.final2)

for(j in 1:11){
  roi.j <- roi.array.final2 %>% filter(k== j)
  roi.j1 <- roi.array.final2 %>% filter(k == j+1)
  
  roi.add2[j] <- length(roi.j1$V1) - sum(roi.j1$V1 %in% roi.j$V1)
  roi.del2[j] <- length(roi.j$V1) - sum(roi.j$V1 %in% roi.j1$V1)
}

write.csv(k_vec2,"36-70_num.csv")
write.csv(roi.add2,"36-70_add.csv")
write.csv(roi.del2,"36-70_del.csv")

###


k_vec3 <- c()
roi.array.final3 <- c()
i = 12

for(k in 1:i){
  l = 71:105
  final.index2 <- unique(as.vector(roi_labels[,,l,k]))[unique(as.vector(roi_labels[,,l,k])) %in% unique.value]
  
  
  my.roi <- roi_labels[,,l,k]
  
  final.index1 <- c()
  
  for(i in 1:(length(l)-1)){
    index <- unique(as.vector(my.roi[,,i]))
    index.next <- unique(as.vector(my.roi[,,i+1]))
    #index.next.next <- unique(as.vector(my.roi[,,i+2]))
    for(j in index){
      if(j %in% index.next){
        final.index1 <- c(final.index1, j)
      }else{
        final.index1 <- final.index1
      }
    }
  }
  
  
  final.index1 <- unique(final.index1)
  
  final.index <- final.index1[final.index1 %in% final.index2]
  
  
  size <- rep(0, length(final.index))
  index.x <- rep(0, length(final.index))
  index.y <- rep(0, length(final.index))
  index.z <- rep(0, length(final.index))
  
  for(i in 1:length(final.index)){
    size[i] <- length(which(my.roi==final.index[i]))
    index.xyz <- which(my.roi==final.index[i],arr.ind = TRUE)
    xyz <- apply(index.xyz, 2, mean)
    index.x[i] <- xyz[1]
    index.y[i] <- xyz[2]
    index.z[i] <- xyz[3]
  }
  
  my.roi.table <- data.frame(index = final.index, size = size, X = index.x, Y = index.y, Z = index.z, T = k)
  my.roi.table <- my.roi.table[my.roi.table$size>10,]
  
  roi.array <- c()
  roi.array <- cbind(my.roi.table[,1],k)
  roi.array.final1 <- rbind(roi.array.final1, roi.array)
  
  k_vec3 <- bind_rows(k_vec3, my.roi.table)
  
}

roi.add3 <- c()
roi.del3 <- c()
roi.array.final3 <- as.data.frame(roi.array.final3)

for(j in 1:11){
  roi.j <- roi.array.final3 %>% filter(k== j)
  roi.j1 <- roi.array.final3 %>% filter(k == j+1)
  
  roi.add3[j] <- length(roi.j1$V1) - sum(roi.j1$V1 %in% roi.j$V1)
  roi.del3[j] <- length(roi.j$V1) - sum(roi.j$V1 %in% roi.j1$V1)
}

write.csv(k_vec3,"71-105_num.csv")
write.csv(roi.add3,"71-105_add.csv")
write.csv(roi.del3,"71-105_del.csv")


###


k_vec4 <- c()
roi.array.final4 <- c()
i = 12

for(k in 1:i){
  l = 1:105
  final.index2 <- unique(as.vector(roi_labels[,,l,k]))[unique(as.vector(roi_labels[,,l,k])) %in% unique.value]
  
  
  my.roi <- roi_labels[,,l,k]
  
  final.index1 <- c()
  
  for(i in 1:(length(l)-1)){
    index <- unique(as.vector(my.roi[,,i]))
    index.next <- unique(as.vector(my.roi[,,i+1]))
    #index.next.next <- unique(as.vector(my.roi[,,i+2]))
    for(j in index){
      if(j %in% index.next){
        final.index1 <- c(final.index1, j)
      }else{
        final.index1 <- final.index1
      }
    }
  }
  
  
  final.index1 <- unique(final.index1)
  
  final.index <- final.index1[final.index1 %in% final.index2]
  
  
  size <- rep(0, length(final.index))
  index.x <- rep(0, length(final.index))
  index.y <- rep(0, length(final.index))
  index.z <- rep(0, length(final.index))
  
  for(i in 1:length(final.index)){
    size[i] <- length(which(my.roi==final.index[i]))
    index.xyz <- which(my.roi==final.index[i],arr.ind = TRUE)
    xyz <- apply(index.xyz, 2, mean)
    index.x[i] <- xyz[1]
    index.y[i] <- xyz[2]
    index.z[i] <- xyz[3]
  }
  
  my.roi.table <- data.frame(index = final.index, size = size, X = index.x, Y = index.y, Z = index.z, T = k)
  my.roi.table <- my.roi.table[my.roi.table$size>10,]
  
  roi.array <- c()
  roi.array <- cbind(my.roi.table[,1],k)
  roi.array.final1 <- rbind(roi.array.final1, roi.array)
  
  k_vec4 <- bind_rows(k_vec4, my.roi.table)
  
}

roi.add4 <- c()
roi.del4 <- c()
roi.array.final4 <- as.data.frame(roi.array.final4)

for(j in 1:11){
  roi.j <- roi.array.final4 %>% filter(k== j)
  roi.j1 <- roi.array.final4 %>% filter(k == j+1)
  
  roi.add4[j] <- length(roi.j1$V1) - sum(roi.j1$V1 %in% roi.j$V1)
  roi.del4[j] <- length(roi.j$V1) - sum(roi.j$V1 %in% roi.j1$V1)
}

write.csv(k_vec4,"1-105_num.csv")
write.csv(roi.add4,"1-105_add.csv")
write.csv(roi.del4,"1-105_del.csv")


# Save image files

C2_30_1 <- as.nifti(roi_labels[,,,1])
writeNIfTI(C2_30_1, "C2-30-1")
C2_30_2 <- as.nifti(roi_labels[,,,2])
writeNIfTI(C2_30_2, "C2-30-2")
C2_30_3 <- as.nifti(roi_labels[,,,3])
writeNIfTI(C2_30_3, "C2-30-3")
C2_30_4 <- as.nifti(roi_labels[,,,4])
writeNIfTI(C2_30_4, "C2-30-4")
C2_30_5 <- as.nifti(roi_labels[,,,5])
writeNIfTI(C2_30_5, "C2-30-5")
C2_30_6 <- as.nifti(roi_labels[,,,6])
writeNIfTI(C2_30_6, "C2-30-6")
C2_30_7 <- as.nifti(roi_labels[,,,7])
writeNIfTI(C2_30_7, "C2-30-7")
C2_30_8 <- as.nifti(roi_labels[,,,8])
writeNIfTI(C2_30_8, "C2-30-8")
C2_30_9 <- as.nifti(roi_labels[,,,9])
writeNIfTI(C2_30_9, "C2-30-9")
C2_30_10 <- as.nifti(roi_labels[,,,10])
writeNIfTI(C2_30_10, "C2-30-10")
C2_30_11 <- as.nifti(roi_labels[,,,11])
writeNIfTI(C2_30_11, "C2-30-11")
C2_30_12 <- as.nifti(roi_labels[,,,12])
writeNIfTI(C2_30_12, "C2-30-12")




