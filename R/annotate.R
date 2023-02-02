#' Annotate data.frame
#'
#' This function takes in a dataframe that is result of post.process.df (or track.components) 
#' and an integer t (total timepoints), and returns a dataframe with 4 columns: 
#' count, addition, deletion, and survivor. The count column
#' contains the number of rows in the input dataframe where the "t" column has
#' a value equal to j, for each j in 1 to t. The addition column contains the
#' number of new rows in the dataframe between time j and j + 1. The deletion
#' column contains the number of rows in the dataframe at time j that do not
#' appear in the dataframe at any later time. The survivor column contains
#' the number of rows in the dataframe at time j that also appear in the dataframe
#' at time j + 1.
#'
#' @param df A dataframe
#' @param t An integer value
#' @return A dataframe with 4 columns: count, addition, deletion, and survivor.
#' @import dplyr
#' @export
annotate.df <- function(df, t){
  
  
  # Count
  all.count <- rep(NA, t)
  for(j in 1:t){
    all.count[j] <- as.numeric(df|> filter(t==j)|> summarize(n = n()))
  }
  
  # Addition
  all.add.count <- rep(NA, t)
  for(j in 1:t){
    roi.j <- df|> filter(t== j)
    roi.j1 <- df|> filter(t == j+1)
    
    all.add.count[j] <- length(roi.j1$index) - sum(roi.j1$index %in% roi.j$index)
  }
  
  # Deletion
  all.del.count <- rep(NA, t)
  for(i in 1:t){
    all.dat.del.tmp <- df|> filter(t >i)
    all.dat.del.tmp2 <- df|> filter(t==i)
    all.del.count[i] <- sum(!(all.dat.del.tmp2$index %in% all.dat.del.tmp$index))
  }
  
  # Survival
  all.survival.count <- rep(NA, t)
  for(i in 1:t){
    all.dat.survivor.tmp <- df|> filter(t ==i+1)
    all.dat.survivor.tmp2 <- df|> filter(t==1)
    all.survival.count[i] <- sum(all.dat.survivor.tmp2$index %in% all.dat.survivor.tmp$index)
  }

  data.frame(t = 1:t,
             count = all.count, 
             addition = all.add.count,
             deletion = all.del.count,
             survivor = all.survival.count)
}

# TODO: add plotting function




# TODO: add make.overlay
