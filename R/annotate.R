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
annotate_df <- function(df, t){
  
  
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

#' Make Overlay
#' 
#' This function creates an overlay by drawing boxes around centroids of cells in a dataframe, at a specified timepoint, as well as cells in adjacent timepoints.
#' 
#' @param cell_df A dataframe containing the cell data, with columns for x, y, z, t, and index
#' @param dims A numeric vector of length 4, representing the dimensions of the image in x, y, z, and t
#' @param t An integer representing the timepoint of interest
#' 
#' @return A 3-dimensional binary array representing the overlay 
#' @export
make_overlay <- function(cell_df, dims, t){
  
  # draw box around a cell's centroid
  draw_box <- function(row){
    box <- array(0, dims[1:3])
    # handle edge cases
    X.ROI <- max(1,(row$x-5)):min(dims[1],(row$x+5))
    Y.ROI <- max(1,(row$y-5)):min(dims[2],(row$y+5))
    Z.ROI <- max(1,(row$z-3)):(row$z+3)
    # add color
    box[X.ROI, Y.ROI, Z.ROI] <- TRUE
    return(box)
  }
  
  if(! t %in% seq_len(dims[4])) stop("t is outside of the specified dimensions.")
  ## get cells at timepoint t
  df_t <- cell_df |>
    dplyr::filter(.data$t == t) 
  
  index_t <- unique(df_t$index)
  
  ## search cells in adjacent timepoints
  # get adjacent timepoints if they exist (handles edge cases when t=1 and t=T)
  t_adj <- intersect(c(t-1, t+1), 
                     seq_len(dims[4]))
  # get the cells at the adj
  df_t_adj <- cell_df |>
    dplyr::filter(.data$t %in% t_adj, 
                  .data$index %in% index_t) 
  
  ## combine data.frames then make mask
  df_t_all <- rbind(df_t, df_t_adj) 
  
  df_t_all |>
    split(seq_len(nrow(df_t_all))) |> # split data.frame into a list of rows
    purrr::map(draw_box) |> # draw box for each row
    purrr::reduce(pmax) # take 'union' 
}

