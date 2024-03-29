#' Annotate data.frame
#'
#' This function takes in a dataframe that is result of post_process_df (or track_components) 
#' and an integer t (total time points), and returns a dataframe with 4 columns: 
#' `count`, `added`, `deleted`, and `survivor`. `count` is the number of cells at time point t. 
#' `added` is the number of new cells in time point t (relative to time point t - 1); 
#' `added` is always NA for t = 1. 
#' `deleted` is the number of cells at t that do not show up at later time points.
#' `survivor` is the number of cells at time point t that remain from time point 1.
#'
#' @param cell_df A dataframe that is the result from pos
#' @param t An integer value
#' @return A dataframe with 4 columns: count, addition, deletion, and survivor.
#' @import dplyr
#' @export
annotate_df = function (cell_df, t) {
  
  annot_df <- data.frame(t = seq_len(t)) # declare return object
  
  # get count at each t ----
  count_df <- dplyr::count(cell_df, t) |>
    dplyr::rename(count = n)
  annot_df <- annot_df |> 
    dplyr::full_join(count_df, by = 't')
  
  # get num added at each t: get indices at time point t that are new relative to time point t - 1 ----
  added_df <- split(cell_df$index, cell_df$t) |> # get indices at t
    
    slider::slide(~ setdiff(unlist(.x[2]), # indices at time point t (see .before)
                            unlist(.x[1])) |> # indices at time point t - 1
                    length() |> 
                    as.data.frame() |> 
                    setNames('added'), 
                  .before = 1) |> 
    dplyr::bind_rows(.id = 't') |> 
    dplyr::mutate(t = as.integer(t)) # add col for joining
  annot_df <- annot_df |> 
    dplyr::full_join(added_df, by = 't')
  annot_df[1, 'added'] <- NA # set 0 to NA; time point 1 does not have a time point before it
  
  # get num deleted: for time point t, how many indices from t-1 are not present anymore----
  del_df <- split(cell_df$index, cell_df$t) |>
    slider::slide(~ setdiff(unlist(.x[1]), # time point t - 1
                            unlist(.x[2])) |> # all later time points (excluding time point t)
                    length() |> 
                    as.data.frame() |> 
                    setNames('deleted'), 
                  .before = 1) |>
    dplyr::bind_rows(.id = 't') |> 
    dplyr::mutate(t = as.integer(t)) # add col for joining
  del_df[1, 'deleted'] <- NA
  annot_df <- annot_df |>  # join results 
    dplyr::full_join(del_df, by = 't')
  
  # get survivors: at time point t, how many cells remain from time point 1
  surv_df <- split(cell_df$index, cell_df$t) |>
    slider::slide(~ intersect(unlist(.x[1]), # indices at time point 1
                              unlist(.x[length(.x)])) |> # indices at time point t
                    length() |> 
                    as.data.frame() |> 
                    setNames('survivor'),
                  .before = Inf) |> 
    dplyr::bind_rows(.id = 't') |> 
    dplyr::mutate(t = as.integer(t)) # add col for joining
  annot_df <- annot_df |>  # join results 
    dplyr::full_join(surv_df, by = 't')
  annot_df[1, 'survivor'] <- NA # redundant to compare time point 1 with itself
  
  return(annot_df)
}

#' Make Overlay
#' 
#' This function creates an overlay by drawing boxes around centroids of cells in a dataframe, at a specified time point, as well as cells in adjacent time points.
#' 
#' @param cell_df A dataframe containing the cell data, with columns for x, y, z, t, and index
#' @param dims A numeric vector of length 4, representing the dimensions of the image in x, y, z, and t
#' @param t An integer representing the time point of interest
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
  ## get cells at time point t
  df_t <- cell_df |>
    dplyr::filter(.data$t == t) 
  
  index_t <- unique(df_t$index)
  
  ## search cells in adjacent time points
  # get adjacent time points if they exist (handles edge cases when t=1 and t=T)
  t_adj <- intersect(c(t-1, t+1), 
                     seq_len(dims[4]))
  # get the cells at the adj
  df_t_adj <- cell_df |>
    dplyr::filter(.data$t %in% t_adj, 
                  .data$index %in% index_t) 
  
  ## combine data.frames then make mask
  df_t_all <- rbind(df_t, df_t_adj) 
  
  overlay <- df_t_all |>
    split(seq_len(nrow(df_t_all))) |> # split data.frame into a list of rows
    purrr::map(draw_box) |> # draw box for each row
    purrr::reduce(pmax) # take 'union' 
  
  overlay[which(overlay == 0, arr.ind = TRUE)] <- NA
  overlay <- oro.nifti::as.nifti(overlay)
  return(overlay)
}

#' Plots a 3D image with an overlay
#' 
#' `plot_overlay` is a wrapper to `oro.nifti::overlay`. It takes a 3D image and an overlay that is the output of `make_overlay()` and plots the chosen slices.
#' @param img 3D nifti or array, usually the input of the boss model
#' @param overlay an overlay that is the result of `make_overlay`
#' @param z an integer vector representing the slices to be plotted
#' @return a plot where the cell bodies are marked by the a red overlay
#' @export
plot_overlay <- function(img, overlay, z = 1){
  
  if(length(dim(img)) >= 4) stop("Image must have 3 dimensions at most.") 
  
  oro.nifti::overlay(img,
                     overlay,
                     col.y = scales::alpha('red', 0.5),
                     z = z,
                     plot.type = 'single')
}


