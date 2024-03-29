% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/thresholding.R
\name{threshold_img}
\alias{threshold_img}
\title{Threshold an image}
\usage{
threshold_img(img, thr, n.cores = 1, ...)
}
\arguments{
\item{img}{image to be thresholded (2D, 3D, or 4D array)}

\item{thr}{threshold value (scalar, vector, or matrix)}

\item{n.cores}{number of cores to use for parallel processing (only used in threshold_3d and threshold_4d)}

\item{...}{extra arguments passed to mclapply}
}
\value{
binary mask of thresholded image (same dimensionality as input image)
}
\description{
The function takes an image and a threshold value and applies the appropriate threshold function
(threshold_2d, threshold_3d, or threshold_4d) based on the dimensionality of the image.
It also checks that the threshold value has the correct dimensionality and length to match the image.
}
