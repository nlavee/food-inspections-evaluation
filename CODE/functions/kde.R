#function to get temperature on heat map at a new point

# Kernal density estimation - estimate the density directly from the data without assuming a particular form for the underlying distribution
# More on : http://research.cs.tamu.edu/prism/lectures/pr/pr_l7.pdf

kde <- function (new, x, y, h) 
{
  nx <- length(x)
  
  # check to make sure the length of latitude and longitude are the same
  if (length(y) != nx)
    stop("data vectors must be the same length")
  
  # check to make sure all data are valid
  if (any(!is.finite(x)) || any(!is.finite(y))) 
    stop("missing or infinite values in the data are not allowed")
  
  h <- if (missing(h)) 
    c(bandwidth.nrd(x), bandwidth.nrd(y)) #if h is missing, we'll create our bandwidth based on x and y
  else rep(h, length.out = 2L)
  
  h <- h/4
  ax <- (new[1]-x)/h[1L]
  ay <- (new[2]-y)/h[2L]
  z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), 
                                                  , nx))/(nx * h[1L] * h[2L])
  z
}
