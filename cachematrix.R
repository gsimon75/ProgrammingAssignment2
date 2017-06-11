## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly.

## `makeCacheMatrix` creates a wrapper object around a matrix that also can
## store the inverse (if it already has been calculated).
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	## set the wrapped matrix itself (and invalidate the cached inverse)
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	## get the wrapped matrix
	get <- function() x
	## set the cached inverse
	set_inverse <- function(new_inverse) inverse <<- new_inverse
	## get the cached inverse
	get_inverse <- function() inverse
	list(set = set, get = get,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}

## Return the inverse of a 'cached matrix' created by `makeCacheMatrix`
## If it has already been calculated, then use the cached value, otherwise
## calculate it, and cache the result for future use.
cacheSolve <- function(x, ...) {
	inverse <- x$get_inverse()
	if (!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	inverse <- solve(x$get(), ...)
	x$set_inverse(inverse)
	inverse
}

## Usage example:
##
## Import the definitions
#> source("cachematrix.R")
##
## Create a (non-cached) 3x3 matrix 
#> m <- matrix(nrow=3, ncol=3, c(1,2,3, 4,1,7, 6,7,8))
##
## Create a cached matrix wrapper around it
#> cm <- makeCacheMatrix(m)
##
## Check if it indeed contains our data
#> cm$get()
#     [,1] [,2] [,3]
#[1,]    1    4    6
#[2,]    2    1    7
#[3,]    3    7    8
##
## Check if it is invertable (because we assume it is)
#> solve(cm$get())
#           [,1]       [,2]       [,3]
#[1,] -0.9111111  0.2222222  0.4888889
#[2,]  0.1111111 -0.2222222  0.1111111
#[3,]  0.2444444  0.1111111 -0.1555556
##
## Get its inverse for the first time
## (Note that there is no "getting cached data" message)
#> cacheSolve(cm)
#           [,1]       [,2]       [,3]
#[1,] -0.9111111  0.2222222  0.4888889
#[2,]  0.1111111 -0.2222222  0.1111111
#[3,]  0.2444444  0.1111111 -0.1555556
##
## Get its inverse again, note the "getting cached data" message
#> cacheSolve(cm)
#getting cached data
#           [,1]       [,2]       [,3]
#[1,] -0.9111111  0.2222222  0.4888889
#[2,]  0.1111111 -0.2222222  0.1111111
#[3,]  0.2444444  0.1111111 -0.1555556

## vim: set ts=8 sw=8 noet tw=80 cc=+0:
