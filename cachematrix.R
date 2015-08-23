## these functions will return the inverse of a matrix.
## makeCacheMatrix will cache the inverse of a matrix and 
## cacheSolve will use the stored cache matrix or will create a new 
## inverse matrix and makeCacheMatrix will cache it.
##
## makeCacheMatrix function will cache the inverse of a matrix
## set a list of options to allow the calling function to:
##	get the cached matrix
##	set the cached matrix
##	get the cached inverse matrix
##	set the cached inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
#
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
#
	get <- function() {x}
#
	setInv <- function(solve) {m <<- solve}
#
	getInv <- function() {m}
#
	list(set = set, get=get,
		 setInv = setInv,
		 getInv = getInv)
}

## The cacheSolve function will return a Matrix that is the inverse of Matrix 'x'
## if the Matrix does not change the function will use the cached inverse from 
## the makeCacheMatrix function else it will create an inverse of the Matrix and 
## pass to makeCacheMatrix to save in the cache.
##
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        print("getting cached data")
        return(m)
	}
	else {
		data <- x$get()
		m <- solve(data, ...)
		x$setInv(m)
		print("setting cached data")
		m
	}
}
