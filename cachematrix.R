## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix Function contains
##	setmatrix function which sets the matrix
##	getmarix function which returns the matix value set through setmatrix function
##	setinverse function which sets the inverse matrix in global variable
##	getinverse function which returns the inverse matrix set through setinverse function
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function () x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list(setmatrix = setmatrix, 
		getmatrix = getmatrix, 
		setinverse=setinverse,
	 	getinverse=getinverse)
}


## Write a short comment describing this function
## Cachsolve function computes the inverse of matrix through solv function if the inverse matrix is not computed earlier. 
## If the inverse matrix is already computed then the function returns the value from the cache (value persisted in global variable)
cacheSolve <- function(x, ...) {
	m <- x$getinverse()
        ## Return a matrix that is the inverse of 'x'
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$getmatrix()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}
