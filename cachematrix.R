## The functions below caches the inverse of matrix in a "special" matrix object
## so that recomputation of the inverse is avoided if it was already computed and
## the content of the matrix have not changed.

## Define a function that create the "special matrix" object as a list of functions
## used to set/get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set =set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse)
}

## Define a function that either returns the inverse of the matrix if it was already
## computed/cached or solves and caches its inverse if it wasn't already computed. 

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}
