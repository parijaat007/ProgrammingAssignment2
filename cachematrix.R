## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##Caching function for a matrix argument.
makeCacheMatrix <- function(x = matrix()) {
        Matrix <- NULL
	set <- function(y) {
		x <<- y
		Matrix <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) Matrix <<- inverse
	getinverse <- function() Matrix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##Calculates the inverse of an assumed square matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Matrix <- x$getinverse()
	if(!is.null(Matrix)) {
		message("getting data")
		return(Matrix)
	}
	data <- x$get()
	Matrix <- solve(data) %*% data
	x$setinverse(Matrix)
	Matrix
}
