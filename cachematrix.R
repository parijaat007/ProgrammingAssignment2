## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix constructs functions (setMatrix, getMatrix, setInverse 
## and getInverse) and returns a list containing these four functions. First, makeCacheMatrix 
## initializes the matrix, x (as an argument with a default value) and the inverse, Inv variables.
## Function setMatrix sets or resets the matrix to a desired value. Function getMatrix retrieves 
## the value of the matrix in order to supply it as a data for inverser computation. Function 
## setInverse does the caching by seting the object Inv to the value of the computed inverse. 
## Function getInverse retrieves the value of the cached object Inv from the cache, either to 
## be immediately returned (if it is not null) or be reset by Function setInverse and returned 
## therefater.
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


## Function cacheSolve simply recieves and executes the list of functions constructed 
## by Function makeCacheMatrix. First, the value of inverse in the cache is retrieved with a call to 
## Function getInverse. The retrieved inverse value is tested to ascertain it is not null. Not 
## being null indicates that the inverse of the concerned matrix had been previously computed 
## and cached. In that case, Function caheSolve simply display a message to indicate the situation 
## and returns the retrieved inverse value. If the retrieved inverse value is null, Function 
## cacheSolve goes on to retrieve the matrix with a call to Function getMatrix and pass the retrieved
## matrix as data argument to the in-built Function solve to compute the inverse. Thereafter, the 
## computed inverser is stored in the cache with a call to Function setInverse and also returned as 
## output of Function cacheSolve.
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
