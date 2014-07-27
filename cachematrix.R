## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.
## Here below are a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: a special "matrix" object that can cache its inverse. 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	## set the value of the matrix.
	set <- function(y)
	{	## the <<- sign assigns a value to an object in an environment that is different from the current environment.
		x <<- y
                inv <<- NULL
	}
	
	## get the value of the matrix.
	get <- function() x
	
	## set the value of inverse of the matrix.
        setsolve <- function(solve) inv <<- solve
	
	## get the value of inverse of the matrix.
        getsolve <- function() inv
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

	## get inverse.
        inv <- x$getsolve()
		
	## check if inverse has already been calculated; If so, return the inv and cache the skip the computation.
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
		
	## Otherwise, calculate the inverse and set the value in the cache via the setsolve function.
        data <- x$get()
        inv <- solve(data, ...)
        x$setsolve(inv)
        inv
}
