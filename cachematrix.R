## Put comments here that give an overall description of what your
## functions do
##  
## This script caches the inversion of a matrix, so subsequent calls
## to the cacheSolve method will return the cache result instead 
## of going through the inversion again, which could be a pretty
## costly computation
#
## To test the script,
## example(solve) # get example matrix from solve
##  x1 <- makeCacheMatrix(h8)
##  cacheSolve(x1)
##  cacheSolve(x1)
## 
## on the second run, we should see a message 
## "getting cached matrix" at the prompt indicating the cache result
## is used

## Write a short comment describing this function
#
#  The following sets up the internal functions when making a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(matrix) m <<- matrix
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## Write a short comment describing this function
#  
#  invert an matrix using cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## retrieve the cached result
	m <- x$getMatrix()
	
	## check if anything in the cache
	if(!is.null(m)) {
		message("getting cached matrix")
		return(m)
	}
	
	## missed cache, go for the real computation
	data <- x$get()
	m <- solve(data, ...)
	
	## cache the result
	x$setMatrix(m)
	
	##return the result
        m
}
