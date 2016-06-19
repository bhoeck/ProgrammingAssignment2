## The function makeCacheMatrix() creates a special matrix object that can
## cache it's inverse i. In addition the function utilizes getter and setter
## functions to store or return the inverse i. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## The function cacheSolve() computes the inverse i of the matrix created by
## function makeCacheMatrix(). In addition the function checks, whether
## the inverse i has already been calculated and stored. If the inverse i has 
## been calculated before, the cached result is returned. Otherwise the 
## inverse i is calculated, stored and returned. 

cacheSolve <- function(x, ...) {
	  i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


