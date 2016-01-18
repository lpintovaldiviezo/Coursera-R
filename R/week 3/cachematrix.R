
## makeCacheMatrix is used to stored in memory the matrix which has been passed as parameter.

makeCacheMatrix <- function(x = matrix()) {
	  inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve is used to returned the inverse of the matrix passed as parameter. It can return the value stored in memory if it exists or calculated it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}