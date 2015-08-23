## `makeCacheMatrix`: This function creates a special "matrix" object
##  that can cache its inverse.

## `cacheSolve`: This function computes the inverse of the special "matrix" 
##  returned by `makeCacheMatrix`.


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
