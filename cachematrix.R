## caching the inverse of a matrix.Matrix inversion may cost much time to compute.So decreasing the time and optimizing
## the structure is quite important.

## I fisrt make a special "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list (set = set, get = get, setinverse = setinverse , getinverse = getinverse)
}


## Then, I compute the inverse of the special "matrix" mentioned above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- inv(data,...)
        x$setinverse(inv)
        inv
}
