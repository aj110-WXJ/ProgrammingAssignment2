

##Finding the inverse of a large matrix could be time consuming operation. Thus, if the contents of the matrix is not changing, it may make sense to cache
## the inverse matrix, so when needed again, it can be looked up in the cache rather than recomputed. 



## This function creates a special matrix. It is a list containing a function that sets the value of the matrix, 
## gets the matrix, finds the value of the matrix, and gets the value of the matrix
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

## This following function calculates the inverse of the speical matrix created with the above function. 
## It first checks to see if the inverse has already been deduced and stored. 
## If so, it will get the inverse from the cache and skip the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    mx <- x$get()
    inv <- solve(mx, ...)
    x$setinverse(inv)
    inv
}






