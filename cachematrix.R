## The following two defined functions create first a matrix object that can cache
## its inverse. The inverse of the matrix is then computed and returned.

## The first function makeCacheMatrix stores a matrix and caches its inverse. 
## The steps are: first the value of the matrix is set and then the function gets this
## value. Then the value of the inverse of the matrix is set and it is get in the last
## step. A list containing these steps is returned. 

makeCacheMatrix <- function(x = matrix()) {
        invs <- NULL
        set <- function(y) {
                x <<- y
                invs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invs <<- inverse
        getinverse <- function() invs
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
}



## The following function cacheSolve calculates the inverse of the matrix which was created
## with the above function. It first checks if the inverse has already been calculated.
## If yes, it gets the inverse from the cache and does not compute it. 
## If no, it calculates the inverse of the data and sets the value of the inverse
## in the cache. 

cacheSolve <- function(x, ...) {
        invs <- x$getinverse()
        if (!is.null(invs)) {
                message ("getting cached data")
                return(invs)
        }
        matrix <- x$get()
        invs <- solve(matrix, ...)
        x$setinverse(invs)
        invs
}
