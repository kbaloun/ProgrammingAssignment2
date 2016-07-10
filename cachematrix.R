## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x 
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


##  Compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##     then the cachesolve should retrieve the inverse from the cache.

#  For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function (x, ...) {

    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (!is.null(inv)) {
        print("pulling inverse from cache")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setinverse(inv)
    inv
        
    ## Computing the inverse of a square matrix can be done with the solve function in R. 
    ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
}

# tested with these matrix examples
# http://stackoverflow.com/questions/11995832/inverse-of-matrix-in-r
# ex: 
# c=rbind(c(1, -1/4), c(-1/4, 1))
# cacheSolve(makeCacheMatrix(c))
