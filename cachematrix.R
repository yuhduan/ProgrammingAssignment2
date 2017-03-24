## Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly. 


## This function creates a special "matrix" object that can cache its inverse.

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


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

# Testing
# a <- matrix(c(1,5,8,6,3,7,2,4,9), 3, 3)
# b <- makeCacheMatrix(a)
# b$get()
# results:  [,1] [,2] [,3] 
#    [1,]     1    6    2   
#    [2,]     5    3    4   
#    [3,]     8    7    9

# b$getinverse()
# results: NULL

# cacheSolve(b)
# results:      [,1]       [,2]       [,3]
#   [1,]  0.01754386  0.7017544 -0.3157895
#   [2,]  0.22807018  0.1228070 -0.1052632
#   [3,] -0.19298246 -0.7192982  0.4736842

# cacheSolve(b)
# results:
# getting cached data
#             [,1]       [,2]       [,3]
# [1,]  0.01754386  0.7017544 -0.3157895
# [2,]  0.22807018  0.1228070 -0.1052632
# [3,] -0.19298246 -0.7192982  0.4736842
