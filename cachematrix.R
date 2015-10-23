## A pair of funcitons that cache the inverse of a matrix

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
         }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
        }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

x = rbind(c(1,2),c(1,3))
> m=makeCacheMatrix(x)
> m$get()
     [,1] [,2]
[1,]    1    2
[2,]    1    3
>  cacheSolve(m)
     [,1] [,2]
[1,]    3   -2
[2,]   -1    1
> cacheSolve(m)
getting cached data.
     [,1] [,2]
[1,]    3   -2
[2,]   -1    1
> 
