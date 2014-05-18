## Provide fast repeated computation of matrix inverse
## by caching and reusing result

## Matrix object with cacheable inverse attribute

makeCacheMatrix <- function(x = matrix()) 
{
    i <- NULL
    set <- function(y)
    {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function (inverse) i <<- inverse
    getinverse <- function () i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Compute the inverse of the matrix object returned 
## by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
    i <- x$getinverse()
    if (!is.null(i))
    {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    # cache the inverse
    x$setinverse(i)
    
    i
}
