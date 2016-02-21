## Caching the inverse of matrix

## Here I have written two functions that can help to gain better performance.
## Sometimes it is better use cache instead of recomputing values twice.

## The first function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		MyInversion <- NULL
        set <- function(y) {
                x <<- y
                MyInversion <<- NULL
        }
        get <- function() x
        setInversion <- function(inverse) MyInversion <<- inverse
        getInversion <- function() MyInversion
        list(set = set,
             get = get,
             setInversion = setInversion,
             getInversion = getInversion)

}

## The second function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

        MyInversion <- x$getInversion()
        if (!is.null(MyInversion)) {
                message("getting cached data")
                return(MyInversion)
        }
        m <- x$get()
        MyInversion <- solve(m, ...)
        x$setInversion(MyInversion)
        MyInversion
}
