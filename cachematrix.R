## The makeCacheMatrix is a special matrix object created to cache the inverse values for the matrix
## using the special operator <<- and setting up the solve function, the cacheSolve function can then retrieve the inverse values
## from the cache, saving computation time as long as the matrix has not changed.

## This function can save in the matrix the inverse values.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
            x <<- y
            m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
    }

## This function will check if the inverse data has already been calculated and cached and retrieve the inverse from the cache

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getmatrix()
    if(!is.null(m))
      message("getting cached data")
      return(m)
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
        ## Return a matrix that is the inverse of 'x'
 }

