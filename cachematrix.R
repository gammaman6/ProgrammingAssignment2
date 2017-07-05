## creates a cache for the inverse of a specified matrix

## inititalize a null matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

## get matrix and set cache as the inverse and then return it
        
        get <- function() x
        setCache <- function(inverse) m <<- inverse
        getCache <- function() m
        
        list(set = set,
             get = get,
             setCache = setCache,
             getCache = getCache)
        
}


## returns the cached matrix, if not present will calculate inverse of specifiend matrix

cacheSolve <- function(x, ...) {
         m <- x$getCache()
        
        if (!is.null(m)) {
                message("loading cache matrix...")
                return(m)
        }
        
        ## Return a matrix that is the inverse of 'x'
        
        else {
                dat <- x$get()
                m <- solve(dat, ...)
                x$setCache(m)
                return(m)
        }
}
