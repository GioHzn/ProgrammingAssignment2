makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}


#Write the following functions:

# makeCacheMatrix: # This function creates a special "matrix" object that can cache its inverse.

# cacheSolve: # This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
 ### It's comparable to the makeVector() function, but instead of a vector a matrix is cached.
 ### In the function the inverted matrix is calculated with solve()
 ### and cached in setinv. 
 ### Like the makeVector() function, a list is made of set, get, etc. to easily subset the objects
 ### Using the <<- operator, the objects are saved to the parent environment and thus saved
 ### even after the function has finished.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
### in this function the inverse matrix from makeCacheMatrix is returned
### as the invMatrix is stored in x$setinv /// x being the object and $setinv subsetting
### the first part of the function returns the cached inverted matrix
### the second part only calculates the inverted matrix if it hasn't been cached before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}


#An example test you can run in the console to test the functions :)
x <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(x)
