# First I create a matrix in order to test the fuctions.
# This matrix creator function it's inspired by the solve's rdocumentation 
#example: https://www.rdocumentation.org/packages/base/versions/3.6.1/topics/solve

createMatrix <- function(n){
    hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
    h <<- hilbert(n); h
}

# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


# This function computes the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

# To test the functions:

set.seed(230963)
r <- sample(5:10, 1)

createMatrix(r)

cachM <- makeCacheMatrix(h)
cachM$get()
cachM$getsolve()
cacheSolve(cachM)