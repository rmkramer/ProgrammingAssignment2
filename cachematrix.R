## Put comments here that give an overall description of what your
##      functions do

## Functions facilitate the creation and storing of a matrix and its inverse in an environment
##      other than the environment of the solution thus saving space for the active code
##  makeCacheMatrix is the object definition of the cache saving the matrix and its inverse
##  cachesolve is the function which supports the creation of a matrix inverse in the cache 
##      or if already created pulls the inverse matrix from the cache 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
##  define thwe object to set the matrix in cache
    setmtx <- function(y) {
        x <<- y
        m <<- NULL
    }
##  define the object to get the matrix from the cache
    getmtx <- function() x
##  define the object to set the matrix inverse in the cache    
    setinv <- function(solve) m <<- solve
##  dwfine the object to get the matrix inverse from the cache
    getinv <- function() m
##  establish the cache
    list(setmtx = setmtx, getmtx = getmtx,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
##  Return a matrix that is the inverse of 'x'
##  get the matrix inverse from the cache environment
    m <- x$getinv()
##  if matrix invers has bee created return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
##  otherwise get the matrix from the cache environment and create the matrix inverse
    data <- x$getmtx()
    m <- solve(data)
##  store the matrix inverse in the cache environment
    x$setinv(m)
    m
}
