## These functions work together to efficiently compute and cache the inverse of a matrix:
##
## 1. makeCacheMatrix:
##    - Creates a special "matrix" object that can store a matrix and cache its inverse.
##    - Provides methods to set and get the matrix, as well as to set and get the cached inverse.
##
## 2. cacheSolve:
##    - Computes the inverse of the special "matrix" object created by makeCacheMatrix.
##    - If the inverse is already cached, it retrieves the cached result and skips computation.
##    - If the inverse is not cached, it computes the inverse, caches it, and returns the result.
##
## Together, these functions optimize repeated computations of matrix inverses by leveraging caching.


## makeCacheMatrix is a function that creates a special "matrix" object capable of caching its inverse. It includes:
## - setting the value of the matrix
## - getting the value of the matrix
## - setting the value of the cached inverse
## - getting the value of the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

matrix <- matrix(data = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 8, 7, 6, 5, 4, 3, 2), nrow = 4, ncol = 4, byrow = TRUE)
cached_matrix <- makeCacheMatrix(matrix)
cached_matrix$get()

new_matrix <- matrix(data = c(1, 3, 4, 5, 6, 7, 6, 9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 4, ncol = 4, byrow = TRUE)
cached_matrix$set(new_matrix)
cached_matrix$get()

inverse <- solve(cached_matrix$get())
cached_matrix$setinverse(inverse)
cached_matrix$getinverse()

## cacheSolve is a function that computes the inverse of the special "matrix" created by makeCacheMatrix.
## It first checks if the inverse is already cached.
## If cached, it retrieves the inverse with a message. 
## If not, it computes the inverse, caches it, and returns the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

my_matrix <- matrix(data = c(1, 3, 4, 5, 6, 7, 6, 9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 4, ncol = 4, byrow = TRUE)
cached_matrix <- makeCacheMatrix(my_matrix)

inverse1 <- cacheSolve(cached_matrix)
print(inverse1)

inverse2 <- cacheSolve(cached_matrix)
print(inverse2)
