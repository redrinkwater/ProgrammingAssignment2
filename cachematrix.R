## makeCacheMatrix and cacheSolve calculate and return the inverse of a matrix and cache the result

## makeCacheMatrix 
## produces a list of helper functions and creates the variable m, which stores the cache of the matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                         ##creates variable m, cache variable
  
  set <- function(y) {              ##creates function set - purpose of this is to replace the matrix x
    x <<- y                         ##sets x to equal y
    m <<- NULL                      ##sets m to equal null, clears cache
  }
  
  get <- function() {               ##returns x
    x
  }
  
  setmatrix <- function(matrix){    ##sets m to equal matrix
    m <<- matrix
  }
  
  getmatrix <- function(){          ##gets m
    m
  }
  
  list(set = set, get = get,        ##produces a list of the helper functions
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## cacheSolve
## takes makeCacheMatrix and either checks for a previously cached inverse of matrix and returns it, 
## or calculates the inverse of the matrix and caches and returns it

cacheSolve <- function(x, ...) {
  
  m <- x$getmatrix()              ##gets matrix from passed in cache matrix
  
  if(!is.null(m)) {               
    message("getting cached data")  ##returns the cached matrix if available
    return(m)                       
  }
                                  ##no matrix inverse cached, so needs to be calculated
  data <- x$get()                 ##gets the matrix
  m <- solve(data)                ##gets the inverse of matrix
  x$setmatrix(m)                  ##caches the inverse of matrix
  m
}



