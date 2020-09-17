## makeCacheMatrix function is used to create and store a matrix.
## cacheSolve function is used to invert the matrix


makeCacheMatrix <- function(x = matrix()) {
    if(ncol(x) == nrow(x)){
      i <- NULL
      set <- function(y){
        x <<- y
        i <<- NULL
      }
      
      get <- function() x
      setinversion <- function(inversion) i <<- inversion
      getinversion <- function() i
      list(set = set, get = get, setinversion = setinversion, getinversion = getinversion)
      
    }else{
      message("This is an uninversitable matrix.")
      i <<- NULL
    }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinversion()
    if(!is.null(i)){
      message("getting the inverse of a matrix")
      return(i)
    }
    data <- x$get()
    i <- solve(data, matrix(1, nrow = nrow(data), ncol = ncol(data)), ...)
    x$setinversion(i)
    i
}
