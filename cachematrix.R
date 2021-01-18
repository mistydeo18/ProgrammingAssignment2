makeCacheMatrix <- function(a = matrix()) {
  x <- NULL
  set <- function(c) {
    a <<- c
    x <<- NULL
  }
  get <- function() a
  set_inverse <- function(inverse) 
    x <<- inverse
  get_inverse <- function() x
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(a, ...) {
  ## Return a matrix that is the inverse of 'a'
  x <- a$get-inverse()
  if(!is.null(x)) {
    message("getting inversed matrix")
    return(x)
  }
  data <- a$get()
  x <- solve(data, ...)
  a$set_inverse(x)
  x
}

