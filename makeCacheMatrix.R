
#Here I create a function to generate the matrix.
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
 # First, I set the value of the vector with the matrix to an object "set"
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Then, after the value is set, the function gets it into the object "get"
  get <- function() x
  
  #Here solve gives the inverse of the matrix. So the function sets the value of the inverse and then gets it
  
 
  #its inverse with solve
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

#Here, after setting and getting the values of the matrix and the inverse, the program checks if the inverse
# was already calculated and, if not, it calculates it and applies it to the matrix
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
