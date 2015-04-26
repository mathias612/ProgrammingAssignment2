#  The makeCacheMatrix function create with value initially null as default 
makeCacheMatrix <- function(x = numeric()) {
  # Initialize the value of the matrix inverse to null
  minverse <- NULL
  
  # Create function where values will be cached
  set <- function(y) {
    x <<- y
    minverse <<- NULL
  }
  
  # Get value of inverse 
  get <- function() x
  setsolve <- function(solve) minverse <<- solve
  # Gets inverse
  getsolve <- function() minverse
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

# The cacheSolve function determines if the inverse matrix has been completed or not and if not then performs the inverse
cacheSolve <- function(x, ...) {
  minverse <- x$getsolve()
  # If the inverse already exists it gets it and returns it
  if(!is.null(minverse)) {
    message("getting cached data")
    return(minverse)
  }
  
  # If inverse does not exist it calculates it
  data <- x$get()
  minverse <- solve(data, ...)
  x$setsolve(minverse)
  minverse
}

mat = matrix(c(2, 4, 1, 5), nrow=2, ncol=2) 
print(mat)
m1 <- makeCacheMatrix(mat)