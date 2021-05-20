#Our functions will allow us to store in the cache the inverse of matrices (which is a costly computation).
#This way we will be able to compute the inverse of a big matrix just by assigning the matrix to the first functions and the result of the first function to the second one.
#To do so out two functions will have to store the matrix and cache its inverse.

## The first function we are going to creare is a function that creates a special matrix object that can cache its inverse by:
# setting the value of the matrix
# getting the value of the matrix
# setting the inverse of the matrix
# getting the inverse of the matrix

makeMatrix <- function(x = matrix()) {						
  m <- NULL									
  set <- function(y) {							
    x <<- y
    m <<- NULL
  }										
  get <- function() x							
  setsolve <- function(solve) m <<- solve				
  getsolve <- function() m						
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)	#now we put all these functions in a list, that will be the return of the makeMatrix function
}


#The second function we are going to create is a function that has as for arugument the previous function defined on a matrix (lets call it "test"), and as return retrieves the inversion of the "test" matrix itself.

cacheinvertedMatrix <- function(x, ...) {
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

###Checking
matrix1 <- matrix(1:4, ncol=2, nrow=2)
solve(matrix1) == cacheinvertedMatrix(makeMatrix(matrix1))
#As I can see with my functions I can compute the correct inverted matrix. Of course in my example I didn't need teh two functions because the matrix was small and easy to invert, though they can be useful for matrices containing big data.
