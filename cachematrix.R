## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix function takes in a mtatrix and cacheSolve returns the inverse


## This function takes in x (matrix form). 'Solve' is used to find the inverse.
## This function creates a list with function to 
## get a matrix,
## set inverse of a matrix and
## getinverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialluy m is set to NULL
  m <- NULL
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(get = get,setinv = setinv,getinv = getinv)

}


## This function retrun the inverse of matrix x by using 'Solve' function

cacheSolve <- function(x, ...) {
  ## to retrieve the matrix aand storing it in m by using the getinv function
  m<-x$getinv()
  ## if the inverse is calculated before then 
  ## retrieve the previsously stored matrix
  if(!is.null(m))
  {
    message("Retrieving the inverse")
    return(m)
  }
  
  #else calculate the inverse
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  message("The required inverse is")
  print(m)
  ## Return a matrix that is the inverse of 'x'
}
