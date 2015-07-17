## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#used the same format as for the cacheVector example:
#the argument of the function is a matrix given as input
#i = Inverse Matrix => initialized with an empty matrix having the same dimensions as x
#set = set function rewrites the values of the matrix x with matrix y and re-initialize the inverse matrix i with an empty matrix
#setInv = set function for the Inverse - with the argument being a matrix with the same dimensions as x

makeCacheMatrix <- function(x = matrix()) {
  i <- matrix(vector(), nrow=nrow(x), ncol=ncol(x))
  set <- function(y=matrix()) {
    x <<- y
    i <<- matrix(vector(), nrow=nrow(x), ncol=ncol(x))
  }
  get <- function() x
  setInv <- function(inv=matrix(vector(), nrow=nrow(x), ncol=ncol(x))) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

}


## Write a short comment describing this function
#i is initialized with the inverse of x
#if the inverse of x has not been calculated yet -iis a matrix filled with NAs
#na = is.na.data.frame will return a vector with TRUE values if the argument passed is a matrix filled with NAs
#if the sum(na) is 0 then the inverse matrix was not with NAs - has values
#will print the existing inverse
#if the sum(na) >0 then the inverse of the matrix is filled with NAs and the inverse will be calculated
#we store in data the original matrix
#in i we store the calculated inverse of the initial matrix 
#(if there is only 1 rgument given to the solve function it will return the inverse of the argument)
#we set the new inverse of x and then print it
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  na<-is.na.data.frame(i)
  if(sum(na)==0) 
    {
    message("getting cached data")
    print(i)
    }
  if(sum(na)>0)
    {
    data <- x$get()
    i <- solve(data)
    x$setInv(i)
    print(x$getInv())
    }
  
}
