## Creating 2 functions. First one - makeCacheMatrix function creates a matrix to store the inverse of a matrix. Second function checks is there is a cache, if yes it outputs
#the stores values and if not, it calculates the inverser of the value and stores in the cache.


## makeCacheFunction creates a list of function to set a matrix, get a matrix, set the inverser of a matrix, get the inverser of a matrix

## inv.matrix is a matrix that caches the inverse of a matrix. To initialize, inv.matrix is given NA values.
## Once user or cacheSolve function provides the inverse matrix value is in input in inv.matrix using set.inverse() function



makeCacheMatrix <- function(x = matrix()) {
  
  dim.matrix <- nrow(x)            # Calculating size of square matrix m
  inv.matrix <- matrix( , dim.matrix, dim.matrix) # Assigning NA values to inv.matrix
  
  set.mat <- function(y)
  {
    x <<- y
    dim.matrix <- nrow(x)            # Calculating size of square matrix m
    inv.matrix <<- matrix( ,dim.matrix, dim.matrix)  # Assigning NA values to inv.matrix    
  }
  
  get.mat <- function()              
    x
  
  set.inverse <- function(inv.matrix.cal)
    inv.matrix <<- inv.matrix.cal    # Assigning provided value to inv.matrix 
  
  get.inverse <- function()         # Fetching inverse matrix value
    inv.matrix
  
  list(set.mat = set.mat, get.mat = get.mat,    # creating list containing all functions
       set.inverse = set.inverse,
       get.inverse = get.inverse)
  
}


## cacheSolve fetches the inverse matrix value using get.inverse function and checks if there are NA values. If yes, then it
# solves for the inverse and stores in inv.matrix
# If thre are no NA values, it means inverse calculations has been done, and it simply print inv.matrix value

cacheSolve <- function(x, ...) {

inv.matrix <- x$get.inverse()

  if(is.na(inv.matrix[1,1] == TRUE)) ## Checking for NA value in inv.matrix
  {
    mat <- x$get.mat()
    inv.matrix <- solve(mat)   # Calculating inverse
    x$set.inverse(inv.matrix)  # Storing using set.inverse function
    
    print(inv.matrix)

  }

  else                 # If no NA values, it means inv.matrix already exists. Print inv.matrix
  {
    print("getting cached data")
    print(inv.matrix)
  }
}
