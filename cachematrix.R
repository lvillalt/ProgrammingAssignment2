makeCacheMatrix <- function(x = matrix(), nr, nc, br) {
  ## PARAMETERS: x (matrix object), nr (number of rows), nc (num of columns)
  ##, br (byrow TRUE or FALSE) 
  ## OUTPUT: Creates an R object that contains the inverse of the matrix and
  ## the row and column sum vectors to determine if the matrix has changed
  ##
  
  ## The problem setup asks to do the inversion if the matrix has not changed, to this 
  ## effect we will calculate column and row sum vectors. If BOTH are identical 
  ## to the cached ones, we will use the cached inversion.
  ##
  ## If any vector is different, we will calculate a new inverse, and
  ## cache the new inverse, and the new column and row vectors

  ### The following is a sample console run showing the functions in use
  
  ##> source("cachematrix.R")
  ##> yMy <- makeCacheMatrix(c(1, 0, 2,  0, 0, 17,  0, 1, 0), 3, 3, TRUE)
  ##> YmY <- cacheSolve(yMy)
  ##> yMy$get()
  ##     [,1] [,2] [,3]
  ##[1,]    1    0    2
  ##[2,]    0    0   17
  ##[3,]    0    1    0
  ##> YmY
  ##     [,1]        [,2] [,3]
  ##[1,]    1 -0.11764706    0
  ##[2,]    0  0.00000000    1
  ##[3,]    0  0.05882353    0
  ##> yMy$get() %*% YmY
  ##     [,1] [,2] [,3]
  ##[1,]    1    0    0
  ##[2,]    0    1    0
  ##[3,]    0    0    1
  ##> YmY <- cacheSolve(yMy)
  ##getting cached data
  ##> YmY <- cacheSolve(yMy)
  ##getting cached data
 
  ### When first calling the makeCacheMatrix() function, values are NULLed
  ### and the matrix 'x', and its row and column sums are cached by a set() call
       i <- NULL
        set <- function(y, nr, nc, br) {
                x <<- matrix(y, nrow=nr, ncol=nc, byrow=br)
				rS <<- rowSums(x)
				cS <<- colSums(x)
                i <<- NULL
        }
		
  ### 	The get() function returns the matrix in 'x'
  ###   The set() functions cache the corresponding objects  
        get <- function() matrix(x, nrow=nr, ncol=nc, byrow=br)
        setinverse <- function(solve) i <<- solve
		setrowsums <- function() rS <<- rowSums(matrix(x, nrow=nr, ncol=nc, byrow=br))
		setcolsums <- function() cS <<- colSums(matrix(x, nrow=nr, ncol=nc, byrow=br))
		
  ###   This get() functions return the inverse, rowsums, and colsums		
        getinverse <- function() i
		getrowsums <- function() rS
		getcolsums <- function() cS
		
  ###   The object is a list with these elements		
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse,
			 setrowsums = setrowsums,
			 setcolsums = setcolsums,
			 getrowsums = getrowsums,
			 getcolsums = getcolsums)
}

cacheSolve <- function(x, ...) {
  ### PARAMETERS: x ( a cacheMatrix object), ... (All parameters required by the matrix() call)
  ### OUTPUT: i (inverse of matrix in x is displayed), i, rowsums, colsums are cached in x
  ### ASSUMPTION: Matrix is SQUARE and is INVERTIBLE
  
  ## return a matrix that is the inverse of x
  ## Since non square matrices (e.g. An n x m matrix where n != m) do not have
  ## an inverse, and the assumption given is that the matrix, x, has an inverse,
  ## then x is both SQUARE (n x n) and INVERTIBLE. 
  ##
  ## This function will then calculate a matrix Y, such that XY = YX = I_n
  ## where I_n is the n x n identity matrix. Then Y = X^-1

  ## The problem setup asks to do this if the matrix has not changed, to this 
  ## effect we will calculate column and row sum vectors. If BOTH are identical 
  ## to the cached ones, we will use the cached inversion.
  ##
  ## If any of the vectors is different, we will calculate a new inverse, and
  ## cache the new inverse, and the new column and row vectors
  
  ### We will first attempt to get the inverse, and the row and column sums
  ### from the object.  
         i <- x$getinverse()
		 locRsum <- rowSums(x$get())
		 locCsum <- colSums(x$get())
		 
  ###	If the inverse exists AND the sum vectors are identical to the cached ones,
  ###   use the cached inverse, print a message to that effect  
        if(!is.null(i)&&identical(locRsum, x$getrowsums())&&identical(locCsum, x$getcolsums())) {
                message("getting cached data")
                return(i)
        }
		
  ### Otherwise, get the matrix from the object, to solve for the inverse again
  ### from x$get() get a matrix with the correct elements, and number of rows and columns  
        data <- as.matrix(x$get(), nrow=nrow(x$get()), ncol=ncol(x$get()))
        i <- solve(data, ...)
		
  ### Use the 'set' functions to cache the new inverse and sums vectors		
        x$setinverse(i)
		x$setrowsums()
		x$setcolsums()
        i
  ### This last 'i' causes the inverse to print on the console		
}
