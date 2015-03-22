## Coursera R-Programming Assignment 2 (Date: 3/22/2015)

makeCacheMatrix <<- function(x) {
        
        det_matrix <<- det(x)
        
        minor <<- function (x,i,j){
              det_minor <- det(x[-i,-j])
              }
      
        get_cofactor <- function(x,i,j){
              ans <- (-1)^(i+j) * minor(x,i,j)
              }
                  
        get_adjoint_matrix <- function(x){
              n <<- nrow(x)
              adjoint_matrix <<- matrix(NA,n,n)  
                        for(i in 1:n){
                        for(j in 1:n){
                        adjoint_matrix[j,i] <- get_cofactor(x,i,j)
                        }}
              inversed_matrix <<- adjoint_matrix / det_matrix           
              }
        get_adjoint_matrix(x)

}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        if(det(x)==0){
                message("Please use an invertible matrix!")
                break
        }
        
        if(nrow(x)==2){
       
                det <- det(x)
                a11 <- x[1,1] 
                a12 <- x[1,2] 
                a21 <- x[2,1] 
                a22 <- x[2,2] 
                
                x[1,1] = a22 / det
                x[1,2] = -a12 / det
                x[2,1] = -a21 / det
                x[2,2] = a11 / det 
                
                print(x)
        } else{
                output <- makeCacheMatrix(x)    
                if(!is.null(output)){
                        message("You got it!  Good Job :D")
                        return(output)
                        }
                }

}
