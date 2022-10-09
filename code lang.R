## option + - get <-

##### remove - list
# assign value "5" to object "x"
i <- 5
ls()
# remove x
rm(i)
# what is left?
ls()
# remove all objects
rm(list = ls())

ls()

##### ?apply 
x <- cbind(x1 = 3, x2 = c(4:1, 2:5))
dimnames(x)[[1]] <- letters[1:8]
apply(x, 2, mean, trim = .2) ##
col.sums <- apply(x, 2, sum)
row.sums <- apply(x, 1, sum)
rbind(cbind(x, Rtot = row.sums), Ctot = c(col.sums, sum(col.sums)))
stopifnot(apply(x, 2, is.vector))

##
apply(x, 2, sort)
names(dimnames(x)) <- c("row", "col")
x3 <- array(x, dim = c(dim(x),3),
            dimnames = c(dimnames(x), list(C = paste0("cop.",1:3))))
identical(x,  apply( x,  2,  identity))
identical(x3, apply(x3, 2:3, identity))

## ?abbreviate
