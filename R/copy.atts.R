`copy.atts` <-
function(from,to,which=NULL,
         which.not=c("dim","dimnames","names","row.names","class")) {
    aaa <- get.atts(from=from,which=which,which.not=which.not)
    put.atts(to=to,atts=aaa)
}

