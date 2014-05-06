patiencegame <- function(n){
    deck <- sample(1:n)
    piles <- list()
    tops <- c()

    for (k in deck){
        if (sum(tops > k) == 0){
            piles <- append(piles, k)
            tops <- c(tops,k)
        }else{
            for (i in order(tops)){
                if (tops[i] > k){
                    tops[i] <- k
                    piles[[i]] <- c(piles[[i]],k)
                    break
                }
            }
        }
    }
    return(list(deck=deck,piles=piles))
}

showpiles <- function(res){
    barplot(sapply(res$piles, length))
}


