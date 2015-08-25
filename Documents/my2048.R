
main = matrix(0, 1, 16)
pos1 = sample(1:16, 1)
val1 = 2
pos2 = sample((1:16)[-pos1], 1)
val2 = 2
main[pos1] = val1
main[pos2] = val2
score = 0
high = 2

while (high < 2048){
    dim(main) = c(4, 4)
    print(main)
    input = readline(prompt = "A?W?S?D?Q? ")
    if (input == "A"){
        effective_move = 0
        for (i in 1:4){
            vec = main[i, main[i,] != 0]
            nelem = length(vec)
            
            if (nelem == 0){
                next
            } else if (all(main[i, 1:nelem] == vec)){
                next
            } else if (nelem == 4){
                main[i, 1:nelem] = vec
            } else {
                main[i, 1:nelem] = vec
                main[i, (nelem+1):4] = 0
                effective_move = effective_move + 1
            }
        }
        for (ii in 1:4){
            for (jj in 2:4){
                if (main[ii, jj] == 0){
                    next
                } else if (main[ii, jj] == main[ii, jj-1]){
                    if (jj >= 4){
                        main[ii, jj-1] = main[ii, jj-1] * 2
                        main[ii, 4] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii, jj-1]
                    } else{
                        shift = main[ii, min(jj+1, 4):4]
                        main[ii, jj-1] = main[ii, jj-1] * 2
                        main[ii, jj:3] = shift
                        main[ii, 4] = 0 
                        effective_move = effective_move + 1
                        score = score + main[ii, jj-1]
                    }
                }
            }
        }
    } else if (input == "W"){
        effective_move = 0
        for (i in 1:4){
            vec = main[main[, i] != 0, i]
            nelem = length(vec)
            
            if (nelem == 0){
                next
            } else if (all(main[1:nelem, i] == vec)){
                next
            } else if (nelem == 4){
                main[1:nelem, i] = vec
            } else {
                main[1:nelem, i] = vec
                main[(nelem+1):4, i] = 0
                effective_move = effective_move + 1
            }
        }
        for (ii in 2:4){
            for (jj in 1:4){
                if (main[ii, jj] == 0){
                    next
                } else if (main[ii, jj] == main[ii-1, jj]){
                    if (ii >= 4){
                        main[ii-1, jj] = main[ii-1, jj] * 2
                        main[4, jj] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii-1, jj]
                    } else {
                        shift = main[min(ii+1, 4):4, jj]
                        main[ii-1, jj] = main[ii-1, jj] * 2
                        main[ii:3, jj] = shift
                        main[4, jj] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii-1, jj]
                    }
                }
            }
        }
    } else if (input == "S"){
        effective_move = 0
        for (i in 1:4){
            vec = main[main[, i]!= 0, i]
            nelem = length(vec)
            
            if (nelem == 0){
                next
            } else if (all(main[(5-nelem):4, i] == vec)){
                next
            } else if (nelem == 4){
                main[(5-nelem):4, i] = vec
            } else {
                main[(5-nelem):4, i] = vec
                main[1:(4-nelem), i] = 0
                effective_move = effective_move + 1
            }
        }
        for (ii in 3:1){
            for (jj in 1:4){
                if (main[ii, jj] == 0){
                    next
                } else if (main[ii+1, jj] == main[ii, jj]){
                    if (ii <= 1){
                        main[ii+1, jj] = main[ii, jj] * 2
                        main[1, jj] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii+1, jj]
                    } else {
                        shift = main[1:min(ii-1, 4), jj]
                        main[ii+1, jj] = main[ii, jj] * 2
                        main[2:ii, jj] = shift
                        main[1, jj] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii+1, jj]
                    }
                }
            }
        }
    } else if (input == "D"){
        effective_move = 0
        for (i in 1:4){
            vec = main[i, main[i, ]!= 0]
            nelem = length(vec)
            
            if (nelem == 0){
                next
            } else if (all(main[i, (5-nelem):4] == vec)){
                next
            } else if (nelem == 4){
                main[i, (5-nelem):4] = vec
            } else {
                main[i, (5-nelem):4] = vec
                main[i, 1:(4-nelem)] = 0
                effective_move = effective_move + 1
            }
        }
        for (ii in 1:4){
            for (jj in 3:1){
                if (main[ii, jj] == 0){
                    next
                } else if (main[ii, jj+1] == main[ii, jj]){
                    if (jj <= 1){
                        main[ii, jj+1] = main[ii, jj] * 2
                        main[ii, 1] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii, jj+1]
                    } else {
                        shift = main[ii, 1:min(jj-1, 4)]
                        main[ii, jj+1] = main[ii, jj] * 2
                        main[ii, 2:jj] = shift
                        main[ii, 1] = 0
                        effective_move = effective_move + 1
                        score = score + main[ii, jj+1]
                    }
                }
            }
        }
    } else if (input == "Q"){
        print("The End!")
        print(paste("Your achieved largest number is", high))
        print(paste("Your score is", score))
        break
    } else{
        print("INVALID MOVE!!")
        print("Please enter one of the following:")
        print("A: swipe left")
        print("D: swipe right")
        print("W: swipe up")
        print("S: swipe down")
        print("Q: exit game")
        next
    }
    
    if (effective_move < 1){
        next
    } else {
        dim(main) = c(1, 16)
        posi = which(main != 0) 
        if (length(posi) ==15){
            posj = (1:16)[-posi]
        } else {
            posj = sample((1:16)[-posi], 1)
        }
        vali = (rbinom(1, 1, 0.25)+1) * 2
        main[posj] = vali
    }
    
    high = max(main)
    print("Slide and combine tiles, to make a 2048!")
    print(paste("Current score:", score))
}



