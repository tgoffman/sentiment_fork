classify_polarity <- function(textColumns, originalTextColumns, algorithm="bayes",pstrong=0.5,pweak=1.0,prior=1.0,verbose=FALSE,...) {
	matrix <- create_matrix(textColumns,...)
	lexicon <- read.csv(system.file("data/subjectivity.csv.gz",package="sentiment"),header=FALSE)

	counts <- list(positive=length(which(lexicon[,3]=="positive")),negative=length(which(lexicon[,3]=="negative")),total=nrow(lexicon))

	for (i in 1:nrow(matrix)) {
		if (verbose) print(paste("DOCUMENT",i))
		scores <- list(positive=0,negative=0,words=list(positive=c(), negative=c()))
		doc <- matrix[i,]

		words <- findFreqTerms(doc,lowfreq=1)
		
		for (word in words) {
			index <- pmatch(word,lexicon[,1],nomatch=0)
			if (index > 0) {
				entry <- lexicon[index,]
				
				polarity <- as.character(entry[[2]])
				category <- as.character(entry[[3]])
				count <- counts[[category]]
	
				score <- pweak
                if (polarity == "strongsubj") score <- pstrong
				if (algorithm=="bayes") score <- abs(log(score*prior/count))
		
				if (verbose) {
                    print(paste("WORD:",word,"CAT:",category,"POL:",polarity,"SCORE:",score))
				}

                                scores$words[[category]] <- rbind(scores$words[[category]], c(word, score))
				scores[[category]] <- scores[[category]]+score
			}		
		}

		for (key in names(scores)) {
                        if (key == 'words') { break }
                 	if (algorithm=="bayes") {
				count <- counts[[key]]
				total <- counts[["total"]]
				score <- abs(log(count/total))
				scores[[key]] <- scores[[key]]+score

            		} else {
				scores[[key]] <- scores[[key]]+0.000001
			}
		}
		
        best_fit <- names(scores)[which.max(unlist(scores))]


        ratio <- as.integer(abs(scores$positive/scores$negative))
        words <- scores$words[[best_fit]]                
        if (ratio==1) best_fit <- "neutral"

        topwords <- c()
        if (best_fit != "neutral" && class(nrow(words)) != "NULL") {
          # Find top 2 most largest values

          words <- rbind(words[rev(order(words[,2])),]) # Order by highest values of polarity
          sentences <- strsplit(originalTextColumns, "\\.")[[1]]

          word <- words[1,1]
          sentenceIndex <- grep(word, sentences, fixed=TRUE)

          sentence <- sentences[sentenceIndex]
          if (length(sentence) > 0) {
            topwords <- rbind(topwords, c(sentence, words[1,2]))
          }

          if (nrow(words) > 1) {
            word <- words[1,2]
            sentenceIndex <- grep(word, sentences, fixed=TRUE)
            sentence <- sentences[sentenceIndex]
            if (length(sentence) > 0) {
              topwords <- rbind(topwords, c(sentences[sentenceIndex], words[2,2]))
            }
          }
        }
        else {
          # TODO: Find largest value from negative and positive
          
        }

	documents <- list(scores$positive,scores$negative,abs(scores$positive/scores$negative),best_fit,topwords)

		if (verbose) {
			print(paste("POS:",scores$positive,"NEG:",scores$negative,"RATIO:",abs(scores$positive/scores$negative)))
			cat("\n")
		}
	}
	
	return(documents)
}
