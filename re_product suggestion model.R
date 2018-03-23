


library(dplyr)
library(tm)
library(caret)
library(stringr)
library(stringi)
library(SnowballC)



# setwd("/Users/stevecondylios/Desktop/rails/re/Product Suggestion Model/")
r <- read.csv("cmp_w_prodno.csv", stringsAsFactors = FALSE)


colnames(r) <- c("prod_number", "product")


# testing for product match suggestion tool
# script continues from 'data cleaning for seeding.R' (r is simply 11230 bunnings products)

products <- r[,"product"]
products <- str_trim(products)
products <- toupper(products)


corpus <- VCorpus(VectorSource(products))
# DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = FALSE, removeNumbers = FALSE))

# stop=FALSE, stem=FALSE, rmNum=FALSE: dry   hose  ryobi vacuum    wet 
# stop=TRUE, stem=FALSE, rmNum=FALSE: 18m   35mm    dry   hose  ryobi vacuum    wet 
# so how about we try to split the 18 and m, and the 35 and mm (because the numbers might tell us something)
# Hooooolllld up - it removes decimal places, so 1.8m becomes 18m (!)

# Inspection of re tdm shows many concatenated dimensions, which could create matches if split and matched
# Therefore, splitting strings when a number is immediately adjacent with an a-z character could result in improved performance

names(tdm) <- make.names(names(tdm))
tdm <- as.matrix(tdm)
# inspect first product
# training_tdm[1,training_tdm[1,] != 0]
# saveRDS(tdm, "tdm.rds")
# saveRDS(corpus, "corpus.rds")
# tdm <- readRDS("tdm.rds")
# corpus <- readRDS("corpus.rds")



# Import bunnings products set
bunnings <- read.csv("all_cats_fresh.csv", stringsAsFactors = FALSE)
bunnings <- bunnings[bunnings$one_if_rel == 1, ]
bunnings$time_of_scrape <- NULL
bunnings$one_if_rel <- NULL
bunnings$ML_one_if_rel <- NULL
rownames(bunnings) <- 1:nrow(bunnings)










#----- Generating ranked matches for *one* comp product -----#

# select bunnings to test products at random

item_index_in_test_set <- 233
comp_prod <- bunnings[,"product"][item_index_in_test_set]
print(paste("Product: ",bunnings[,"product"][item_index_in_test_set]))

comp_product_corpus <- VCorpus(VectorSource(bunnings[,"product"][item_index_in_test_set]))
comp_product_tdm <- DocumentTermMatrix(comp_product_corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = FALSE, removeNumbers = FALSE))
names(comp_product_tdm) <- make.names(names(comp_product_tdm)) # not sure if this is working correctly, maybe it's a very smart function that does more than it appears to do
comp_product_tdm <- as.matrix(comp_product_tdm) # note: the first column is 'Docs', but if we remove this column, we actually remove the next column - very strange, not sure why this happens, in any case don't remove the column!
# test_product_tdm <- test_product_tdm[,2:ncol(test_product_tdm)] 

score <- c()
start_time <- Sys.time()
for (i in 1:nrow(tdm)) {
  
  tdm_terms <- colnames(tdm[,tdm[i,] == 1])
  product_terms <- colnames(comp_product_tdm)
  score[i] <- sum(product_terms  %in% tdm_terms)
  
}
end_time <- Sys.time()
end_time - start_time # 26 minutes per product just on this loop with full tdm, 14 minutes for tdm with only 

prod_list_and_terms <- as.data.frame(tdm)

# Rename products and score before cbinding to avoid column name collisions

colnames(prod_list_and_terms)[which(colnames(prod_list_and_terms) == "score")] <- "score_renamed"
colnames(prod_list_and_terms)[which(colnames(prod_list_and_terms) == "products")] <- "products_renamed"

prod_list_and_terms <- cbind(products, prod_list_and_terms, score)


prod_list_and_terms <- arrange(prod_list_and_terms, desc(score)) 


# Inspect results
top_50_matches <- prod_list_and_terms[1:50,c("products", "score")]
top_50_matches

print(paste("Product: ",bunnings[item_index_in_test_set, "product"]))

# terms used
terms_used <- names(comp_product_tdm[1,comp_product_tdm[1,] != 0])
terms_used


write.table(top_50_matches, "top_50_matches.csv", append=TRUE, sep=",", row.names = FALSE)
write.table(c("","",""), "top_50_matches.csv", append=TRUE, sep=",", row.names = FALSE, col.names = FALSE, na = "")
write.table(comp_prod, "top_50_matches.csv", append=TRUE, sep=",", row.names = FALSE)
write.table(c("","",""), "top_50_matches.csv", append=TRUE, sep=",", row.names = FALSE, col.names = FALSE, na = "")
write.table(terms_used, "top_50_matches.csv", append=TRUE, sep=",", row.names = FALSE)









#----- Inspecting results and working out how to use the tdm given current method applied to it is unfeasibly long in duration/cost -----#

# ~26 minutes per product just for the main loop, time consuming elsewhere too
# unfeasible without modification (~ 45 days for ~ 20k products across 6 comps - would take 7 days on 64 core machine!)
# given this is unfeasable, how about inspecting the tdm and seeing if it can be reduced - we will lose some predictive power but it may enable us to run the model 
# within a feasable timeframe
a <- colSums(tdm)

# total number of words across all ~131k products? 
sum(a) # ~700k

# total number of distinct words across ~131k products
ncol(tdm) # ~52k


table(a) # ~32k words appear only once, ~6k twice, 2.5k thrice, 1.7k four times, 1.1k five times, 
# 355 words appear 10 times, 137 words appear 20 times, 65 appear 30 times, 33 appear 50 times, 14 appear 100 times, 
# the most frequently any word appears is 4578 times



b <- table(a) %>% as.data.frame(.) %>% arrange(., desc(Freq))

# Which words appear more than 300 times? 
tdm[,which(a > 300)] %>% colnames()

# Which words appear more than 1000 times? 
tdm[,which(a > 1000)] %>% colnames()

# Which words appear more than 100 times? 
tdm[,which(a > 100)] %>% colnames() # ~1000 words











#----- Generating ranked matches for *a list* comp products -----#

# Current tdm size: ~55GB

# reduce tdm - reduced size: 1.2GB 
tdm <- tdm[, which(a > 100)]























