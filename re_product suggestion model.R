


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

# select bunnings to test products at random

item_index_in_test_set <- 233
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
end_time - start_time # 26 minutes per product just on this loop

prod_list_and_terms <- as.data.frame(tdm)

# Rename products and score before cbinding to avoid column name collisions



prod_list_and_terms <- cbind(products, prod_list_and_terms, score)
names(prod_list_and_terms) <- make.names(names(prod_list_and_terms)) # note this won't do anything as make.names won't handle duplicate column names (based on my current experience)
# so we rename the conflicting columns manually 
index_of_score <- which(colnames(prod_list_and_terms) == "score")
index_of_products <- which(colnames(prod_list_and_terms) == "products")
colnames(prod_list_and_terms)[index_of_score[1]] <- "score_string"
colnames(prod_list_and_terms)[index_of_products[2]] <- "products_string"


prod_list_and_terms <- arrange(prod_list_and_terms, desc(score)) 


# Inspect results
top_50_matches <- prod_list_and_terms[1:50,c("products", "score")]
top_50_matches

print(paste("Product: ",bunnings[item_index_in_test_set, "product"]))

# terms used
terms_used <- names(comp_product_tdm[1,comp_product_tdm[1,] != 0])
terms_used


# ~26 minutes per product just for the main loop, time consuming elsewhere too
# unfeasible without modification (~ 45 days for ~ 20k products across 6 comps - would take 7 days on 64 core machine!)








