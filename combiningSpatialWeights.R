# spdep's original "union.nb" code
union.nb <- function (nb.obj1, nb.obj2) 
{
     if (!inherits(nb.obj1, "nb") | !inherits(nb.obj2, "nb")) {
          stop("Both arguments must be of class nb")
     }
     if (any(attr(nb.obj1, "region.id") != attr(nb.obj2, "region.id"))) {
          stop("Both neighbor objects must be \n generated from the same coordinates")
     }
     n <- length(nb.obj1)
     if (n != length(nb.obj2)) 
          stop("Both arguments must be of same length")
     if (n < 1) 
          stop("non-positive number of entities")
     card1 <- card(nb.obj1)
     card2 <- card(nb.obj2)
     new.nb <- vector(mode = "list", length = n)
     for (i in 1:n) {
          if (card1[i] == 0) {
               if (card2[i] == 0) 
                    new.nb[[i]] <- 0L
               else new.nb[[i]] <- nb.obj2[[i]]
          }
          else {
               if (card2[i] == 0) 
                    new.nb[[i]] <- nb.obj1[[i]]
               else new.nb[[i]] <- sort(union(nb.obj1[[i]], nb.obj2[[i]]))
          }
     }
     attr(new.nb, "region.id") <- attr(nb.obj1, "region.id")
     attr(new.nb, "type") <- paste("union(", attr(nb.obj1, "type"), 
                                   ",", attr(nb.obj2, "type"), ")")
     class(new.nb) <- "nb"
     new.nb
}

union.nb(nb.obj1, nb.obj2) # fails



# this custom function will let us combine nbs of different types
custom_union.nb <- function (nb.obj1, nb.obj2) 
{
     if (!inherits(nb.obj1, "nb") | !inherits(nb.obj2, "nb")) {
          stop("Both arguments must be of class nb")
     }
     #if (any(attr(nb.obj1, "region.id") != attr(nb.obj2, "region.id"))) {
     #     stop("Both neighbor objects must be \n generated from the same coordinates")
     #}
     #n <- length(nb.obj1)
     #if (n != length(nb.obj2)) 
     #     stop("Both arguments must be of same length")
     #if (n < 1) 
     #     stop("non-positive number of entities")
     
     # generating row numbers
     length_1 <- length(nb.obj1); idx_1 <- 1:length_1
     length_2 <- length(nb.obj2); idx_2 <- 1:length_2
     
     # associating row numbers with region.id (renamed "global_id",
     # which is the row number of the original object before subsetting)
     df1 <- data.frame(rowNum = idx_1, global_id = attr(nb.obj1, "region.id"))
     df2 <- data.frame(rowNum = idx_2, global_id = attr(nb.obj2, "region.id"))
     
     # test for overlap between global_id's
     combined_df <- data.frame(global_id = unique(union(df1$global_id, df2$global_id)))
     combined_df <- merge(combined_df, df1, all.x = TRUE, by='global_id')
     combined_df <- merge(combined_df, df2, all.x = TRUE, by='global_id')
     names(combined_df)[2:3] <- c('idx_1', 'idx_2')
     
     # length of iterator for loop below
     n <- nrow(combined_df)
     new.nb <- vector(mode = "list", length = n)
     for (i in 1:n) {
          
          # case 1: idx_1 is not null (nb.obj1 contains relevant neighbors),
          # idx_2 does not
          if !is.na(combined_df[i, 2])
          {
               # sel_feature is a selected spatial feature
               sel_feature <- combined_df[i, 2]
               
               # temporary nb object
               temp.nb1 <- nb.obj1[[sel_feature]]
               
               # use the look-up to select the row's global ID
               select_row <- which(combined_df[, 2] == temp.nb1)
               
               
               new.nb[[i]] <- select_row
          }
          
          # case 2: idx_2 is not null (nb.obj2 contains relevant neighbors),
          # idx_1 does not
          if !is.na(combined_df[i, 3])
          {
               # sel_feature is a selected spatial feature
               sel_feature <- combined_df[i, 3]
               
               # temporary nb object
               temp.nb2 <- nb.obj2[[sel_feature]]
               
               # use the look-up to select the row
               select_row <- which(combined_df[, 3] == temp.nb2)
               
               
               new.nb[[i]] <- select_row
               
          }
          
          # case 3:  both nb's have neighs
          # this is designed to handle cases of overlapping nb lists
          # same object that *might* have different neighbors
          if !is.na(combined_df[i, 2] & !is.na(combined_df[i, 3])
          {
               sel_feature <- as.integer(combined_df[i, 2:3])
               
               # we are extracting the number of neighbors per selelcted feature
               # they are the same selected feature, but they *may* have different neighbors
               # if they have a different number of neighbors, then they do not have the same neighbors exactly
               
               card1 <- length(nb.obj1[sel_feature[1]]) # these are the same spatial features, they *may* have different neighbors though
               card2 <- length(nb.obj1[sel_feature[2]]) # these are the same spatial features, they *may* have different neighbors though
               
               # compare nb.objx by the cardinalities, and combine
               if (card1 == 0 & card2 == 0){      # both empty
                    new.nb[[i]] <- 0L             # therefore zero
               }
               if (card1 == 0 & card2 > 0){
                    
                    # grab the neighbors from the second object
                    temp.nb2 <- nb.obj2[[sel_feature[2]]]
                    
                    # use the look-up to select the row
                    select_row <- which(combined_df[, 3] == temp.nb2)
                    
                    # push the canges
                    new.nb[[i]] <- select_row

               }
               if (card1 >0 & card2 == 0){
                    
                    # grab the neighbors from the second object
                    temp.nb1 <- nb.obj1[[sel_feature[1]]]
                    
                    # use the look-up to select the row
                    select_row <- which(combined_df[, 2] == temp.nb1)
                    
                    # push the canges
                    new.nb[[i]] <- select_row
                    
               }
               if (card1 >0 & card2 >0){
                    
                    # sel_feature is a selected spatial feature
                    sel_feature1 <- combined_df[i, 2]
                    sel_feature2 <- combined_df[i, 3]
                    
                    # temporary nb object
                    temp.nb1 <- nb.obj1[[sel_feature1[1]]]
                    temp.nb2 <- nb.obj2[[sel_feature2[2]]]
                    
                    # use the look-up to select the row's global ID
                    select_row1 <- which(combined_df[, 2] == temp.nb1)
                    select_row2 <- which(combined_df[, 3] == temp.nb2)
                    
                    combined_select_row <- sort(union(select_row1, select_row2))
                    
                    new.nb[[i]] <- combined_select_row
               }
               
     attr(new.nb, "region.id") <- combined_df$global_id
     attr(new.nb, "type") <- paste("union(", attr(nb.obj1, "type"), 
                                   ",", attr(nb.obj2, "type"), ")")
     class(new.nb) <- "nb"
     new.nb
     return(new.nb)
}

new_custom_nb <- custom_union.nb(nb.obj1, nb.obj2)

listw <- nb2listw(new_custom_nb)



# make a fake overlapping set
combined_df[c(1:6),2] <- c(27, 333, 334, 336, 335, 339)
head(combined_df)
