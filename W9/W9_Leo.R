ddsf <- data_df[c(3,19104), ]
ddsf
head(ddsf[1,])

def om_condition(x){
    if sum
}

?read.csv()
row.names()	

b1 <- c(ac1 = 1, ac2 = 4, ac3 = 2)
b2 <- c(ac1 = 2, ac2 = 2, ac3 = 1)
b3 <- c(ac1 = 4, ac2 = 4, ac3 = 2)
b4 <- c(ac1 = 3, ac2 = 0, ac3 = 0)
ac_bundles <- cbind(b1, b2, b3, b4)

class(mommy_day_valid)
as.matrix(mommy_day_valid)
as.matrix(mommy_day_valid)


cos_all <- cosine(as.matrix(data_df)); cos_all

view(cos_all)


#Fake data
myData = data.frame(x = 1:100, y = runif(100))
rownames(myData) = paste0("row", 1:nrow(myData))
myData

#Filter
myData %>% rownames_to_column("rowNames") %>% 
    arrange(desc(y)) %>% slice(1:10) %>% pull(rowNames)
?pull
class(cos_all)
colnames(cos_all)

cos_all_df <- as.data.frame(cos_all)
def find_top6_per_column(dataframe, col_){
    dataframe %>% rownames_to_column("rowNames") %>% top_n(6, col_) %>% pull(rowNames)
}

yt_ <- cos_all_df %>% rownames_to_column("IndexName"); view(yt_)
tt <- yt_ %>% top_n('between', n =6) %>%pull(IndexName)

#--------------test
b1 <- c(ac1 = 1, ac2 = 4, ac3 = 2, ac4 = 7 , ac5 = 4 , ac6 = 1)
b2 <- c(ac1 = 2, ac2 = 2, ac3 = 1, ac4 = 5 , ac5 =5 , ac6 = 1)
b3 <- c(ac1 = 4, ac2 = 4, ac3 = 2, ac4 =2,  ac5 = 6, ac6 = 11)
b4 <- c(ac1 = 3, ac2 = 0, ac3 = 0, ac4 =4,  ac5 = 9, ac6 = 1)
b5 <- c(ac1 = 7, ac2 = 6, ac3 = 5, ac4 =4,  ac5 = 5, ac6 = 1)
b6 <- c(ac1 = 3, ac2 = 6, ac3 =6, ac4 =6,  ac5 = 5, ac6 = 1)

ac_bundles <- cbind(b1, b2, b3, b4, b5, b6)
cos_df <-  as.data.frame(cosine(ac_bundles)); cos_df
ac_bundles_df <- cos_df %>% rownames_to_column("IndexName")
ac_bundles_df


#---------------

# new_df <- data.frame(matrix(ncol = dim(ac_bundles_df)[2] -1, nrow = 6))
# for (i in c(2:dim(ac_bundles_df))){
#     print(i)
#     new_df[i-1] <- ac_bundles_df %>%
#         arrange(ac_bundles_df[i]) %>%
#         slice(dim(ac_bundles_df)[1]-k:dim(ac_bundles_df)[1]) %>%
#         pull(colnames(ac_bundles_df)[1])
#     print(new_df[i] )
# }
# new_df
#---------------


cos_all_df <- as.data.frame(cos_all) %>% rownames_to_column("IndexName") 
view(cos_all_df)
ac_bundles_df <- cos_all_df
new_df <- data.frame(matrix(ncol = dim(ac_bundles_df)[2] -1, nrow = 6))
view(new_df)

# 
# for (i in c(2:dim(ac_bundles_df))){
#     print(i)
#     k = 6
#     new_df[i-1] <- ac_bundles_df %>%
#         arrange(ac_bundles_df[i]) %>%
#         slice(dim(ac_bundles_df)[1]-k:dim(ac_bundles_df)[1]) %>%
#         pull(colnames(ac_bundles_df)[1])
#     print(new_df[i])
# }


# here!!!!!

cos_all_df <- as.data.frame(cos_all) %>% rownames_to_column("IndexName") 
view(cos_all_df)
ac_bundles_df <- cos_all_df
new_df <- data.frame(matrix(ncol = dim(ac_bundles_df)[2] -1, nrow = 6))
view(new_df)


dim(ac_bundles_df)
for (i in c(2:dim(ac_bundles_df)[2])){
    print(i)
    tt <- arrange(ac_bundles_df, desc(ac_bundles_df[i]))
    tt2 <- tt %>% slice_head(n = 6)%>% pull(colnames(ac_bundles_df)[1])
    new_df[i-1] <- tt2
    print(tt2)
}



nx <- colnames(ac_bundles_df)
colnames(new_df) <- colnames(ac_bundles_df)[2:length(nx)]
rownames(new_df) <- c('1st', '2nd', '3rd', '4th', '5th', '6th') 
view(new_df)
#---------------

new_df <- data.frame(matrix(ncol = length(ac_bundles_df) -1 , nrow = 6))
new_df[1] <- 

colnames_list <- colnames(ac_bundles_df)[-1]; colnames_list
length(ac_bundles_df)

for (x in 2:length(ac_bundles_df)) {
    
    print(x)
}
dim(new_df)


dim(ac_bundles_df)
def get_recom(x){
    arrange(desc(x)) %>% slice(1:6) %>% 
        pull(colnames(ac_bundles_df)[1])}

ac_bundles_df

sapply(ac_bundles_df,get_recom())
ac_bundles_df
view(yt_)
class(cos_all)
view(cos_all)

magi <- function(x, k){
    ac_bundles_df <- as.data.frame(x) %>% rownames_to_column("IndexName");
    bundle_row_num <- dim(ac_bundles_df)[1];
    bundle_col_num <- dim(ac_bundles_df)[2];
    new_df <- data.frame(matrix(ncol = bundle_col_num -1, nrow = k))
    
    # Using loop for vector based sorting (can not apply sapply() in this case)
    for (i in c(2:bundle_col_num)){
        new_df[i-1] <- ac_bundles_df %>% 
            arrange( desc(ac_bundles_df[i]))%>%
            slice_head(n = k+1)%>% 
            pull(colnames(bundle_row_num))
        
    }
    # # Get the colnames & rownames for new dataframe
    # nx <- colnames(ac_bundles_df);
    # colnames(new_df) <- colnames(ac_bundles_df)[2:length(nx)];
    # rownames(new_df) <- c('itself', '1st', '2nd', '3rd', '4th', '5th')
    # 
    # return(new_df) 
}

k=5

cos_all <- cosine(as.matrix(data_df))
# transfer matrix coss_all into df then add a added column as "IndexName"
ac_bundles_df <- as.data.frame(cos_all) %>% rownames_to_column("IndexName") 
bundle_row_num <- dim(ac_bundles_df)[1]
bundle_col_num <- dim(ac_bundles_df)[2]
    

#-------------

cos_all <- cosine(as.matrix(data_df))


jk<-function(cos_all){
    # transfer matrix coss_all into df then add a added column as "IndexName"
    ac_bundles_df <- as.data.frame(cos_all) %>% rownames_to_column("IndexName") 
    
    # Build a empty dataframe with size 6*165
    new_df <- data.frame(matrix(ncol = dim(ac_bundles_df)[2] -1, nrow = 6))
    
    # Using loop for vector based sorting (can not apply sapply() in this case)
    for (i in c(2:dim(ac_bundles_df)[2])){
        new_df[i-1] <- ac_bundles_df %>% 
            arrange( desc(ac_bundles_df[i]))%>%
            slice_head(n = 6)%>% 
            pull(colnames(ac_bundles_df)[1])
    }
    # Get the colnames & rownames for new dataframe
    nx <- colnames(ac_bundles_df)
    colnames(new_df) <- colnames(ac_bundles_df)[2:length(nx)]
    rownames(new_df) <- c('itself', '1st', '2nd', '3rd', '4th', '5th')
    return (new_df)
}

data_matrix<-as.matrix(data_df)
# based on row


means <- apply(data_matrix, 2, mean) 
means <- apply(data_matrix, 1, mean) 

# Deduction the mean
dim(data_matrix)
dim(data_matrix)
length(means)
?replicate()
bundle_means_matrix <- replicate(ncol(data_matrix), means);
dim(bundle_means_matrix)
col_normalized_data_df <- data_matrix - bundle_means_matrix

cosine_single( name ="sweetmothersday",df = col_normalized_data_df, top_n =5 )
pts

summary( lm( pts$y ~ pts$x))


pts <- interactive_regression()
class(pts)


mean_list <- sapply(pts,mean)

mean_list[1]


temp =pts
temp$x <-( pts$x - mean_list[1])/sd(pts$x)
temp$y <- (pts$y - mean_list[2])/sd(pts$y)
temp

summary(lm( pts$y ~ pts$x))
cor(pts)
