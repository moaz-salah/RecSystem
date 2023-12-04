if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")   #require returns FALSE and gives a warning if the package does not exist
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)


##MMS: STEP 1:  DATA Wrangling (Already submitted code)
##############################################################################################################################

getwd()

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <-"ml-10M100K.zip"

if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)


ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE), stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")

movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

#Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


#MMS: Code to remove Movies in Test Data that has no match in Train data, then moved these records to Train Data
# Make sure userId and movieId in final hold-out test set are also in edx set  
final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%         
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)


#MMS: I excluded movielens object as I need to do some data analysis on it and then will remove it 
rm(dl, ratings, movies, test_index, temp, removed)
#rm(dl, ratings, movies, test_index, temp, movielens, removed)  #MMS: Cleaning Operation: Remove All Temp Objects

##MMS: STEP 1:  DATA PREPARATION + Prepare Evaluation Method & Reference Model
##############################################################################################################################

##MMS: 1- Split Working Data to Training and Testing Dataset (20-80%)
set.seed (1, sample.kind = "Rounding")

test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
temp <- edx[test_index2,]
work_train_data <- edx[-test_index2,]

#MMS: Cleansing Data Again: make sure no movie in Test data has no match in Training data
work_test_data <- temp %>%
  semi_join(work_train_data, by = "movieId") %>%         
  semi_join(work_train_data, by = "userId")
work_train_data <- rbind(work_train_data,    anti_join(temp, work_test_data))
rm(temp,  test_index2)


##MMS: 2- Prepare Evaluation Function. Used Method is RMSE
Eval_RMSE <- function(true_ratings, predicted_ratings){    sqrt(mean((true_ratings - predicted_ratings)^2))}


##MMS: 3- Prepare Reference Model: THE AVERAGE
mu <- mean(work_train_data$rating, na.rm = TRUE)
mu     #3.512478

Ref_rmse <- Eval_RMSE(work_test_data$rating, mu)
Ref_rmse   # 1.059904   1.059602 
rmse_results <- tibble(method = "Just the average", RMSE = Ref_rmse)



##MMS: STEP 2:  DATA EXPLORATION 
##############################################################################################################################

head(movielens)
str(movielens)

#MMS: General analysis
movielens|> summarize(n_users = n_distinct(userId))  # 69878
movielens|> summarize(n_movies = n_distinct(movieId))  # 10677
movielens|> summarize(min_rating = min(rating))  # .5
movielens|> summarize(max_rating = max(rating))  # 5


#Users Analysis
#Number of Movies rated by a single user..Looking at both ends 
head(movielens |> group_by(userId) %>%
       summarise(n=n()) %>%
       arrange(n))

head(movielens |> group_by(userId) %>%
       summarise(n=n()) %>%
       arrange(desc(n)))

movielens |> group_by(userId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "blue", fill="red") +
  scale_x_log10() + 
  ggtitle("Distribution of Users Rating") +
  xlab("Number of Ratings") +
  ylab("Number of Users") 

#Movies Analysis
#Smallest and largest Number of rates movies got 

head(movielens |> group_by(movieId,title) %>%
       summarise(n=n()) %>%
       arrange(n))

head(movielens |> group_by(movieId,title) %>%
       summarise(n=n()) %>%
       arrange(desc(n)))

movielens |> group_by(movieId) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "red", fill="blue") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies Rating") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") 


#Rating Analysis
if(any( is.na(movielens[,"rating"])) )
  print("Missing Data") 

#What are the distribution of ratings?
movielens |> group_by(rating) %>%
  summarise(n=n()) %>%
  arrange(n) %>%
  ggplot(aes(x=rating,y=n, label=n)) +
  geom_text(hjust=0, vjust=0) +
  geom_line() +
  geom_point() +
  scale_x_log10() + 
  ggtitle("Distribution of Movies Rating") +
  xlab("Ratings") +
  ylab("Number of Ratings") 


#Genres Analysis

#How many Genre per Movie?
tibble(count = str_count(movielens$genres, fixed("|")), genres = movielens$genres) %>% 
  group_by(count) %>%
  summarise(n = n()) %>%
  arrange(-count) %>% 
  ggplot(aes(x=count+1,y=n, label=n)) +
  geom_text(hjust=0, vjust=0) +
  geom_line() +
  geom_point() +
  ggtitle("Distribution of Genres per Movies") +
  xlab("Number of Genres") +
  ylab("Number of Movies") 

# head(movielens |> mutate(xx= str_length(movielens$genres)) %>%
#        group_by(movieId,title) %>%
#        arrange(xx),100) %>% print(n=100)

 
rm(movielens, edx)


  
##MMS: STEP 3:  MODELING 
##############################################################################################################################
##MMS: 3A- First Model: Linear
##########################################
 

#3A.1 Add Movie Effect (Biase)

#fit-lm-m <- lm(rating ~ as.factor(movieId), data = work_train_data)    # Error: cannot allocate vector of size 570.8 Gb

#Alternative way:
y_mat <- select(work_train_data, movieId, userId, rating) |>
  pivot_wider(names_from = movieId, values_from = rating)       #Execution Time : 4-5 min
rnames <- y_mat$userId
y_mat <- as.matrix(y_mat[,-1])  #MMS: From table to Matrix
rownames(y_mat) <- rnames

b_m <- colMeans(y_mat - mu, na.rm = TRUE)            #MMS: Remove the Average 
qplot(b_m, bins = 10, color = I("black"))

fit_lm_m <- data.frame(movieId = as.integer(colnames(y_mat)), mu = mu, b_m = b_m)
Movie_rmse<- left_join(work_test_data, fit_lm_m , by = "movieId") |>   
  mutate(pred_m = mu + b_m) |> 
  summarize(M_rmse = Eval_RMSE(rating, pred_m))

rmse_results<- rmse_results %>% add_row(method = "Considering Movie Biase", RMSE = as.double(Movie_rmse))        # 0.944 
rmse_results

fit_lm_m|> group_by(b_m)%>%
  summarise(n = n()) %>%
  ggplot(aes(x=b_m,)) +
  geom_histogram(color = "red", fill="blue") +
  ggtitle("Distribution of Movies Biase") +
  xlab("Movie Biase") +
  ylab("Number of Movies") 


##################################################
#3A.2 Add User Effect (Biase)
gc()
b_u <- rowMeans(sweep(y_mat - mu, 2, b_m), na.rm = TRUE)     #MMS: Remove the Average and the Movie Biase  
#Many times: Error: cannot allocate vector of size 5.6 Gb

fit_lm_m_u <- data.frame(userId = as.integer(rownames(y_mat)), b_u = b_u)

Movie_User_rmse<- left_join(work_test_data, fit_lm_m, by = "movieId") |> 
  left_join(fit_lm_m_u, by = "userId") |> 
  mutate(pred_m_u = mu + b_m + b_u) |> 
  summarize(M_U_rmse = Eval_RMSE(rating, pred_m_u))

rmse_results<- rmse_results %>% add_row(method = "Considering Movie & User Biase", RMSE = as.double(Movie_User_rmse))        # 0.8862152  
rmse_results

fit_lm_m_u|> group_by(b_u)%>%
  summarise(n = n()) %>%
  ggplot(aes(x=b_u,)) +
  geom_histogram(color = "red", fill="blue") +
  ggtitle("Distribution of Users Biase") +
  xlab("Users Biase") +
  ylab("Number of Movies") 
#(Same result when I calculated RowMeans without removing the mean and the Movie biase)



#Another Method (Less memory)  (FOR USER & MOVIEW BIASE)######################################################
# temp_Work_Data <- work_train_data |> group_by(movieId)  |>  summarize(b_m=mean(rating-mu))
# b_m2<- temp_Work_Data$b_m
# names(b_m2) <-temp_Work_Data$movieId
# #head(b_m2)
# #temp_Work_Data[which(temp_Work_Data$movieId==185),]
# #b_m[c("1")] #b_m2[c("1")]
# 
# work_train_data_Mod <- work_train_data|> left_join(temp_Work_Data, by='movieId')   |>   group_by(userId)  |>  summarize(b_u=mean(rating-mu-b_m))
# b_u2<- work_train_data_Mod$b_u
# 
# work_train_data_Extended <- work_train_data |> left_join(temp_Work_Data, by='movieId') |> left_join(work_train_data_Mod, by='userId')
# Movie_rmse2<- work_train_data_Extended |> summarize(M_rmse = Eval_RMSE(rating, mu+b_u2))
# Movie_rmse3<- work_train_data_Extended |> summarize(M_rmse = Eval_RMSE(rating, mu+b_u2+b_m2))


##################################################
#3A.3 Correct the Movie Biase by adding Penalized Factor

#To select  λ, we use cross validation:
gc()
n <-  colSums(!is.na(y_mat))
lambdas <- seq(0, 10, 0.1)

sums <- colSums(y_mat - mu, na.rm = TRUE)
rmses <- sapply(lambdas, function(lambda){
  b_m <-  sums / (n + lambda)
  fit_lm_m$b_m <- b_m
  left_join(work_test_data, fit_lm_m, by = "movieId") |> mutate(pred = mu + b_m) |> 
    summarize(rmse = RMSE(rating, pred)) |>
    pull(rmse)
})
lambda <- lambdas[which.min(rmses)]
qplot(lambdas, rmses)  
print(lambda)  #1.7


fit_lm_m$b_m_reg <- colSums(y_mat - mu, na.rm = TRUE) / (n + lambda)  #MMS: this is the Regularized Bias of Movies (that count for the number of users who Estimated)  

RegMovie_User_rmse<- left_join(work_test_data, fit_lm_m, by = "movieId") |> 
  left_join(fit_lm_m_u, by = "userId") |> 
  mutate(pred_m_u_Reg = mu + b_m_reg + b_u) |> 
  summarize(M_U__Reg_rmse = Eval_RMSE(rating, pred_m_u_Reg))

rmse_results<- rmse_results %>% add_row(method = "Considering Regularized Movie & User Biase", RMSE = as.double(RegMovie_User_rmse))        # 0.8660715  
rmse_results




##MMS: 3B- SECOND Model: MATRIX FACTORIZATION
##########################################
gc() #Free unused memory
library(recosystem)

#MMS: May be needed to upgrade to R 4.3.2
#------------------------------------------
# if(!require(installr)) {
#   install.packages("installr"); 
#   require(installr)
# } 
# updateR()

set.seed(1, sample.kind = "Rounding")

#MMS: Put data in needed frormat
train_data <-  with(work_train_data, data_memory(user_index = userId, 
                                                 item_index = movieId, 
                                                 rating     = rating))

reco_obj <-  recosystem::Reco()

#MMS: Select the best tuning parameters
opts <- reco_obj$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                          #    lrate = c(0.1, 0.2),
                                          #    costp_l2 = c(0.01, 0.1), 
                                          #    costq_l2 = c(0.01, 0.1),
                                              nthread  = 4, niter = 5))         #MMS; nthread:5, ntier:5 -->  20 min 


# Train the algorithm  
reco_obj$train(train_data, opts = c(opts$min, nthread = 4, niter = 10))   #Try 10;  0.7991247 , try 20:  0.7991247    , 50: mf_rmse 0.8006816


test_data <-  with(work_test_data, data_memory(user_index = userId, 
                                               item_index = movieId, 
                                               rating     = rating))


y_hat_reco <-  reco_obj$predict(test_data, out_memory())

mf_rmse<- Eval_RMSE(work_test_data$rating, y_hat_reco)
rmse_results<- rmse_results %>% add_row(method = "Matrix Factorization", RMSE = as.double(mf_rmse))        # 0.7991247  
rmse_results


#Basic cleansing of data  (Applied on the model that gave best results. No change in RMSE, but makes the results more logical)
#MMS: Any rate less than 0.5 --> 0.5. Anything more than 5 --> 5
y_hat_reco[y_hat_reco<.5]<-.5
y_hat_reco[y_hat_reco>5]<-5



##MMS: 3C- THIRD Model: MATRIX FACTORIZATION ON RESIDUALS    
############################################################

##NOTE: ((I expect I have some issue here but not sure what it may be))
#-----


# compute residuals on train set
gc()
work_train_data2 <- work_train_data %>% 
  left_join(fit_lm_m, by = "movieId") %>%
  left_join(fit_lm_m_u, by = "userId") %>%
  mutate(res = rating - mu - b_u - b_m_reg)


work_test_data2 <- work_test_data %>% 
  left_join(fit_lm_m, by = "movieId") %>%
  left_join(fit_lm_m_u, by = "userId") %>%
  mutate(res = rating - mu - b_u - b_m_reg)


train_data2 <-  with(work_train_data2, data_memory(user_index = userId, 
                                                  item_index = movieId, 
                                                  rating     = res,  index1 = T))

test_data2 <-  with(work_test_data2, data_memory(user_index = userId, 
                                                item_index = movieId, 
                                                rating     = res , index1 = T))

reco_obj2 <-  recosystem::Reco()

# Select the best tuning parameters
opts <- reco_obj2$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                              nthread  = 4, niter = 5))   #MMS; nthread:4, ntier:5 -->  22 min 

# Train the algorithm  
reco_obj2$train(train_data2, opts = c(opts$min, nthread = 4, niter = 10))   #Try 10;  

y_hat_reco_2 <-  reco_obj2$predict(test_data2, out_memory())
y_hat_reco_2 <-  y_hat_reco_2 + mu

mf_rmse_r<- Eval_RMSE(work_test_data2$rating, y_hat_reco_2)

rmse_results<- rmse_results %>% add_row(method = "Matrix Factorization Residual", RMSE = as.double(mf_rmse_r))    
rmse_results




##MMS: STEP 4:  FINAL VALIDATION 
##############################################################################################################################

#MMS: Put in the needed format
Validt_data <-  with(final_holdout_test, data_memory(user_index = userId, 
                                               item_index = movieId, 
                                               rating     = rating))

y_hat_reco_Final <-  reco_obj$predict(Validt_data, out_memory())

y_hat_reco_Final[y_hat_reco_Final<.5]<-.5
y_hat_reco_Final[y_hat_reco_Final>5]<-5

mf_rmse_Final<- Eval_RMSE(final_holdout_test$rating, y_hat_reco_Final)
mf_rmse_Final         #0.7947753

#MMS: Explore 
final_holdout_test_predicted<-final_holdout_test |> mutate (predict= y_hat_reco_Final)
final_holdout_test_predicted |> arrange(desc(rating))|> head(10)
final_holdout_test_predicted |> arrange(rating)|> head(10)

#MMS: Smallest and Largest difference  
final_holdout_test_predicted |> arrange(desc(abs(rating-predict)))|> head(10)
final_holdout_test_predicted |> arrange(abs(rating-predict))|> head(10)




