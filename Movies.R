#————————————————————————准备阶段———————————————————————————*/
# 安装包和载入包
install.packages("recommenderlab")
install.packages("reshape")
install.packages("ggplot2")
library(recommenderlab)
library(reshape)
library(ggplot2) 

# 打开数据集
# 第一列为用户ID，第二列电影ID，第三列是评分，第四列是用户评分的时间。                    
mydata <- read.table("C:\\Users\\UserName\\Desktop\\ml-100k\\u.data", header = FALSE, stringsAsFactors = TRUE)
mydata <- mydata[, -4]  # 删去第四列

#————————————————————————数据可视化————————————————————————————*/
# 画出饼图展现评分的分布特点
ggplot(mydata, aes(x = factor(1), fill = factor(V3))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  ggtitle("评分分布图") +
  labs(x = "", y = "") +
  guides(fill = guide_legend(title = '评分分数'))

#————————————————————————数据处理————————————————————————————*/
# 使用reshape包对数据进行处理，生成一个以V1为行，V2为列的矩阵，使用V3进行填充
mydata <- cast(mydata, V1 ~ V2, value = "V3")
mydata <- mydata[, -1]  # 第一列数字为序列，剔除

# 将mydata属性改为数据框，并转换为realRatingMatrix类型
mydata <- as.data.frame(mydata)
mydata <- as(as.matrix(mydata), "realRatingMatrix")
colnames(mydata) <- paste0("movie", 1:1682)

#————————————————————————建立模型————————————————————————————*/
# 定义推荐模型函数
train_and_predict <- function(data, method, user_range, movie_range, n_recommendations = 5) {
  model <- Recommender(data[1:943], method = method)
  
  # 预测1：用户对电影的评分
  predict_ratings <- predict(model, data[user_range], type = "ratings")
  print(as(predict_ratings, "matrix")[1:4, movie_range])
  
  # 预测2：给用户推荐电影
  predict_recommendations <- predict(model, data[10:15], n = n_recommendations)
  print(as(predict_recommendations, "list"))
}

# 基于用户 UBCF
train_and_predict(mydata, "UBCF", 100:110, 1000:1005)
# 基于项目 IBCF
train_and_predict(mydata, "IBCF", 100:110, 905:990)
# 奇异值分解 SVD
train_and_predict(mydata, "SVD", 100:110, 1000:1005)
# 基于流行度 POPULAR
train_and_predict(mydata, "POPULAR", 100:110, 1000:1005)


#————————————————————————电影主题统计信息————————————————————————————*/
# 加载u.data和u.item文件
u_data <- read.table("C:\\Users\\UserName\\Desktop\\ml-100k\\u.data", header = FALSE, sep = "\t", stringsAsFactors = TRUE)
colnames(u_data) <- c("UserID", "MovieID", "Rating", "Timestamp")

u_item <- read.table("C:\\Users\\UserName\\Desktop\\ml-100k\\u.item", header = FALSE, sep = "|", quote = "", stringsAsFactors = TRUE)
colnames(u_item) <- c("MovieID", "MovieTitle", "ReleaseDate", "VideoReleaseDate", "IMDbURL", 
                      "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", 
                      "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", 
                      "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")

# 合并u_data和u_item，基于MovieID
merged_data <- merge(u_data, u_item, by = "MovieID")

# 确保genre_columns是数值类型
genre_columns <- colnames(u_item)[6:24]
merged_data[genre_columns] <- lapply(merged_data[genre_columns], as.numeric)

# 计算每个用户对每个类型的平均评分
user_genre_ratings <- aggregate(merged_data[, genre_columns], by = list(merged_data$UserID), FUN = mean)

# 设置评分阈值，判断用户是否喜欢某个类型
rating_threshold <- 0.2
user_genre_likes <- user_genre_ratings[, -1] > rating_threshold

#————————————————————————数据可视化————————————————————————————*/
# 计算每个类型的电影的平均评分
genre_means <- colMeans(user_genre_ratings[, -1])
genre_means_df <- data.frame(Genre = names(genre_means), MeanRating = genre_means)

# 绘制条形图
ggplot(genre_means_df, aes(x = reorder(Genre, -MeanRating), y = MeanRating, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(title = "每个类型的电影的平均评分", x = "电影类型", y = "平均评分") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 绘制用户对每个类型的平均评分条形图
user_id <- 88
user_ratings <- user_genre_ratings[user_genre_ratings$Group.1 == user_id, -1]
user_ratings_long <- data.frame(Genre = colnames(user_ratings), Rating = as.numeric(user_ratings))

ggplot(user_ratings_long, aes(x = reorder(Genre, -Rating), y = Rating, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(
    title = paste("用户", user_id, "对每个类型的电影的平均评分"),
    x = "电影类型",
    y = "平均评分"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#————————————————————————数据分析————————————————————————————*/
# 统计每个类型的喜爱用户数
genre_likes_count <- colSums(user_genre_likes, na.rm = TRUE)
print(genre_likes_count)

# 找到指定用户最喜欢的三个类型
user_ratings_sorted <- user_ratings_long[order(-user_ratings_long$Rating), ]
top_3_genres <- head(user_ratings_sorted, 3)
print(top_3_genres)

# 找到用户未评分过的电影
rated_movies <- u_data[u_data$UserID == user_id, "MovieID"]
unrated_movies <- u_item[!u_item$MovieID %in% rated_movies, ]

# 筛选出这些类型中的电影
top_3_genre_names <- top_3_genres$Genre
unrated_movies_numeric <- unrated_movies[, !colnames(unrated_movies) %in% c("MovieID", "Title")]
recommended_movies <- unrated_movies[rowSums(unrated_movies_numeric[, top_3_genre_names]) > 0, ]
print(recommended_movies)

#————————————————————————建立模型———————————————————————————*/
# 基于评分推荐
movie_ratings <- aggregate(Rating ~ MovieID, data = u_data, FUN = mean, na.rm = TRUE)
recommended_movies_with_ratings <- merge(recommended_movies, movie_ratings, by = "MovieID")
recommended_movies_sorted <- recommended_movies_with_ratings[order(-recommended_movies_with_ratings$Rating), ]
recommended_movies_sorted$Genres <- apply(recommended_movies_sorted[, genre_columns], 1, function(row) {
  paste(names(row[row == 1]), collapse = ", ")
})
top_5_recommendations <- head(recommended_movies_sorted[, c("MovieID", "MovieTitle", "Rating", "Genres")], 5)
print("基于评分推荐的前五名电影：")
print(top_5_recommendations)

# 基于流行度推荐
movie_popularity <- table(u_data$MovieID)
movie_popularity <- as.data.frame(movie_popularity)
colnames(movie_popularity) <- c("MovieID", "Popularity")
recommended_movies_with_popularity <- merge(recommended_movies, movie_popularity, by = "MovieID")
recommended_movies_sorted <- recommended_movies_with_popularity[order(-recommended_movies_with_popularity$Popularity), ]
recommended_movies_sorted$Genres <- apply(recommended_movies_sorted[, genre_columns], 1, function(row) {
  paste(names(row[row == 1]), collapse = ", ")
})
top_5_recommendations <- head(recommended_movies_sorted[, c("MovieID", "MovieTitle", "Popularity", "Genres")], 5)
print("基于流行度推荐的前五名电影：")
print(top_5_recommendations)

#————————————————————————流行度———————————————————————————*/
# 计算每个类型的电影被评分的次数
genre_popularity <- colSums(merged_data[, genre_columns])
genre_popularity_df <- data.frame(Genre = names(genre_popularity), Popularity = genre_popularity)
genre_popularity_df <- genre_popularity_df[order(-genre_popularity_df$Popularity), ]

# 绘制条形图
ggplot(genre_popularity_df, aes(x = reorder(Genre, -Popularity), y = Popularity, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(title = "每个类型电影的流行度", x = "电影类型", y = "被评分次数") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 指定用户ID
user_data <- merged_data[merged_data$UserID == user_id, ]
user_genre_popularity <- colSums(user_data[, genre_columns])
user_genre_popularity_df <- data.frame(Genre = names(user_genre_popularity), Popularity = user_genre_popularity)
user_genre_popularity_df <- user_genre_popularity_df[order(-user_genre_popularity_df$Popularity), ]

# 绘制条形图
ggplot(user_genre_popularity_df, aes(x = reorder(Genre, -Popularity), y = Popularity, fill = Genre)) +
  geom_bar(stat = "identity") +
  labs(title = paste("用户", user_id, "对各个类型电影的流行度"), x = "电影类型", y = "评分次数") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))