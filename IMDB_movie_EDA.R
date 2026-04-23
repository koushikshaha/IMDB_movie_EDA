install.packages(c("readr", "dplyr", "ggplot2", "gridExtra","GGally","corrplot", "plotly"))
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(GGally)
library(corrplot)
library(plotly)

FILE_ID = "1mASYC6eyEajnigyR6F52fqd9bhG18BRP"
url <- paste0("https://drive.google.com/uc?export=download&id=", FILE_ID)
movies <- read_csv(url)

movies[!complete.cases(movies),]

movies <- movies %>%
  select(-director_facebook_likes, -actor_3_facebook_likes,
         -actor_1_facebook_likes, -actor_3_name,-actor_2_facebook_likes,
         -cast_total_facebook_likes,-aspect_ratio,
         -movie_facebook_likes , -movie_imdb_link, -actor_2_name, -facenumber_in_poster, -actor_3_name)

movies <- movies %>%
  filter(budget != max(budget, na.rm = TRUE))

sum(is.na(movies$imdb_score))

colSums(is.na(movies))

print(movies$budget)


movies <- movies %>%
  mutate(
    budget_million = budget / 1e6,
    gross_million = gross / 1e6,
    profit_million = (gross - budget) / 1e6,
    main_genre = sapply(strsplit(genres, "\\|"), `[`, 1)
  )


mode_color <- names(which.max(table(movies$color)))
mode_color
movies$color[is.na(movies$color)] <- mode_color

sum(is.na(movies$color))


sum(is.na(movies$duration))
mean_duration <- mean(movies$duration, na.rm = TRUE)
mean_duration

movies$duration[is.na(movies$duration)] <- mean_duration
movies$duration[is.na(movies$duration)] <- round(mean_duration)


mean_num_reviews <- mean(movies$num_user_for_reviews, na.rm = TRUE)
mean_num_reviews
movies$num_user_for_reviews[is.na(movies$num_user_for_reviews)] <- mean_num_reviews


sum(is.na(movies$num_user_for_reviews))  # Should be 0 now
mean_critics <- mean(movies$num_critic_for_reviews, na.rm = TRUE)
mean_critics
movies$num_critic_for_reviews[is.na(movies$num_critic_for_reviews)] <- mean_critics
sum(is.na(movies$num_critic_for_reviews))


movies <- movies %>%
  filter(!is.na(gross), !is.na(budget))

sum(is.na(movies$gross))
sum(is.na(movies$budget))

summary(movies)


ggplot(movies, aes(x = imdb_score)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of IMDb Scores", x = "IMDb Score", y = "Count")


ggplot(movies, aes(x = duration)) +
  geom_histogram(binwidth = 10, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Movie Duration", x = "Duration (minutes)", y = "Count")


ggplot(movies, aes(x = budget_million)) +
  geom_histogram(binwidth = 500, fill = "red", color = "black") +
  labs(title = "Distribution of Movie budgets", x = "budget (USD million)", y = "Count")

ggplot(movies, aes(x = gross_million)) +
  geom_histogram(binwidth = 500, fill = "red", color = "black") +
  labs(title = "Distribution of Movie gross", x = "gross (USD million)", y = "Count")


ggplot(movies, aes(x = profit_million)) +
  geom_histogram(binwidth = 500, fill = "red", color = "black") +
  labs(title = "Distribution of Movie Profits", x = "Profit (USD million)", y = "Count")


movies %>%
  count(color) %>%
  mutate(prop = n / sum(n),
         label = paste0(color, " (", round(prop * 100, 1), "%)")) %>%
  ggplot(aes(x = "", y = prop, fill = color)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Distribution of Movie Colors") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")


movies <- movies %>%
  mutate(
    main_genre = sapply(strsplit(genres, "\\|"), `[`, 1)
  )
genre_counts <- movies %>%
  count(main_genre)
ggplot(genre_counts, aes(x = reorder(main_genre, n), y = n)) +
  geom_col(fill = "orange") +
  coord_flip() +
  labs(
    title = "Number of Movies by Genre",
    x = "Genre",
    y = "Number of Movies"
  )


movies %>%
  count(language) %>%
  slice_max(n, n = 10) %>%   # Keep only top 10 languages
  ggplot(aes(x = reorder(language, n), y = n)) +
  geom_col(fill = "cyan") +
  coord_flip() +
  labs(title = "Top 10 Languages by Number of Movies",
       x = "Language",
       y = "Number of Movies")


movies %>%
  count(country) %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "gold") +
  coord_flip() +
  labs(title = "Top 10 Countries by Movie Production", x = "Country", y = "Number of Movies")


ggplot(movies, aes(x = imdb_score)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(title = "Density Plot of IMDb Scores",
       x = "IMDb Score",
       y = "Density")

ggplot(movies, aes(x = duration)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Density Plot of Movie Duration",
       x = "Duration (minutes)",
       y = "Density")

movies %>%
  filter(title_year >= 1980) %>%
  ggplot(aes(x = title_year)) +
  geom_density(fill = "purple", alpha = 0.6) +
  labs(title = "Density Plot of Movie Release Years (1980–Present)",
       x = "Release Year", y = "Density")


ggplot(movies, aes(x = budget_million, y = gross_million)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Budget vs Gross Revenue", x = "Budget (USD in millions)", y = "Gross Revenue (USD milions)")


ggplot(movies, aes(x = duration, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  labs(title = "Duration vs IMDb Score", x = "Duration (minutes)", y = "IMDb Score")


ggplot(movies, aes(x = budget_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "Budget vs IMDb Score",
       x = "Budget (USD in million)",
       y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = gross_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "Gross Revenue vs IMDb Score",
       x = "Gross Revenue (USD)",
       y = "IMDb Score") +
  theme_minimal()

ggplot(movies, aes(x = profit_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "Profit vs IMDb Score",
       x = "Profit (USD)",
       y = "IMDb Score") +
  theme_minimal()


p1 <- ggplot(movies, aes(x = budget_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "blue") +
  labs(title = "IMDb Score vs Budget",
       x = "Budget (Million USD)",
       y = "IMDb Score") +
  theme_minimal()

p2 <- ggplot(movies, aes(x = gross_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "green") +
  labs(title = "IMDb Score vs Gross",
       x = "Gross Revenue (Million USD)",
       y = "IMDb Score") +
  theme_minimal()

p3 <- ggplot(movies, aes(x = profit_million, y = imdb_score)) +
  geom_point(alpha = 0.5, color = "red") +
  labs(title = "IMDb Score vs Profit",
       x = "Profit (Million USD)",
       y = "IMDb Score") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 3)



ggplot(movies,aes(x = main_genre, y = imdb_score)) +
  geom_boxplot(fill = "pink") +
  coord_flip() +
  labs(title = "IMDb Scores by Genre", x = "Genre", y = "IMDb Score")


ggplot(movies,aes(x = main_genre, y = duration)) +
  geom_boxplot(fill = "violet") +
  coord_flip() +
  labs(title = "Movie Duration by Genre", x = "Genre", y = "Duration (minutes)")


movies %>%
  mutate(main_genre = sapply(strsplit(genres, "\\|"), `[`, 1)) %>%
  count(main_genre) %>%
  top_n(5, n) %>%
  ggplot(aes(x = "", y = n, fill = main_genre)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Proportion of Top 5 Genres") +
  theme_void()

top_countries <- movies %>%
  count(country) %>%
  slice_max(n, n = 10) %>%
  pull(country)

movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = imdb_score)) +
  geom_boxplot(fill = "purple") +
  coord_flip() +
  labs(title = "IMDb Score by Top 10 Countries",
       x = "Country",
       y = "IMDb Score") +
  theme_minimal()

movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = budget_million)) +
  geom_boxplot(fill = "skyblue") +
  coord_flip() +
  labs(title = "Budget by Country",
       x = "Country",
       y = "Budget (Million USD)") +
  theme_minimal()

movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = profit_million)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Profit by Country",
       x = "Country",
       y = "Profit (Million USD)") +
  theme_minimal()

p1 <- movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = profit_million)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Profit by Country",
       x = "Country",
       y = "Profit (Million USD)") +
  theme_minimal()

p2 <- movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = budget_million)) +
  geom_boxplot(fill = "skyblue") +
  coord_flip() +
  labs(title = "Budget by Country",
       x = "Country",
       y = "Budget (Million USD)") +
  theme_minimal()

p3 <- movies %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = country, y = imdb_score)) +
  geom_boxplot(fill = "lightgreen") +
  coord_flip() +
  labs(title = "IMDb Score by Country",
       x = "Country",
       y = "IMDb Score") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)


top_languages <- movies %>%
  count(language) %>%
  slice_max(n, n = 10) %>%
  pull(language)

movies %>%
  filter(language %in% top_languages) %>%
  ggplot(aes(x = language, y = imdb_score)) +
  geom_boxplot(fill = "lightblue") +
  coord_flip() +
  labs(title = "IMDb Score by Top 10 Languages",
       x = "Language",
       y = "IMDb Score") +
  theme_minimal()


top_5_countries <- movies %>%
  count(country) %>%
  slice_max(n, n = 5) %>%
  pull(country)

top_5_languages <- movies %>%
  count(language) %>%
  slice_max(n, n = 5) %>%
  pull(language)

movies %>%
  filter(country %in% top_5_countries) %>%
  count(main_genre, country) %>%
  ggplot(aes(x = main_genre, y = n, fill = country)) +
  geom_col(position = "dodge") +
  labs(title = "Movies by Genre and Country",
       x = "Genre",
       y = "Number of Movies",
       fill = "Country") +
  theme_minimal()


movies %>%
  filter(language %in% top_5_languages) %>%
  count(main_genre, language) %>%
  ggplot(aes(x = main_genre, y = n, fill = language)) +
  geom_col(position = "dodge") +
  labs(title = "Movies by Genre and Language",
       x = "Genre",
       y = "Number of Movies",
       fill = "Language") +
  theme_minimal()

movies %>%
  count(main_genre, color) %>%
  ggplot(aes(x = main_genre, y = n, fill = color)) +
  geom_col(position = "dodge") +
  labs(title = "Movie Color Format by Genre",
       x = "Genre",
       y = "Number of Movies",
       fill = "Color Type") +
  theme_minimal()

ggplot(movies, aes(x = budget_million, y = gross_million, color = main_genre)) +
  geom_point(alpha = 0.6) +
  labs(title = "Budget vs Gross (Colored by Genre)",
       x = "Budget (Million USD)",
       y = "Gross Revenue (Million USD)",
       color = "Genre") +
  theme_minimal()


ggplot(movies, aes(x = duration, y = imdb_score, color = country)) +
  geom_point(alpha = 0.6) +
  labs(title = "Duration vs IMDb Score (Colored by Country)",
       x = "Duration (Minutes)",
       y = "IMDb Score",
       color = "Country") +
  theme_minimal()

ggplot(movies, aes(x = profit_million, y = imdb_score, color = language)) +
  geom_point(alpha = 0.6) +
  labs(title = "Profit vs IMDb Score (Colored by Language)",
       x = "Profit (Million USD)",
       y = "IMDb Score",
       color = "Language") +
  theme_minimal()


yearly_stats <- movies %>%
  group_by(title_year) %>%
  summarise(
    avg_imdb_score = mean(imdb_score, na.rm = TRUE),
    avg_budget_million = mean(budget_million, na.rm = TRUE),
    avg_profit_million = mean(profit_million, na.rm = TRUE),
    movies_count = n()
  ) %>%
  arrange(title_year)

ggplot(yearly_stats, aes(x = title_year, y = avg_imdb_score)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Average IMDb Score per Year", x = "Year", y = "Avg IMDb Score")

ggplot(yearly_stats, aes(x = title_year, y = avg_budget_million)) +
  geom_line(color = "green") +
  geom_point() +
  labs(title = "Average Budget per Year", x = "Year", y = "Avg Budget (Million)")

ggplot(yearly_stats, aes(x = title_year, y = avg_profit_million)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Average Profit per Year", x = "Year", y = "Avg Profit (Million)")

ggplot(yearly_stats, aes(x = title_year, y = movies_count)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Number of Movies Released per Year", x = "Year", y = "Count")



p1 <- ggplot(yearly_stats, aes(x = title_year, y = avg_imdb_score)) +
  geom_line(color = "blue") +
  geom_point() +
  labs(title = "Average IMDb Score per Year", x = "Year", y = "Avg IMDb Score")

p2 <- ggplot(yearly_stats, aes(x = title_year, y = avg_budget_million)) +
  geom_line(color = "green") +
  geom_point() +
  labs(title = "Average Budget per Year", x = "Year", y = "Avg Budget (Million)")

p3 <- ggplot(yearly_stats, aes(x = title_year, y = avg_profit_million)) +
  geom_line(color = "purple") +
  geom_point() +
  labs(title = "Average Profit per Year", x = "Year", y = "Avg Profit (Million)")

p4 <- ggplot(yearly_stats, aes(x = title_year, y = movies_count)) +
  geom_line(color = "red") +
  geom_point() +
  labs(title = "Number of Movies Released per Year", x = "Year", y = "Count")

grid.arrange(p1, p2, p3, p4, ncol = 2)


movies %>%
  arrange(desc(imdb_score)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(movie_title, imdb_score), y = imdb_score)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top 10 IMDb Rated Movies",
       x = "Movie Title", y = "IMDb Score")

top_actors_profit <- movies %>%
  group_by(actor_1_name) %>%
  summarise(avg_profit = mean(profit_million, na.rm = TRUE)) %>%
  arrange(desc(avg_profit)) %>%
  slice_head(n = 10)

ggplot(top_actors_profit, aes(x = reorder(actor_1_name, avg_profit), y = avg_profit)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 10 Actors by Average Profit",
       x = "Actor",
       y = "Average Profit (USD)")



movies$color_num <- as.numeric(as.factor(movies$main_genre))

pairs(
  movies[, c("budget_million", "gross_million", "profit_million", "duration")],
  col = movies$color_num,
  pch = 19,
  main = "Pair Plot of Selected Movie Features by Genre"
)

numeric_movies <- movies %>%
  select(where(is.numeric))

cor_matrix <- cor(numeric_movies, use = "complete.obs")

corrplot(cor_matrix, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # show correlation values
         number.cex = 0.7)

numeric_movies <- movies %>%
  select(where(is.numeric))


cor_matrix <- cor(numeric_movies, use = "complete.obs")
plot_ly(
  x = colnames(cor_matrix),
  y = colnames(cor_matrix),
  z = cor_matrix,
  type = "heatmap",
  colors = colorRamp(c("red", "white", "blue")),
  zmin = -1, zmax = 1
) %>%
  layout(
    title = "Correlation Heatmap of Movies Dataset",
    xaxis = list(title = ""),
    yaxis = list(title = "", autorange = "reversed")
  )



movies$main_genre <- as.factor(movies$main_genre)

ggplot(movies, aes(x = budget_million, 
                   y = profit_million, 
                   color = main_genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # regression lines
  ggtitle("Budget vs Profit by Movie Genre") +
  xlab("Budget (Million USD)") +
  ylab("Profit (Million USD)") +
  theme_minimal()


movies$main_genre <- as.factor(movies$main_genre)

p1 <- ggplot(movies, aes(x = budget_million, 
                         y = gross_million, 
                         color = main_genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Budget vs Gross by Movie Genre") +
  xlab("Budget (Million USD)") +
  ylab("Gross Revenue (Million USD)") +
  theme_minimal()

p2 <- ggplot(movies, aes(x = duration, 
                         y = profit_million, 
                         color = main_genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Duration vs Profit by Movie Genre") +
  xlab("Duration (Minutes)") +
  ylab("Profit (Million USD)") +
  theme_minimal()

p3 <- ggplot(movies, aes(x = imdb_score, 
                         y = profit_million, 
                         color = main_genre)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("IMDB Score vs Profit by Movie Genre") +
  xlab("IMDB Score") +
  ylab("Profit (Million USD)") +
  theme_minimal()

grid.arrange(p1, p2, p3, ncol = 1)


remove_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  x[x < lower_bound | x > upper_bound] <- NA
  return(x)
}

movies$budget_clean <- remove_outliers(movies$budget_million)
movies$gross_clean <- remove_outliers(movies$gross_million)
movies$profit_clean <- remove_outliers(movies$profit_million)
# We also need to clean imdb_score if we compare it
movies$imdb_score_clean <- remove_outliers(movies$imdb_score)


p_bg_before <- ggplot(movies, aes(x = budget_million, y = gross_million)) +
  geom_point(alpha = 0.4, color = "grey30") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Gross (Before Outlier Removal)", x = "Budget", y = "Gross") +
  theme_minimal()

p_bg_after <- ggplot(movies, aes(x = budget_clean, y = gross_clean)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Gross (After Outlier Removal)", x = "Budget (Cleaned)", y = "Gross (Cleaned)") +
  theme_minimal()


p_bp_before <- ggplot(movies, aes(x = budget_million, y = profit_million)) +
  geom_point(alpha = 0.4, color = "grey30") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Profit (Before Outlier Removal)", x = "Budget", y = "Profit") +
  theme_minimal()

p_bp_after <- ggplot(movies, aes(x = budget_clean, y = profit_clean)) +
  geom_point(alpha = 0.4, color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Profit (After Outlier Removal)", x = "Budget (Cleaned)", y = "Profit (Cleaned)") +
  theme_minimal()

p_ip_before <- ggplot(movies, aes(x = imdb_score, y = profit_million)) +
  geom_point(alpha = 0.4, color = "grey30") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Score vs. Profit (Before Outlier Removal)", x = "IMDb Score", y = "Profit") +
  theme_minimal()

p_ip_after <- ggplot(movies, aes(x = imdb_score_clean, y = profit_clean)) +
  geom_point(alpha = 0.4, color = "purple") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "IMDb Score vs. Profit (After Outlier Removal)", x = "IMDb Score (Cleaned)", y = "Profit (Cleaned)") +
  theme_minimal()


p1 <-grid.arrange(p_bg_before, p_bg_after, ncol = 2)
p2 <-grid.arrange(p_bp_before, p_bp_after, ncol = 2)
p3<-grid.arrange(p_ip_before, p_ip_after, ncol = 2)

grid.arrange(p1, p2, p3, ncol = 1)
