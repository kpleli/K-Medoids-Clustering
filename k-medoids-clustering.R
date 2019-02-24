
library(dplyr) # for data cleaning
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
library(psych)

cars = read.csv(file.choose(), sep = "," , header= T)
cars

#convert symboling to factor since it is reading as an integer
cars$symboling = factor(cars$symboling)

glimpse(cars)
describe(cars)

#calculate gower's distance without make variable
gower_dist <- daisy(cars[,-3], metric = "gower")
summary(gower_dist)

# Calculate silhouette width for many k using PAM
sil_width <- c(NA)
for(i in 2:10){
  pam_fit <- pam(gower_dist,
  diss = TRUE,
  k = i)

  sil_width[i] <- pam_fit$silinfo$avg.width
}

# Plot sihouette width to determine appropriate number of clusters
plot(1:10, sil_width,
  xlab = "Number of clusters",
  ylab = "Silhouette Width")
lines(1:10, sil_width)

#use k metoids with 8 clusters
pam_fit <- pam(gower_dist, diss = TRUE, k = 8)

pam_results <- cars %>%
  dplyr::select(-make) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
  pam_results$the_summary

#visualize
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
    make = cars$make)
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))