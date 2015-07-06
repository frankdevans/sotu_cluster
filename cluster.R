library(dplyr)
library(magrittr)
library(tm)
library(proxy)
library(ggplot2)
library(RColorBrewer)


load(file = './data/sotu_df.RData')

# TM EDA
sotu_corpus <- Corpus(VectorSource(sotu$content)) %>%
    tm_map(x = ., FUN = PlainTextDocument) %>%
    tm_map(x = ., FUN = removePunctuation) %>%
    tm_map(x = ., FUN = removeNumbers) %>%
    tm_map(x = ., FUN = removeWords, stopwords(kind = 'en')) %>%
    tm_map(x = ., FUN = stripWhitespace)


doc_term <- DocumentTermMatrix(sotu_corpus)
doc_term$dimnames$Docs <- sotu$file_name
findFreqTerms(x = doc_term, lowfreq = 500)

# Cluster: Hierarchical
tf_idf <- weightTfIdf(m = doc_term, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf)
tf_idf_dist <- dist(tf_idf_mat, method = 'cosine')

clust_h <- hclust(d = tf_idf_dist, method = 'average')
plot(clust_h)

clust_h <- hclust(d = tf_idf_dist, method = 'ward.D2')
plot(clust_h)

clust_h <- hclust(d = tf_idf_dist, method = 'mcquitty')
plot(clust_h)


# Compute Cohesiveness
dist_mat <- as.matrix(tf_idf_dist)

df_clust_cuts <- data_frame(cut_level = 1:length(sotu$file_name),
                            avg_size = 0,
                            avg_dist = 0)

for (i in 1:(nrow(df_clust_cuts) - 1)) {
    df_clust_cuts[df_clust_cuts$cut_level == i, 'avg_size'] <- mean(table(cutree(tree = clust_h, k = i)))
    
    df_dist <- data_frame(doc_name = doc_term$dimnames$Docs,
                          clust_cut = cutree(tree = clust_h, k = i)) %>%
        inner_join(x = ., y = ., by = 'clust_cut') %>%
        filter(doc_name.x != doc_name.y)
    df_dist$cos_dist <- NA
    for (t in 1:nrow(df_dist)) {
        df_dist$cos_dist[t] <- dist_mat[df_dist$doc_name.x[t], df_dist$doc_name.y[t]]
    }
    df_dist <- df_dist %>%
        group_by(clust_cut) %>%
        summarise(cos_dist = mean(cos_dist))
    
    df_clust_cuts[df_clust_cuts$cut_level == i, 'avg_dist'] <- mean(df_dist$cos_dist)
}


# Graph Cohesiveness
ggplot(data = df_clust_cuts, aes(x = cut_level, y = avg_dist)) +
    geom_line()

ggplot(data = df_clust_cuts, aes(x = cut_level, y = avg_size)) +
    geom_line()


ggplot(data = df_clust_cuts, aes(x = avg_size, y = avg_dist)) +
    geom_point()




# Cluster: KMeans
tf_idf_norm <- tf_idf_mat / apply(tf_idf_mat, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
km_clust <- kmeans(tf_idf_norm, 5)
pca_comp <- prcomp(tf_idf_norm)
pca_rep <- data_frame(sotu_name = sotu$file_name,
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(km_clust$cluster))
ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
    #geom_point(size = 4, shape = 16) +
    scale_color_brewer(palette = 'Set1') +
    geom_text(mapping = aes(label = sotu_name), size = 3, fontface = 'bold')





























#