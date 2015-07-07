library(dplyr)
library(magrittr)
library(tm)
library(proxy)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

set.seed(1300)


load(file = './data/sotu_df.RData')

# TM Pipeline
sotu_corpus <- Corpus(VectorSource(sotu$content)) %>%
    tm_map(x = ., FUN = PlainTextDocument) %>%
    tm_map(x = ., FUN = removePunctuation) %>%
    tm_map(x = ., FUN = removeNumbers) %>%
    tm_map(x = ., FUN = removeWords, stopwords(kind = 'en')) %>%
    tm_map(x = ., FUN = stripWhitespace)


doc_term <- DocumentTermMatrix(sotu_corpus)
doc_term$dimnames$Docs <- sotu$file_name
#findFreqTerms(x = doc_term, lowfreq = 500)

tf_idf <- weightTfIdf(m = doc_term, normalize = TRUE)
tf_idf_mat <- as.matrix(tf_idf)
tf_idf_dist <- dist(tf_idf_mat, method = 'cosine')

# Cluster: Hierarchical
clust_h <- hclust(d = tf_idf_dist, method = 'average')
plot(clust_h)

clust_h <- hclust(d = tf_idf_dist, method = 'ward.D2')
plot(clust_h,
    main = 'Cluster Dendrogram: Ward Cosine Distance',
    xlab = '', ylab = '', sub = '')

clust_h <- hclust(d = tf_idf_dist, method = 'mcquitty')
plot(clust_h,
     main = 'Cluster Dendrogram: McQuitty Cosine Distance',
     xlab = '', ylab = '', sub = '')


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
    geom_line(color = 'steelblue', size = 2) +
    labs(title = 'Cosine Distance of Hierarchical Clusters by Tree Cut',
         x = 'Tree Cut Level / Number of Final Clusters',
         y = 'Intra-Cluster Mean Cosine Distance')


ggplot(data = df_clust_cuts, aes(x = avg_size, y = avg_dist, color = cut_level)) +
    geom_point(size = 4) +
    labs(title = 'Mean Cluster Size & Cosine Distance by Tree Cut',
         x = 'Mean Number of Documents per Cluster',
         y = 'Intra-Cluster Mean Cosine Distance')




# Cluster: KMeans
tf_idf_norm <- tf_idf_mat / apply(tf_idf_mat, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
km_clust <- kmeans(x = tf_idf_norm, centers = 5, iter.max = 25)
pca_comp <- prcomp(tf_idf_norm)
pca_rep <- data_frame(sotu_name = sotu$file_name,
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(km_clust$cluster))

ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
    scale_color_brewer(palette = 'Set1') +
    geom_text(mapping = aes(label = sotu_name), size = 3, fontface = 'bold') +
    labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
         x = 'Principal Component Analysis: Factor 1',
         y = 'Principal Component Analysis: Factor 2') +
    theme_grey() +
    theme(legend.position = 'right',
          legend.title = element_blank())



# Examine Wordcloud
term_doc <- TermDocumentMatrix(sotu_corpus)
term_doc$dimnames$Docs <- sotu$file_name
td_mat <- as.matrix(term_doc)
td_mat <- td_mat[!row.names(td_mat) %in% stopwords(kind = 'SMART'),]

commonality.cloud(term.matrix = td_mat)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 3], max.words = 50)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster != 3], max.words = 50)



commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 1], max.words = 50)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 2], max.words = 50)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 3], max.words = 50)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 4], max.words = 50)
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 5], max.words = 50)


td_mat <- td_mat[!row.names(td_mat) %in% c('applause'),]
commonality.cloud(term.matrix = td_mat[,km_clust$cluster == 3], max.words = 50)











#
