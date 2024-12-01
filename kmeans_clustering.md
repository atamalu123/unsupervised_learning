k-means clustering
================

First we’ll run k-means clustering on the `pokemon` dataset from
[Kaggle](https://www.kaggle.com/datasets/abcsds/pokemon?resource=download).

The number of clusters is not known beforehand, so testing different
`center` parameters may be a good idea.

``` r
suppressPackageStartupMessages(library(dplyr))
set.seed(1000)

pokemon <- read.csv('pokemon.csv')

# subset features
df <- pokemon %>% select(HP, Attack, Defense, Sp..Atk, Sp..Def, Speed)

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
  # Fit the model: km.out
  km.out <- kmeans(df, centers = i, nstart = 20, iter.max = 50)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}
```

Now we create a scree plot of these kmeans clusters to determine the
best number of centers

``` r
# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")
```

![](kmeans_clustering_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

We can choose 3 as the number of clusters

``` r
# Build model with k clusters: km.out
km.out <- kmeans(df, centers = 3, nstart = 20, iter.max = 50)

# View the resulting model
km.out
```

    ## K-means clustering with 3 clusters of sizes 355, 175, 270
    ## 
    ## Cluster means:
    ##         HP   Attack   Defense   Sp..Atk  Sp..Def    Speed
    ## 1 54.68732 56.93239  53.64507  52.02254 53.04789 53.58873
    ## 2 79.30857 97.29714 108.93143  66.71429 87.04571 57.29143
    ## 3 81.90370 96.15926  77.65556 104.12222 86.87778 94.71111
    ## 
    ## Clustering vector:
    ##   [1] 1 1 3 3 1 1 3 3 3 1 1 2 3 1 1 1 1 1 1 3 1 1 3 3 1 1 1 3 1 3 1 3 1 2 1 1 2
    ##  [38] 1 1 3 1 3 1 3 1 1 1 3 1 1 3 1 2 1 3 1 1 1 3 1 3 1 3 1 3 1 1 2 1 3 3 3 1 2
    ##  [75] 2 1 1 3 1 3 1 2 2 1 3 1 2 2 1 3 1 1 3 1 2 1 2 1 2 1 3 3 3 2 1 2 1 2 1 3 1
    ## [112] 3 1 2 2 2 1 1 2 1 2 1 2 2 2 1 3 1 2 1 3 3 3 3 3 3 2 2 2 1 2 2 2 1 1 3 3 3
    ## [149] 1 1 2 1 2 3 3 2 3 3 3 1 1 3 3 3 3 3 1 1 2 1 1 3 1 1 2 1 1 1 3 1 1 1 1 3 1
    ## [186] 3 1 1 1 1 1 1 3 1 1 3 3 2 1 1 2 3 1 1 3 1 1 1 1 1 2 3 2 1 2 3 1 1 3 1 2 1
    ## [223] 2 2 2 1 2 1 2 2 2 2 2 1 1 2 1 2 1 2 1 1 3 1 3 2 1 3 3 3 1 2 3 3 1 1 2 1 1
    ## [260] 1 2 3 3 3 2 1 1 2 2 3 3 3 1 1 3 3 1 1 3 3 1 1 2 2 1 1 1 1 1 1 1 1 1 1 1 3
    ## [297] 1 1 3 1 1 1 2 1 1 3 3 1 1 1 2 1 1 3 1 3 1 1 1 3 1 2 1 2 1 1 1 2 1 2 1 2 2
    ## [334] 2 1 1 3 1 3 3 1 1 1 1 1 1 2 1 3 3 1 3 1 3 2 2 1 3 1 1 1 3 1 3 1 2 3 3 3 3
    ## [371] 2 1 2 1 2 1 2 1 2 1 2 1 3 1 2 1 3 3 1 2 2 1 3 3 1 1 3 3 1 1 3 1 2 2 2 1 1
    ## [408] 2 3 3 1 2 2 3 2 2 2 3 3 3 3 3 3 2 3 3 3 3 3 3 2 3 1 2 2 1 1 3 1 1 3 1 1 3
    ## [445] 1 1 1 1 1 1 3 1 3 1 2 1 2 1 2 2 2 3 1 2 1 1 3 1 3 1 2 3 1 3 1 3 3 3 3 1 3
    ## [482] 1 1 3 1 2 1 1 1 1 2 1 1 3 3 1 1 3 3 1 2 1 2 1 3 2 1 3 1 1 3 2 3 3 2 2 2 3
    ## [519] 3 3 3 2 3 2 3 3 3 3 2 2 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 1
    ## [556] 1 3 1 1 3 1 1 3 1 1 1 1 2 1 3 1 3 1 3 1 3 1 2 1 1 3 1 3 1 2 2 1 3 1 3 2 2
    ## [593] 1 2 2 1 1 3 2 2 1 1 3 1 1 3 1 3 1 3 3 1 1 3 1 2 3 3 1 2 1 2 3 1 2 1 2 1 3
    ## [630] 1 2 1 3 1 3 1 1 2 1 1 3 1 3 1 1 3 1 3 3 1 2 1 2 1 3 2 1 3 1 2 1 2 2 1 1 3
    ## [667] 1 3 1 1 3 1 2 2 1 2 3 1 3 2 1 3 2 1 2 1 2 2 1 2 1 2 3 2 1 1 3 1 3 3 3 3 3
    ## [704] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 2 2 1 1 3 1 1 3 1 1 1 1 3 1 1 1 1 3 1 1 3
    ## [741] 1 3 1 2 3 1 3 3 1 2 3 2 1 2 1 3 1 2 1 2 1 2 1 3 1 3 1 2 1 3 3 3 3 2 1 3 3
    ## [778] 2 1 2 1 1 1 1 2 2 2 2 1 2 1 3 3 3 2 2 3 3 3 3
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1]  812079.9  709020.5 1018348.0
    ##  (between_SS / total_SS =  40.8 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

Now we can examine cluster characteristics

``` r
# create column for clusters
df$cluster <- km.out$cluster

# summarize data by group
df.summ <- df %>%
  group_by(cluster) %>%
  summarize(mean.HP = mean(HP),
            mean.Attack = mean(Attack),
            mean.Defense = mean(Defense),
            mean.Sp.Atk = mean(Sp..Atk),
            mean.Sp.Def = mean(Sp..Def),
            mean.Speed = mean(Speed))

print(df.summ)
```

    ## # A tibble: 3 × 7
    ##   cluster mean.HP mean.Attack mean.Defense mean.Sp.Atk mean.Sp.Def mean.Speed
    ##     <int>   <dbl>       <dbl>        <dbl>       <dbl>       <dbl>      <dbl>
    ## 1       1    54.7        56.9         53.6        52.0        53.0       53.6
    ## 2       2    79.3        97.3        109.         66.7        87.0       57.3
    ## 3       3    81.9        96.2         77.7       104.         86.9       94.7

We can see from the summary statistics that:

- Cluster 3 has the highest mean HP
- Cluster 2 has the highest mean attack
- Cluster 2 has the highest mean defense
- Cluster 3 has the highest mean special attack
- Cluster 2 has the highest mean special defense
- Cluster 3 has the highest mean speed
