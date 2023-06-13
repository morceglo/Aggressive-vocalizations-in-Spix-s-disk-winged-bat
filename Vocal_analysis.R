library(tidyverse)
library(umap)
theme_set(theme_bw(18))

#Aggressive vocalizations during intergroup interactions in roost 
#defense in the Spix's disk-winged bat. Silvia Chaves-Ramírez et al.

data <- read.csv("Vocal_analysis.csv")
## Edit the dataset, remove the columns num,
## labels and add an ID with name of the row number.
data <- data %>% 
  drop_na() %>%
  select(-num)%>%
  select(-label)%>%
  mutate(ID=row_number()) 

set.seed(142)
umap_fit <- data %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>% 
  umap()

umap_df <- umap_fit$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(ID=row_number())%>%
  inner_join(data, by="ID")

umap_df %>% head()

umap_df %>%
  ggplot(aes(x = UMAP1, 
             y = UMAP2, 
             color = call_type))+ 
  geom_point()+
  labs(x = "UMAP1",
       y = "UMAP2") 

ggsave("Figure 6.png" , width=  7.5, height = 5, dpi = 600 )

#K nearest neighbor
#Leave only relevant columns (type of call, UMAP values)

umap_df_subset <- umap_df[c(1:2,13)]


#Create train and test data (https://www.edureka.co/blog/knn-algorithm-in-r/)

set.seed(123)
dat.d <- sample(3:nrow(umap_df_subset),size=nrow(umap_df_subset)*0.7,replace = FALSE) #random selection of 70% data.

train.umap <- umap_df_subset[dat.d,] # 70% training data
test.umap <- umap_df_subset[-dat.d,] # remaining 30% test data
str(train.umap)


#Creating seperate dataframe for 'call type' feature which is our target
train.umap_labels <- umap_df_subset[dat.d,3]
test.umap_labels <-umap_df_subset[-dat.d,3]


#Building a Machine Learning model

#Install class package
install.packages('class')
# Load class package
library(class)


#Find the number of observations
NROW(train.umap_labels) 

#So, we have 189 observations in our training data set. The square root of 189 is around 13.74, therefore we'll create two models. One with 'K' value as 13 and the other model with a 13'K' value as 14.

knn.13 <- knn(train=train.umap, test=test.umap, cl=train.umap_labels, k=13)
knn.14 <- knn(train=train.umap, test=test.umap, cl=train.umap_labels, k=14)

#Model Evaluation

#Calculate the proportion of correct classification for k = 13, 14
ACC.13 <- 100 * sum(test.umap_labels == knn.13)/NROW(test.umap_labels)
ACC.14 <- 100 * sum(test.umap_labels == knn.14)/NROW(test.umap_labels)

ACC.13

ACC.14


#Check prediction against actual value in tabular form for k=13
table(knn.13 ,test.umap_labels)

# Check prediction against actual value in tabular form for k=14
table(knn.14 ,test.umap_labels)


#Confusion matrix to calculate the accuracy

install.packages('caret')
library(caret)

confusionMatrix(table(knn.13, test.umap_labels))
#Accuracy : 0.9877

