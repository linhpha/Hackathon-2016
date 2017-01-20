# Produce recipe
install.packages('plyr')
library(plyr)
recipe <- rbind.fill(allr,menu)
write.csv(recipe,"recipe.csv") ## Linh's code to clean recipe


# binding2: Remove duplicates and create undirected network
binding <- read.csv("data/binding.csv", stringsAsFactors = F)
binding <- binding[-1]
library(reshape2)
binding2 <- dcast(binding, ingredient.x + ingredient.y ~ country.x)
for (i in 1:nrow(binding2))
{
  binding2[i,c(1,2)] = sort(binding2[i,c(1,2)]) #quite slow
}
binding2 <- binding2[!duplicated(binding2[,c(1,2)]),]
write.csv(binding2,"data/binding2.csv")

#attributes: node attributes
attributes <- read.csv("data/attributes.csv", stringsAsFactors = F)

#category.Prop
attributes <- attributes[-1]
x <- list()
x[[1]] <- unique(attributes[[2]])
for (i in 3:ncol(attributes))
{
  x[[i-1]] <- aggregate(attributes[[i]], by = list(attributes$Category), sum)[[2]]
}

c <- colnames(attributes)[3:ncol(attributes)]
category.Prop <- do.call("cbind.data.frame", x)
colnames(category.Prop) <- c("Ingredients", c)
Other <- category.Prop[7,]
category.Prop <- category.Prop[-7,]
category.Prop <- category.Prop[order(category.Prop$Ingredients,decreasing = T),]
category.Prop <- rbind(Other,category.Prop)
write.csv(category.Prop, "data/categoryProp.csv")
