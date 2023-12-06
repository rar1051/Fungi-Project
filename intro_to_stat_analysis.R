
install.packages("DAAG")
library(DAAG)
data(cuckoos)
# description: length and breadth measurements both mm lain in nests of 6 species 
head(cuckoos)
str(cuckoos)
levels(cuckoos$species)
# study aim: determine if the type of species has an effect on the average length of cuckoo eggs


#box plot 

library(ggplot2)
ggplot(cuckoos,
       aes(x = species, y = length)) +
  geom_boxplot() +
  labs(x = "Host Nest Species", y = "Cuckoo Egg Length (mm)")

# hypothesize: we hypothesize that cuckoo egg length will vary depending on host species 

str(cuckoos)
# independant variable: species (categorical)
# dependant variable: cuckoo egg length (numerical,continuous)

#ANOVA
#normality: assumes data was given from an anova distributed population, can test using visualizations histogram or qq plot or shapiro wilks test 

hist(cuckoos$length)
shapiro.test(cuckoos$length)
# null hypothesis is that sample comes from normal distribution
#alt is that sample does not come from normal distribution 
# reject null hypothesis is p value is less than .05 
# greater than .05 fail to reject null 
# greater than .05 the data is normally distributed, less than .05 the data is not normally distrubuted 

model_aov <- aov(length~species, data = cuckoos)
summary(model_aov)

#res df total obvs# - #groups/species 120-6=114
#mean sum of squares species = variation between sample species
## sp sum sq/ df species
# mean sum of squares residuals = variation within samples 
## res sum squ/ df resid 

#f stat (f calc, f value) mean sum sq sp / mean sum sq res which is var b/w samples/ varia w/i samples = f calc  
# f value of < .05 there is a significant difference 
# to determine the difference between individual species you use a tukey test

# does species shell breadth predict shell length ?

# regression 

library(ggplot2)
ggplot(data = cuckoos, aes(
  x = breadth, y = length)) +
    geom_point()+
    geom_smooth(method = "lm", color = "pink") + 
    labs( x = "Shell breadth", y = "shell length" )
  )

# geom_point is used to add the dots , geom_smooth adds the regression line 

model_reg <- lm(length~breadth, data = cuckoos) # length as pre
summary(model_reg)

#predictor = bredath, response = length
# estimate: avg increase of response variable associated with 1 unit increase in predictor variable 
# std error: measure of uncertainty 
# t-calc: estimate/ std error
#Pr(>|t|)  < 0.05 sig 
# the info supports the hypothesis that shell breadth predicts shell length 
#Residual standard error: average distance values fall from the regression line,  smaller means the line has a better fit 
# degrees of freedom: n - k - 1 = df 
# 120- k(predictor) - 1 = 118

#multiple R squared: closer to 1.0 the better the model is at predicting. SO better the predictor is at predicting the response. 
# p value less than 0.05 than this model will fit data better than a model with no predictors 

# making a histogram for each individual species 

cuckoos_species <- unique(cuckoos$species) # vector to identify which species are going to be pulled from in our for loop 

#using loop tp create histogram for each host species 

for(species in cuckoos_species) {
  subset_data <- cuckoos[cuckoos$species == species, ] #subset data for current species 
  hist(subset_data$length, main = species, xlab = "Shell Length")
}


