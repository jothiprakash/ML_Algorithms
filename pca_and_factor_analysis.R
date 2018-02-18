# Import the data
mydata <- read.csv("pca_gsp.csv")
attach(mydata)

str(mydata)

colnames_mydata <- names(mydata)

# Defining variables
x <- cbind(Ag,
           Mining,
           Constr,
           Manuf,
           Manuf_nd,
           Transp,
           Comm,
           Energy,
           TradeW,
           TradeR,
           RE,
           Services,
           Govt)

# Descriptive statistics
summary(x)
cor(x)

# Principal Component Analysis
pca1 <- princomp(x = x,
                 cor = TRUE,
                 scores = TRUE)
summary(pca1)

# Loadings of principal components
loadings(pca1)
# pca1$loadings

# Screeplot for eigen values
plot(pca1)
screeplot(x = pca1,
          type = "line",
          main = "Scree Plot")

# Bi plot for score variables
biplot(x = pca1)

# Scores of the components
pca1$scores[1:10, ]


# FACTOR ANALYSIS
fa1 <- factanal(x = x,
                factors = 3)
