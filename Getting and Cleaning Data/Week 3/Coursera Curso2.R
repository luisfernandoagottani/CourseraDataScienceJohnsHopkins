x<- 5
print(x)
x

##CREATING VECTORS - c=contetanar

x<- c(0.5,0.6)        ##numeric
x<- c(TRUE, FALSE)    ##logical
x<- c(T, F)           ##logical
x<- c("a", "b", "c")  ##character
x<- c(9:29)           ##integer
x<- c(1+0i, 2+4i)     ##comple

##USING THE VECTOR() FUNCTION

x<- vector("numeric", lenght = 10)
print(x)

##MIXING OBJECTS "When diferent objects are mixed in a vector, coercion occurs so that every element in the vector is of the same class

y<- c(1.7, a)    ##character
y<- c(TRUE, 2)   ##numeric
y<- c("a", TRUE) ##character

##EXPLICIT COERCION

x<- 0:6
class(x)
##retorna [1] "integer"
as.numeric(x)
##retorna [1] 0 1 2 3 4 5 6
as.logical(x)
##retorna [1] FALSE TRUE TRUE TRUE TRUE TRUE TRUE
as.character(x)
##retorna [1] "0" "1" "2" "3" "4" "5" "6"

##LIST - PARA DIVERSOS OBJETOS

x<- list(1, "a", TRUE, 1+4i)

##MATRIZES

m<- matrix(nrow=2, ncol=3)

##adicionando valores

m<- matrix(1:6, nrow=2, ncol=3)

##transformando vetor em matriz

m<- 1:10

dim(m)<- c(2,5)

##cbind and rbind - adicionar vetores a colunas ou linhas

x<- 1:3
y<- 10:12

cbind(x,y)

rbind(x,y)

##FACTORS

x<- factor(c("yes","yes","no","yes","no"))

##LEVELS IN FACTOR - DETERMINA A ORDEM DOS FATORES
x<- factor(c("yes","yes","no","yes","no"),
levels = c("yes", "no"))

##MISSING VALUES

x<- c(1, 2, NA, 10, 3)

is.na(x)

is.nan(x)

x<- c(1, 2, NaN, NA, 4)

is.na(x)

is.nan(x)

## DATA FRAMES

x<- data.frame(foo=1:4, bar=c(T,T,F,F))

##NAMES COLUMS

x<- 1:3

names(x)
NULL
names(x)<- c("foo", "bar", "norf")

##LIST NAMES

x<- list(a=1, b=2, c=3)

##NOMES DE MATRIZES

m<- matrix(1:4, nrow=2, ncol=2)

dimnames(m) <- list(c("a","b"), c("c","d"))

##SUBSETTING

x<- c("a","b","c","c","d","a")

x[1]

x[1:4]

x [x > "a"]

u<- x > "a"

x[u]

##SUBSETTING MATRIZ

x<- matrix(1:6, 2, 3)
x[1,2]

x[1, 2, drop = FALSE]

##REMOVING NA VALUES

x<- c(1, 2, NA, 4, NA, 5)
bad<- is.na(x)
x[!bad]

##instaling Swirl

install.packages("swirl")

##checking version

packageVersion("swirl")

##loanding Swirl

library(swirl)

t3<- strptime("October 17, 1986 08:24")

