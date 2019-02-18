frase <- c("No", "era", "el", "hombre", "más", "honesto", "ni", "el", "más", "piadoso", "pero", "era", "un", "hombre", "valiente")
length(frase)
nchar(frase)
length(unique(frase))
mean(nchar(frase))
median(nchar(frase))
sum(nchar(frase))
sort(nchar(frase))

# Define la función
moda <- function(x){
  unicx <- unique(x)
  unicx[which.max(tabulate(match(x, unicx)))]
}

moda(frase)
