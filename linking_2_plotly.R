library(shiny)
library(plotly)
library(dplyr)


# préparation données bidons

I1 <-  rnorm(500,mean = 10, sd=4)
I2 <-  rnorm(500)
I4 <-  rnorm(500,mean = 10, sd=4) * rnorm(500)
I3 <-  rnorm(500, mean = 24, sd=0.2)

O1 <- sample(seq(from=-2, to=5), replace = T, size = 500)
O2 <- sample(seq(from=-5, to=2 ), replace = T, size = 500)

df <-  data.frame(I1, I2, I3, I4, O1, O2)
df$ID <- paste0(df$O1,df$O2)

plot(df[,1:4])


# affichage plotly
library(plotly)


#highlight key pour définir les variables à query lors de la selection d'un individu dans le graphique 
#ici je surveille tout , mais on peut ne surveiller qu'une selection de colonnes
pp <- highlight_key(df)


# objet plotly vide (= sans trace)
base <- plot_ly(pp, color = I("black"), showlegend = FALSE)
#ajout des traces
p1 <- add_trace(base, x = ~I1)
p2 <- add_trace(base, x = ~I4)
p3 <- add_trace(base, x = ~O1, y = ~O2)



#layout pour la disposition des subplots 
finalplot <- subplot(subplot(p1,p2, nrows=2),p3)
#optionnel : définition du mode de selection 
finalplot <- layout(finalplot, dragmode="lasso")
# définition de la MAJ du dessins en fonction de l'évènement utilisateur (souris)
finalplot <- highlight(finalplot, on="plotly_selected")

finalplot

#export
library(htmlwidgets)
saveWidget(finalplot, "~/tmp/pingpong.html")