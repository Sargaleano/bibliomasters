## Revealing the research landscape of Master’s degrees via bibliometric analyses ##
## Authors:  Sergio A. Rojas and Nathalia Chaparro
## R script for the results reported in the above paper. 
## This script loads the data, runs the analyses, and generates and saves the plots
## (c) March, 2021

# ---------------------------------------------------------------------- #
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version. 
# See license at: https://www.gnu.org/licenses/gpl.txt
# ---------------------------------------------------------------------- #


## Library installation ##
# If "bibliometrix" not recognised because of clash of versions, try installing first:
# install.packages("Rcpp")
# install.packages("tibble")
# install.packages("rlang")
# install.packages("bibliometrix")
# install.packages("ggplot2") 
library("bibliometrix")
library("ggplot2")
library("stringr")
library("networkD3")
library("webshot")

## Set your data working directory here ##
setwd("./data")


################################### Lists for text cleanup and preprocessing #####################################
stopwords <- c("empleando","determinar","principalmente","variable","mejorar","medio","evaluar","aplicacion","herramienta",
              "modelo","investigacion","computador","sistema","metodo","condiciones","fase","identificar",
              "conceptos","propuesto","area","durante","actual","finalmente","alto","pretende","elementos",
              "documento","realizo","cuenta","principales","manera","propone","enfoque","aumentar","grado",
              "tanto","forma","vez","hasta","resultado","francisco","jose","caldas","mayor","ademas","anterior",
              "realizacion","mismos","ello","sido","sino","gran","tal","tres","cuales","permitan","relacionados",
              "relacionadas","existentes","estas","otras","primera","ordenes","necesidades","necesidad","tales",
              "metodologias","permita","esto","desarrolla","establecer","debe","asi","lograr","basada","obtenido",
              "contiene","debido","objetivo","proposito","describe","estos","otros","posible","partir","ya","sea",
              "misma","parte","capitulo","tambien","plantea","busca","proyectos","problema","no","ha","han",
              "traves","base","nivel","realiza","se","este","son","resultados","diferentes","presenta","presente",
              "permite","tipo","proyecto","mas","que","de","del","al","en","mediante","multiples","para","sector",
              "caso","propuesta","analisis","diseno","metodologia","ingenieria","mejor","dado","segun","usando",
              "todos","permiten","actualmente","embargo","tesis","mismo","acuerdo","posteriormente",
              "teniendo","necesario","general")
synonyms <- c("variable;variables;caracteristica;caracteristicas","computador;computadores",
              "colombia;colombiano;nacional;pais","bogota;ciudad;municipio;region;dc",
              "universidad;facultad;distrital","organizacion;organizacional;empresa;empresas;empresarial",
              "problema;problemas","herramienta;herramientas","suministro;sumininstro;cadena",
              "mediante;basado;utilizando","tecnologia;tecnologias;tecnologica","realiza;realizar;desarrollar",
              "metodo;metodos;tecnica;tecnicas;metodologia;metodologica","servicio;servicios",
              "sistema;sistemas","modelo;modelos","diseno;diseo","obtenido;obtenidos",
              "investigacion;propuesta;evaluacion;estudio;formulacion;implementacion;solucion;desarrollo",
              "proyecto;proyectos","produccion;producto;productos","proceso;procesos","imagen;imagenes",
              "industria;industrias;industrial","propuesto;desarrollado","mejor;mejores",
              "algoritmo;algoritmos","redes;red")
ngrams <- c("sistemas expertos","investigacion de operaciones","toma de decisiones",
            "gestion del conocimiento","dinamica de sistemas","logica difusa","tipo 2",
            "inteligencia artificial","inteligencia colectiva","red neuronal",
            "redes neuronales","mineria de datos","analisis envolvente de datos",
            "colonia de hormigas")
##################################################################################################################


######################################## Customised plots theme settings ######################################### 
my_theme <- theme(axis.text=element_text(size=10, family="serif"), axis.title=element_text(size=10,face="plain")) +
  theme(axis.text.x=element_text(color="black"), axis.text.y=element_text(color="black")) +
  theme(panel.background=element_rect(fill='#EFEFEF')) +
  theme(panel.border=element_rect(linetype="solid",fill=NA)) +
  theme(panel.grid.major=element_line(color="#D9D9D9",size=.55,linetype="dotted")) +
  theme(panel.grid.minor=element_line(color="#D9D9D9",size=.55,linetype="dotted")) +
  theme(plot.margin=unit(c(.1,.1,.1,.1), "cm"))  
##################################################################################################################


################################# Load data, run bibliometrix and show summary ###################################
corpus <- "MIE-dissertations-2010-2020.bib"
# corpus <- "MIE-dissertations-2016-2020.bib"
# corpus <- "MIS-dissertations-2012-2020.bib"
# corpus <- "MIS-dissertations-2016-2020.bib"

## for bibliometrix under v3.0.0 ##
# D <- readFiles(corpus) 
# M <- convert2df(D, dbsource="scopus", format="bibtex")

## for bibliometrix v3.0.0 and up ##
M <- convert2df(corpus, dbsource="scopus", format="bibtex")
results <- biblioAnalysis(M)
summary(results, k=20, pause=FALSE)
x <- results     # Variable containing the results used in the analyses
k <- 10          # Number of top results to analise in most plots
# plot(x=results, k=20, pause=FALSE)   # Uncomment to produce default bibliometrix plots
##################################################################################################################


##################################### Plot: Production growth ####################################################
Tab=table(x$Years)

# insert missing years #
YY=setdiff(seq(min(x$Years, na.rm=TRUE),max(x$Years, na.rm=TRUE)),names(Tab))
Y=data.frame(Year=as.numeric(c(names(Tab),YY)),Freq=c(as.numeric(Tab),rep(0,length(YY))))
Y=Y[order(Y$Year),]

names(Y)=c("Year","Freq")
g <- ggplot(Y, aes(x=.data$Year, y=.data$Freq)) +
  geom_line() + geom_area(fill='#4C9900', alpha=.5) +
  geom_point(shape=21,fill="darkgreen",color="white",size=3) +
  labs(x='Year', y='Documents', title="") +
  scale_x_continuous(breaks= (Y$Year[seq(1,length(Y$Year),by=1)]),expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(Y$Freq),2), expand=c(0,.8)) +
  my_theme
plot(g)
ggsave("production-growth.png", width=4, height=2.5, dpi=300)
##################################################################################################################


################################# Plot: Production distribution ##################################################
k <- 12

## Authors ##
xx <- as.data.frame(x$Authors[1:k])
g <- ggplot(data=xx, aes(x=reorder(substr(word(.data$AU,1), 0, 18), -.data$Freq), y=.data$Freq)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="", x="Authors", y="Document count") +
  scale_y_continuous(breaks=seq(0,max(xx$Freq),2), expand=c(0, 0.5)) + 
  my_theme + coord_flip() + scale_x_discrete(label = function(x) str_trunc(x, 16)) 
plot(g)
ggsave("production-distribution-authors.png", width=4, height=2.5, dpi=300)

## Groups ## 
xx <- as.data.frame(x$Sources[1:k])
g <- ggplot(data=xx, aes(x=reorder(word(.data$SO, -1), -.data$Freq), y=.data$Freq)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="", x="Group") + labs(y="Document count") +
  scale_y_continuous(breaks=seq(0, max(xx$Freq), 2), expand=c(0, 0.5)) + 
  my_theme + coord_flip()
plot(g)
ggsave("production-distribution-groups.png", width=4, height=2.5, dpi=300)
##################################################################################################################


############################ Plot: Citation count and average citation ###########################################
Table2=aggregate(x$TotalCitation,by=list(x$Years),length)
Table2$xx <- aggregate(x$TotalCitation,by=list(x$Years),mean)$x
Table2$Annual=NA
d=date()
d=as.numeric(substring(d,nchar(d)-3,nchar(d)))
Table2$Years=d-Table2$Group.1
Table2$Annual=Table2$xx/Table2$Years
Table2$Count=aggregate(x$TotalCitation, by=list(x$Years), sum)$x
names(Table2)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears","Count")

## insert missing years ##
YY=setdiff(seq(min(x$Years,na.rm=TRUE),max(x$Years,na.rm=TRUE)),Table2$Year)
if (length(YY>0)){
  YY=data.frame(YY,0,0,0,0)
  names(YY)=c("Year","N","MeanTCperArt","MeanTCperYear","CitableYears","Count")
  Table2=rbind(Table2,YY)
  Table2=Table2[order(Table2$Year),]
  row.names(Table2)=Table2$Year}

## citation count plot ##
g <- ggplot(Table2, aes(x=.data$Year, y=.data$Count)) +
  geom_line() + geom_area(fill='#4C9900', alpha=.5) +
  geom_point(shape=21, fill="darkgreen", color="white", size=3) +
  labs(x='Year', y='Total cites', title="") +
  scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=1)]), expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(Table2$Count),2), expand=c(0,.8)) +
  my_theme
plot(g)
ggsave("citation-count.png", width=4, height=2.5, dpi=300)

## average article citation per year ##
g <- ggplot(Table2, aes(x=.data$Year, y=.data$MeanTCperYear)) +
  geom_line() + geom_area(fill='#4C9900', alpha=.5) +
  geom_point(shape=21, fill="darkgreen", color="white", size=3) +
  labs(x='Year', y='Average document cites per year', title="") +
  scale_x_continuous(breaks= (Table2$Year[seq(1,length(Table2$Year),by=1)]), expand=c(0,.3)) +
  my_theme
plot(g)
ggsave("citation-average-year.png", width=4, height=2.5, dpi=300)
##################################################################################################################


#################################### Plot: Citation distribution  ############################################
k <- 10

## Documents ##
xx <- data.frame(x$MostCitedPapers$Paper[1:k], x$MostCitedPapers$TC[1:k])
colnames(xx) <- c("Paper", "TC")
g <- ggplot(data=xx, aes(x=reorder(.data$Paper,-.data$TC), y=.data$TC)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="",x="Document") + labs(y="Total cites") +
  scale_y_continuous(breaks=seq(0,max(xx$TC),1), expand=c(0,0.1)) + 
  scale_x_discrete(label = function(x) paste(str_trunc(word(x, 1), 11), 
                                             '(', substr(x, nchar(x)-5, nchar(x)-2), ')', sep="")) +
  my_theme + coord_flip() 
plot(g)
ggsave("citation-distribution-documents.png", width=4, height=2.5, dpi=300)

## Documents per year ##
xx <- data.frame(x$MostCitedPapers$Paper[1:k], x$MostCitedPapers$TCperYear[1:k])
colnames(xx) <- c("Paper", "TCperYear")
g <- ggplot(data=xx, aes(x=reorder(.data$Paper,-.data$TCperYear),y=.data$TCperYear)) +
  geom_bar(stat="identity",fill="#4C9900",alpha=0.8) +
  labs(title="", x="Document") + labs(y="Cites per year") +
  scale_y_continuous(expand=c(0,0.02))+
  scale_x_discrete(label = function(x) paste(str_trunc(word(x, 1), 11), 
                                             '(', substr(x, nchar(x)-5, nchar(x)-2), ')', sep="")) +
  my_theme + coord_flip()
plot(g)
ggsave("citation-distribution-documents-per-year.png", width=4, height=2.5, dpi=300)
##################################################################################################################


####################################### Plot: Author's timeline ##################################################
k <- 12

## Authors ##
res <- authorProdOverTime(M, k, graph=F)
res$graph <- res$graph + 
  geom_line(data=res$dfAU,aes(x=.data$Author,y=.data$year,group=.data$Author),size=1.0,color="green",alpha=0.2) +
  labs(title="", x="", y="") +
  geom_point(aes(alpha=res$dfAU$TCpY,size=res$dfAU$freq), color="darkgreen") +
  guides(size=guide_legend(order=1,"Documents"),alpha=guide_legend(order=2,"Cites/Year")) +
  guides(x=guide_axis(angle=45)) + scale_x_discrete(label = function(x) str_trunc(x, 16)) +
  theme(axis.text.y=element_text(face="plain"), axis.text.x=element_text(face="plain")) +
  theme(legend.title=element_text(size=7), legend.text=element_text(size=7)) +
  my_theme
plot(res$graph)
ggsave("authors-timeline.png", width=4, height=3, dpi=300)

## Groups ##
G <- data.frame(M)
G$AU <- G$SO           # Pretend the author field correspond to group names ($SO)
G$AU <- word(G$AU,-1)  # Trim out the last word of group name
res <- authorProdOverTime(G, k, graph=F)
res$graph <- res$graph + 
  geom_line(data=res$dfAU,aes(x=.data$Author,y=.data$year,group=.data$Author),size=1.0,color="green",alpha=0.2) +
  labs(title="", x="", y="") +
  geom_point(aes(alpha=res$dfAU$TCpY,size=res$dfAU$freq), color="darkgreen") +
  guides(size=guide_legend(order=1,"Documents"),alpha=guide_legend(order=2,"Cites/Year")) +
  guides(x=guide_axis(angle=45)) + scale_x_discrete(label = function(x) str_trunc(x, 16)) +
  theme(axis.text.y=element_text(face="plain"), axis.text.x=element_text(face="plain")) +
  theme(legend.title=element_text(size=7), legend.text=element_text(size=7)) +
  scale_size_continuous(breaks=c(1,2,4,6)) +
  my_theme
plot(res$graph)
ggsave("groups-timeline.png", width=4, height=3, dpi=300)
##################################################################################################################


##################################### Plot: Word trends ##########################################################
library(reshape2)
k <- 10

## Author's Keywords ##
topKW <- KeywordGrowth(M, Tag="DE", sep=";", top=k, cdf=TRUE)
DF <- melt(topKW, id='Year')
g <- ggplot(DF, aes(Year, value, group=variable, color=variable)) +
  geom_line() + geom_point(shape=21,size=2) +
  scale_x_continuous(breaks= (DF$Year[seq(1,length(Table2$Year),by=2)]), expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(DF$value), as.integer(max(DF$value)/5)), expand=c(0,.3)) + 
  guides(x=guide_axis(angle=45)) + labs(x='', y='Cumulative word count', title="") +
  scale_colour_discrete(labels = function(x) str_wrap(x, width=15)) +
  my_theme + theme(legend.title=element_blank(), legend.text=element_text(size=8)) 
plot(g)
ggsave("word-trends-keywords.png", width=4, height=3, dpi=300)

## Individual's Keywords ##
topKW <- KeywordGrowth(M, Tag="ID", sep=";", top=k, cdf=TRUE)
DF <- melt(topKW, id='Year')
g <- ggplot(DF, aes(Year, value, group=variable, color=variable)) +
  geom_line() + geom_point(shape=21,size=2) +
  scale_x_continuous(breaks= (DF$Year[seq(1,length(Table2$Year),by=2)]), expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(DF$value), as.integer(max(DF$value)/5)), expand=c(0,.8))+ 
  guides(x=guide_axis(angle=45)) + labs(x='', y='Cumulative word count', title="") +
  scale_colour_discrete(labels = function(x) str_wrap(x, width=15)) +
  my_theme + theme(legend.title=element_blank(), legend.text=element_text(size=8)) 
plot(g)
ggsave("word-trends-id-keywords.png", width=4, height=3, dpi=300)

## Titles words ##
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$TI_TM <- sapply(strsplit(TT$TI_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
topKW <- KeywordGrowth(TT, Tag="TI_TM", sep=";", top=k, cdf=TRUE)
DF <- melt(topKW, id='Year')
g <- ggplot(DF, aes(Year, value, group=variable, color=variable)) +
  geom_line() + geom_point(shape=21,size=2) +
  scale_x_continuous(breaks= (DF$Year[seq(1,length(Table2$Year),by=2)]), expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(DF$value), as.integer(max(DF$value)/5)), expand=c(0,.8))+ 
  guides(x=guide_axis(angle=45)) + labs(x='', y='Cumulative word count', title="") +
  scale_colour_discrete(labels = function(x) str_wrap(x, width=15)) +
  my_theme + theme(legend.title=element_blank(), legend.text=element_text(size=8)) 
plot(g)
ggsave("word-trends-title.png", width=4, height=3, dpi=300)

## Abstracts words ##
AT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
AT$AB_TM <- sapply(strsplit(AT$AB_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
topKW <- KeywordGrowth(AT, Tag="AB_TM", sep=";", top=k, cdf=TRUE)
DF <- melt(topKW, id='Year')
g <- ggplot(DF, aes(Year, value, group=variable, color=variable)) +
  geom_line() + geom_point(shape=21,size=2) +
  scale_x_continuous(breaks= (DF$Year[seq(1,length(Table2$Year),by=2)]), expand=c(0,.3)) +
  scale_y_continuous(breaks=seq(0,max(DF$value), as.integer(max(DF$value)/5)), expand=c(0,.8))+ 
  guides(x=guide_axis(angle=45)) + labs(x='', y='Cumulative word count', title="") +
  scale_colour_discrete(labels = function(x) str_wrap(x, width=15)) +
  my_theme + theme(legend.title=element_blank(), legend.text=element_text(size=8)) 
plot(g)
ggsave("word-trends-abstract.png", width=4, height=3, dpi=300)
##################################################################################################################


###################################### Plot: Frequent words ######################################################
k <- 15

## Author's Keywords ##
wordFreq <- tableTag(M, Tag="DE", sep=";")
xx <- as.data.frame(wordFreq[1:k])
colnames(xx) <- c("Word", "Frequency")
g <- ggplot(data=xx, aes(x=reorder(.data$Word,-.data$Frequency), y=.data$Frequency)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="",x="Keyword") + labs(y="Frequency") +
  scale_y_continuous(breaks=seq(0,max(xx$Frequency),2), expand=c(0,0.3)) +
  scale_x_discrete(labels = function(x) str_trunc(x, 24)) +  
  my_theme + coord_flip()
plot(g)
ggsave("frequent-words-keywords.png", width=4, height=3, dpi=300)

## Individual's Keywords ##
wordFreq <- tableTag(M, Tag="ID", sep=";")
xx <- as.data.frame(wordFreq[1:k])
colnames(xx) <- c("Word", "Frequency")
g <- ggplot(data=xx, aes(x=reorder(.data$Word,-.data$Frequency), y=.data$Frequency)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="",x="Word") + labs(y="Frequency") +
  scale_y_continuous(breaks=seq(0,max(xx$Frequency),2), expand=c(0,0.3))+ 
  scale_x_discrete(labels = function(x) str_trunc(x, 20)) +  
  my_theme + coord_flip()
plot(g)
ggsave("frequent-words-id-keywords.png", width=4, height=3, dpi=300)

## Titles words ##
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$TI_TM <- sapply(strsplit(TT$TI_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
wordFreq <- tableTag(TT, Tag="TI_TM", sep=";")
xx <- as.data.frame(wordFreq[1:k])
colnames(xx) <- c("Word", "Frequency")
g <- ggplot(data=xx, aes(x=reorder(.data$Word,-.data$Frequency), y=.data$Frequency)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="",x="Word") + labs(y="Frequency") +
  scale_y_continuous(breaks=seq(0,max(xx$Frequency),5), expand=c(0,0.8))+ 
  scale_x_discrete(labels = function(x) str_trunc(x, 20)) +  
  my_theme + coord_flip()
plot(g)
ggsave("frequent-words-titles.png", width=4, height=3, dpi=300)

## Abstracts words ##
AT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
AT$AB_TM <- sapply(strsplit(AT$AB_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
wordFreq <- tableTag(AT, Tag="AB_TM", sep=";")
xx <- as.data.frame(wordFreq[1:k])
colnames(xx) <- c("Word", "Frequency")
g <- ggplot(data=xx, aes(x=reorder(.data$Word,-.data$Frequency), y=.data$Frequency)) +
  geom_bar(stat="identity", fill="#4C9900", alpha=0.8) +
  labs(title="",x="Word") + labs(y="Frequency") +
  scale_y_continuous(breaks=seq(0,max(xx$Frequency),20), expand=c(0,0.8))+ 
  scale_x_discrete(labels = function(x) str_trunc(x, 20)) +  
  my_theme + coord_flip()
plot(g)
ggsave("frequent-words-abstracts.png", width=4, height=3, dpi=300)
##################################################################################################################


####################################### Plot: Wordcloud ##########################################################
library("webshot")
# webshot::install_phantomjs()
library("htmlwidgets")
library("wordcloud2")

## Author's Keywords ##
wordFreq <- tableTag(M, Tag="DE", sep=";")
xx <- as.data.frame(wordFreq)
colnames(xx) <- c("word", "freq")
wordcloud2(xx, size=.3, minSize=10, gridSize= 0, fontFamily="Impact", fontWeight='bold',
             color='random-dark', backgroundColor="white", shuffle=TRUE,
             minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
             shape='circle', ellipticity=0.5, widgetsize=c(1000,500)) # + WCtheme(4)
g <- wordcloud2(xx, size=.3, minSize=10, gridSize= 0, fontFamily="Andale Mono", fontWeight='bold',
             color='random-dark', backgroundColor="white", shuffle=TRUE,
             minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
             shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
saveWidget(g,"aux.html",selfcontained=FALSE)
webshot("aux.html","wordcloud-keywords-web.png", 
        delay=4, vwidth=1000, vheight=500, cliprect="viewport")

## Individual's Keywords ##
wordFreq <- tableTag(M, Tag="ID", sep=";")
xx <- as.data.frame(wordFreq)
colnames(xx) <- c("word", "freq")
wordcloud2(xx, size=.3, minSize=5, gridSize= 0, fontFamily="Impact", fontWeight='bold',
           color='random-dark', backgroundColor="white", shuffle=TRUE,
           minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
           shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
g <- wordcloud2(xx, size=.3, minSize=5, gridSize= 0, fontFamily="Andale Mono", fontWeight='bold',
             color='random-dark', backgroundColor="white", shuffle=TRUE,
             minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
             shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
saveWidget(g,"aux.html",selfcontained=FALSE)
webshot("aux.html","wordcloud-id-keywords-web.png", 
        delay=4, vwidth=1000, vheight=500, cliprect="viewport")

## Titles words ##
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$TI_TM <- sapply(strsplit(TT$TI_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
wordFreq <- tableTag(TT, Tag="TI_TM", sep=";")
xx <- as.data.frame(wordFreq)
colnames(xx) <- c("word", "freq")
wordcloud2(xx, size=.5, minSize=5, gridSize= 0, fontFamily="Impact", fontWeight='bold',
           color='random-dark', backgroundColor="white", shuffle=TRUE,
           minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
           shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
g <- wordcloud2(xx, size=.5, minSize=4, gridSize= 0, fontFamily="Andale Mono", fontWeight='bold',
             color='random-dark', backgroundColor="white", shuffle=TRUE,
             minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
             shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
saveWidget(g,"aux.html",selfcontained=FALSE)
webshot("aux.html","wordcloud-titles-web.png", 
        delay=4, vwidth=1000, vheight=500, cliprect="viewport")

## Abstracts words ##
AT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
AT$AB_TM <- sapply(strsplit(AT$AB_TM, ";"), 
                   function(x) paste(unique(x), collapse = ";")) # Remove duplicates
wordFreq <- tableTag(AT, Tag="AB_TM", sep=";")
xx <- as.data.frame(wordFreq)
colnames(xx) <- c("word", "freq")
wordcloud2(xx, size=.4, minSize=10, gridSize= 0, fontFamily="Impact", fontWeight='bold',
           color='random-dark', backgroundColor="white", shuffle=TRUE,
           minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
           shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
g <- wordcloud2(xx, size=.4, minSize=10, gridSize= 0, fontFamily="Andale Mono",
             color='random-dark', backgroundColor="white", shuffle=TRUE,
             minRotation=-pi/4, maxRotation=pi/4, rotateRatio=0, 
             shape='circle', ellipticity=0.5, widgetsize=c(1000,500))
saveWidget(g,"aux.html",selfcontained=FALSE)
webshot("aux.html","wordcloud-abstracts-web.png", 
        delay=3, vwidth=1000, vheight=500, cliprect="viewport")
##################################################################################################################


##################################### Plot: Topic Map ############################################################
## Author's Keywords topics ##
CS <- conceptualStructure(M, field="DE", method="MDS", stemming=FALSE, minDegree=3, clust=5,
                         k.max=8, labelsize=9, documents=5, graph=FALSE)
g <- CS$graph_terms + my_theme + theme(legend.position="right") + labs(title="") + 
  guides(fill = guide_legend(title = "Topic"),
         shape = guide_legend(title = "Topic"), 
         color =  guide_legend(title = "Topic"))
plot(g)
ggsave("topic-map-keywords.png", width=4, height=3, dpi=300)

## Individual Keywords topics ##
CS <- conceptualStructure(M, field="ID", method="MCA", stemming=FALSE, minDegree=6, clust=5,
                         k.max=8, labelsize=9, documents=10, graph=FALSE)
g <- CS$graph_terms + my_theme + theme(legend.position="right") + labs(title="") + 
  guides(fill = guide_legend(title = "Topic"),
         shape = guide_legend(title = "Topic"), 
         color =  guide_legend(title = "Topic"))
plot(g)
ggsave("topic-map-id-keywords.png", width=4, height=3, dpi=300)

## Title topics ##
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$DE <- TT$TI_TM  # Pretend the extracted terms are keywords ($DE) 
CS <- conceptualStructure(TT, field="DE", method="MCA", stemming=FALSE, minDegree=6, clust=5,
                         k.max=8, labelsize=9, documents=5, graph=FALSE)
g <- CS$graph_terms + my_theme + theme(legend.position="right") + labs(title="") + 
      guides(fill = guide_legend(title = "Topic"),
             shape = guide_legend(title = "Topic"), 
             color =  guide_legend(title = "Topic"))
plot(g)
ggsave("topic-map-titles.png", width=4, height=3, dpi=300)

## Abstracts topics ##
AT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
AT$DE <- AT$AB_TM  # Pretend the extracted terms are keywords ($DE)
CS <- conceptualStructure(AT, field="DE", method="MCA", stemming=FALSE, minDegree=12, clust=5,
                         k.max=8, labelsize=9, documents=5, graph=FALSE)
g <- CS$graph_terms + my_theme + theme(legend.position="right") + labs(title="") + 
  guides(fill = guide_legend(title = "Topic"),
         shape = guide_legend(title = "Topic"), 
         color =  guide_legend(title = "Topic"))
plot(g)
ggsave("topic-map-abstracts.png", width=4, height=3, dpi=300)
##################################################################################################################


###################################### Plot: Word dendrogram #####################################################
cbPalette <- c(RColorBrewer::brewer.pal(9, 'Set1')[-6], RColorBrewer::brewer.pal(8, 'Set2')[-7], 
               RColorBrewer::brewer.pal(12, 'Paired')[-11], RColorBrewer::brewer.pal(12, 'Set3')[-c(2,8,12)])

## Author's Keywords dendrogram ##
clust <- 4
CS <- conceptualStructure(M, field="DE", method="MCA", stemming=FALSE, minDegree=3, clust=clust,
                         k.max=8, labelsize=9, documents=5, graph=FALSE)
CS$km.res$labels <- str_trunc(CS$km.res$labels, 25)   # Truncate labels on dendogram

# g <- CS$graph_dendogram + labs(title="", x="", y="") + my_theme + 
#   theme(axis.text.x=element_blank()) + theme(plot.margin=unit(c(.0,.0,.0,.0), "cm")) +
#   scale_y_continuous(expand=c(0,1))
g <- factoextra::fviz_dend(CS$km.res, rect = TRUE, k=clust, cex=0.7, k_colors = cbPalette[clust:1]) + 
  labs(title="", x="", y="") + scale_y_continuous(expand=expansion(add = c(3, 0.1))) + my_theme + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), plot.margin=unit(c(.0,.0,.0,.0), "cm")) 
plot(g)
ggsave("dendrogram-keywords.png", width=4, height=3, dpi=300)

## Individual Keywords dendrogram ##
clust <- 5
CS <- conceptualStructure(M, field="ID", method="MCA", stemming=FALSE, minDegree=6, clust=clust,
                          k.max=8, labelsize=9, documents=5, graph=FALSE)
CS$km.res$labels <- str_trunc(CS$km.res$labels, 18)   # Truncate labels on dendogram
g <- factoextra::fviz_dend(CS$km.res, rect = TRUE, k=clust, cex=0.8, k_colors = cbPalette[clust:1]) + 
  labs(title="", x="", y="") +   scale_y_continuous(expand=expansion(add = c(1.3, 0.1))) + my_theme + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), plot.margin=unit(c(.0,.0,.0,.0), "cm")) 
plot(g)
ggsave("dendrogram-id-keywords.png", width=4, height=3, dpi=300)

## Title dendrogram ##
clust <- 5
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$DE <- TT$TI_TM  # Pretend the extracted terms are keywords ($DE)
CS <- conceptualStructure(TT, field="DE", method="MCA", stemming=FALSE, minDegree=6, clust=clust,
                         k.max=8, labelsize=9, documents=5, graph=FALSE)
CS$km.res$labels <- str_trunc(CS$km.res$labels, 18)   # Truncate labels on dendogram
g <- factoextra::fviz_dend(CS$km.res, rect = TRUE, k=clust, cex=0.8, k_colors = cbPalette[clust:1]) + 
  labs(title="", x="", y="") +   scale_y_continuous(expand=expansion(add = c(2, 0.1))) + my_theme + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), plot.margin=unit(c(.0,.0,.0,.0), "cm")) 
plot(g)
ggsave("dendrogram-titles.png", width=4, height=3, dpi=300)

## Abstract dendrogram ##
clust <- 5
TT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
TT$DE <- TT$AB_TM  # Pretend the extracted terms are keywords ($DE)
CS <- conceptualStructure(TT, field="DE", method="MCA", stemming=FALSE, minDegree=14, clust=clust,
                         k.max=8, labelsize=5, documents=5, graph=FALSE)
CS$km.res$labels <- str_trunc(CS$km.res$labels, 18)
g <- factoextra::fviz_dend(CS$km.res, rect = TRUE, k=clust, cex=0.8, k_colors = cbPalette[clust:1]) + 
  labs(title="", x="", y="") +   scale_y_continuous(expand=expansion(add = c(1.3, 0.1))) + my_theme + 
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), plot.margin=unit(c(.0,.0,.0,.0), "cm")) 
plot(g)
ggsave("dendrogram-abstracts.png", width=4, height=3, dpi=300)
##################################################################################################################


################################## Plot: Co-occurrence networks ##################################################
## Co-ocurrence Author's keywords ##
net <- biblioNetwork(M, analysis="co-occurrences", network="author_keywords", sep=";")
g <- networkPlot(net, n=100, type="fruchterman", Title="", labelsize=1, alpha=0.9,
                 cluster="louvain", edgesize=5, edges.min=1, verbose=T, weighted=T,
                 label.cex=F, label.n=35, size.cex=F, remove.isolates=T,
                 vos.path="VOSviewer.jar") 
# net2VOSviewer(g)
dev.print(png,filename="coocurrence-keywords.png", width=600, height=600);
dev.off()

## Co-ocurrence Individual keywords ##
net <- biblioNetwork(M, analysis="co-occurrences", network="keywords", sep=";")
g <- networkPlot(net, n=60, type="fruchterman", Title="", labelsize=1, alpha=0.9,
                 cluster="louvain", edgesize=5, edges.min=1, verbose=T, weighted=T,
                 label.cex=F, label.n=30, size.cex=F, remove.isolates=T)
dev.print(png,filename="coocurrence-id-keywords.png", width=600, height=600);
dev.off()

## Co-ocurrence title words ##
TT <- termExtraction(M, Field="TI", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
net <- biblioNetwork(TT, analysis="co-occurrences", network="titles", sep=";")
g <- networkPlot(net, n=60, type="fruchterman", Title="", labelsize=1, alpha=0.9,
                 cluster="louvain", edgesize=3, edges.min=1, verbose=T, weighted=T,
                 label.cex=F, label.n=30, size.cex=F, remove.isolates=T)
dev.print(png,filename="coocurrence-titles.png", width=600, height=600);
dev.off()

## Co-ocurrence abstract words ##
AT <- termExtraction(M, Field="AB", stemming=FALSE, language="spanish", 
                     remove.terms=stopwords, synonyms=synonyms, keep.terms=ngrams, remove.numbers=FALSE)
net <- biblioNetwork(AT, analysis="co-occurrences", network="abstracts", sep=";")
g <- networkPlot(net, n=60, type="fruchterman", Title="", labelsize=1, alpha=0.9,
                 cluster="louvain", edgesize=5, edges.min=1, verbose=T, weighted=T,
                 label.cex=F, label.n=30, size.cex=F, remove.isolates=T)
dev.print(png,filename="coocurrence-abstracts.png", width=600, height=600);
dev.off()
##################################################################################################################


####################################### Plot: Thematic maps ######################################################
## Thematic author's keywords ##
res <- thematicMap(M, field="DE", n=100, minfreq=20, size=.1, repel=TRUE, n.labels=3)
g <- res$map + theme(panel.border=element_rect(linetype="solid", fill=NA))
plot(g)
ggsave("thematic-map-keywords.png", width=6, height=4, dpi=300)

## Thematic individual keywords ##
res <- thematicMap(M, field="ID", n=300, minfreq=10, size=.1, repel=TRUE, n.labels=4)
g <- res$map + theme(panel.border=element_rect(linetype="solid", fill=NA))
plot(g)
ggsave("thematic-map-id-keywords.png", width=6, height=4, dpi=300)
##################################################################################################################


#################################### Plot: Collaboration networks ################################################
## Authors collaboration  ##
net <- biblioNetwork(M, analysis="collaboration", network="authors", sep=";")
net@Dimnames[[1]] <- str_trunc(net@Dimnames[[1]], 18)
net@Dimnames[[2]] <- str_trunc(net@Dimnames[[2]], 18)
g <- networkPlot(net, n=70, type="fruchterman", Title="", labelsize=6, alpha=0.8, 
                 size.cex=T, label.cex=T, degree=NULL, , weighted=T, halo=T, 
                 cluster="louvain", edgesize=1.5, edges.min=1, verbose=T) 
dev.print(png,filename="collaboration-authors.png", width=800, height=800);
dev.off()

## Authors coupling ##
net <- biblioNetwork(M, analysis="coupling", network="authors", sep=";")
net@Dimnames[[1]] <- str_trunc(net@Dimnames[[1]], 18)
net@Dimnames[[2]] <- str_trunc(net@Dimnames[[2]], 18)
g <- networkPlot(net, n=31, label.n=30, type="fruchterman", Title="", labelsize=1.5, alpha=0.8, 
                 size.cex=F, label.cex=F, degree=NULL, , weighted=T, halo=T,
                 cluster="infomap", edgesize=3, edges.min=1, verbose=T) 
dev.print(png,filename="coupling-authors.png", width=800, height=800);
dev.off()
##################################################################################################################

################################### Plot: Bibliography networks ###################################################
## Co-citation network ##
net <- biblioNetwork(M, analysis="co-citation", network="references", sep=";")
x <- net@Dimnames[[2]]
net@Dimnames[[2]] <- paste(word(x, 1), ' (', substr(x, nchar(x)-3, nchar(x)), ')', sep="")
g <- networkPlot(net, n=40, label.n=20, type="fruchterman", Title="", labelsize=1.8, alpha=1,
                 cluster="louvain", edgesize=1, edges.min=1, verbose=T, weighted=T, halo=T)
dev.print(png,filename="cocitation-references.png", width=800, height=400);
dev.off()

## Manuscript coupling ##
net <- biblioNetwork(M, analysis="coupling", network="references", sep=";")
x <- net@Dimnames[[2]]
net@Dimnames[[2]] <- paste(word(x, 1), ' (', substr(x, nchar(x)-3, nchar(x)), ')', sep="")
g <- networkPlot(net, n=40, label.n=30, type="fruchterman", Title="", labelsize=1.8, alpha=1,
                 cluster="louvain", edgesize=10, edges.min=1, verbose=T, weighted=T, halo=T)
dev.print(png,filename="coupling-manuscripts.png", width=800, height=400);
dev.off()
##################################################################################################################


################################### Plot: Energy flow diagram  ###################################################

## Focus on conceptual structure ## 
threeFieldsPlot(M, fields=c("SO","DE","AU"), n=c(12,12,12))
g <- threeFieldsPlot(M, fields=c("SO","DE","AU"), n=c(12, 12, 12), 1200, 600)
g <- htmlwidgets::onRender(g,'function(el, x) { d3.selectAll(".node text").style("font-size", "24");}')
g
saveNetwork(g, "aux.html", selfcontained=TRUE)
webshot("aux.html", "energy-flow-conceptual.png",
        delay=4, vwidth=1200, vheight=600, cliprect="viewport")

## Focus on social structure ## 
threeFieldsPlot(M, fields=c("DE","SO","AU"), n=c(12,12,12))
g <- threeFieldsPlot(M, fields=c("DE","SO","AU"), n=c(12, 12, 12), 1200, 600)
g <- htmlwidgets::onRender(g,'function(el, x) { d3.selectAll(".node text").style("font-size", "24");}')
g
saveNetwork(g, "aux.html", selfcontained=TRUE)
webshot("aux.html", "energy-flow-social.png",
        delay=4, vwidth=1200, vheight=600, cliprect="viewport")
##################################################################################################################


###### END OF SCRIPT ###### 
