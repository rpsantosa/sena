#sena
#27-05-17
# to seve pass in mac os
# git config --global credential.helper osxkeychain  
# git remote add origin https://github.com/rpsantosa/sena.git
# git config --global user.email "ricardo_paixao_santos@yahoo.com.br"
# git config --global user.name "rpsantosa"
# git config remote.origin.url  git@github.com:rpsantosa/sena.git
# https://github.com/rpsantosa/sena.git
# if the origin is wrong
# git remote remove origin
# git remote add origin git@github.com:rpsantosa/sena.git

#######to troubleshooting########################
# git remote -v  # if remote nickname is origin
# git push -u origin master ( to activate buttons on git panel)
# git status         ( if it is the right directory)

#probabilities
out<-vector()
fs<-function(x){ 1/(choose(6,x)*choose(54,6-x)/choose(60,6))}
for(i in 1:6){
  out<-rbind(out,c(i,fs(i)))
}
out<-as.integer(sapply(1:6,fs))
data.frame('k'=1:6,'p'=out)


#data
library(XML)
#dez<-c( rep('character',7),rep(NULL,13))
## mac solution
temp <- tempfile()
download.file('http://www1.caixa.gov.br/loterias/_arquivos/loterias/D_megase.zip',temp)
u<-unzip(temp, files = "D_MEGA.HTM")
tables=readHTMLTable(u, stringsAsFactors=F )[[1]]
# ## end mac solution
#
# u<-"file:///Users/ricardo/Desktop/projects/sena/data/D_MEGA.HTM"
# u<-"file:///home/ricardo/hd3/projects/sena/data/D_MEGA.HTM"
#/home/ricardo/hd3/projects/sena/data
#tables=readHTMLTable(u, stringsAsFactors=F)[[1]]
tbx<-tables[!is.na(tables[,3]),]
tx<-sapply(tbx[,c(3:8)],as.integer)
u<-table(unlist(tx));u<-sort(u,decreasing=T) 
plot(u)
# 5  53  51  23   4  10  33  17  42  54  28  41  13  30  24  43   2  16  52  50  32  27  29 
# 224 223 217 214 213 213 211 209 209 209 207 206 205 205 204 203 202 202 202 201 199 198 198 
# 34  37  44  49  56  36  47  18   1   6  12  31  45   8  59  38  58  35   3  11   7  20  46 
# 197 197 197 197 196 195 195 194 192 192 192 192 192 190 190 189 189 188 187 186 185 183 183 
# 60  19  40  48  57  15  14  25   9  39  21  22  55  26 
# 182 179 179 178 178 177 176 176 173 172 168 166 164 158 
## End(Not run)
d<-c(mean(u)+sd(u),mean(u)-sd(u))
#last 100
txlast<-tail(tx,100)
u<-table(unlist(txlast));u<-sort(u,decreasing=T) 


# for each ten
library(ggplot2)
library(plyr)
library(tidyverse)
tx<-sapply(tbx[,c(3:8)],as.integer)
fet<-function(i,tx){
  uu<-table(tx[,i])
  uu<-sort(uu,decreasing=T) 
  return(as.data.frame(uu))
}
#frequencies in each draw
rlist<-lapply(1:6, FUN = fet, tx=tx);names(rlist)<-1:6
rlistd<-ldply(.data=rlist,.fun=cbind)

# subsetting data: data.frame and data.table
#rlistd[which(rlistd$Var1==4),]  # subsetting data.frame
library(data.table)
rl<-data.table(rlistd)
#rl[Var2==4]                   # using data.table
rl[,Var2:=as.integer(as.character(rl[,Var1]))]
rlo<-order(rl[,Var2])
rl<-rl[rlo]
g<-ggplot(rl, aes(x=as.factor(Var2),y=Freq))
g + geom_col(aes(fill = .id)) + geom_hline(aes(yintercept=mean(Freq)))+facet_grid(.id~ .) 
u<-table(unlist(tx));u<-sort(u,decreasing=T) 
#teste #

#### language hints
mtcars %>%
  split(.$cyl) %>% # from base R
  map(~ lm(mpg ~ wt, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")


x<-mtcars %>%  split(.$cyl)
y<-x %>% map(~ lm(mpg ~ wt, data = .)) %>% 
map(summary) %>%
map_dbl("r.squared")

x1 <- list(
  c(0.27, 0.37, 0.57, 0.91, 0.20),
  c(0.90, 0.94, 0.66, 0.63, 0.06), 
  c(0.21, 0.18, 0.69, 0.38, 0.77)
)
x2 <- list(
  c(0.50, 0.72, 0.99, 0.38, 0.78), 
  c(0.93, 0.21, 0.65, 0.13, 0.27), 
  c(0.39, 0.01, 0.38, 0.87, 0.34)
)

threshold <- function(x, cutoff = 0.8) x[x > cutoff]
x1 %>% sapply(threshold) %>% str()
