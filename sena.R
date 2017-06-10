#sena
#27-05-17


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
dez<-c( rep('character',7),rep(NULL,13))
u<-"file:///Users/ricardo/Desktop/projects/sena/data/D_MEGA.HTM"
u<-"file:///home/ricardo/hd3/projects/sena/data/D_MEGA.HTM"
#/home/ricardo/hd3/projects/sena/data
tables=readHTMLTable(u, stringsAsFactors=F ,colClasses=dez)[[1]]
tbx<-tables[!is.na(tables[,3]),][,-c(11,12)][,c(3:8)]
tx<-sapply(tbx,as.integer)
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
tx<-sapply(tbx,as.integer)
fet<-function(i,tx){
  uu<-table(tx[,i])
  uu<-sort(uu,decreasing=T) 
  return(as.data.frame(uu))
}
#r<-sapply(1:3,FUN=fet,tx=tx)
for(i in 1:6){
  r<-fet(i,tx)
  g <- ggplot(r, aes(Var1,Freq))
  g + geom_col()
  plot(r)
}
u<-table(unlist(tx));u<-sort(u,decreasing=T) 
#teste #