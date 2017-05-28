#sena
#27-05-17


#probabilities
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
tables=readHTMLTable(u, stringsAsFactors=F ,colClasses=dez)[[1]]
tb<-tables[!is.na(tables[,3]),][,-c(11,12)]
tbx<-as.integer(tb[,c(3:8)])
tx<-sapply(tbx,as.integer)
u<-table(unlist(tx));u<-sort(u,decreasing=T) 
5  53  51  23   4  10  33  17  42  54  28  41  13  30  24  43   2  16  52  50  32  27  29 
224 223 217 214 213 213 211 209 209 209 207 206 205 205 204 203 202 202 202 201 199 198 198 
34  37  44  49  56  36  47  18   1   6  12  31  45   8  59  38  58  35   3  11   7  20  46 
197 197 197 197 196 195 195 194 192 192 192 192 192 190 190 189 189 188 187 186 185 183 183 
60  19  40  48  57  15  14  25   9  39  21  22  55  26 
182 179 179 178 178 177 176 176 173 172 168 166 164 158 
## End(Not run)
