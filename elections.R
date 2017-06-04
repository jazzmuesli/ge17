setwd("~/ge2017")
ge15 = read.csv("~/ge2017/ge_2015_results.csv")
ge10 = read.csv("~/ge2017/ge_2010_results.csv")
head(ge10)
data = cbind(ge10[,c("Constituency.Name","Electorate","Votes","Lab","Con","LD","Grn","UKIP","SNP")], data.frame(year=2010))
data = rbind(data, cbind(ge15[,c("Constituency.Name","Electorate","Lab","LD","UKIP","SNP")], data.frame(Con=ge15$C, Grn=ge15$Green,Votes=ge15$Valid.Votes, year=2015)))
View(data[order(data$Constituency.Name),])
euref = read.csv("~/ge2017/Revised estimates of leave vote in Westminster constituencies - demog_based_estimates.csv")
euref=data.frame(Constituency.Name=euref$Constituency, leave=euref$Figure.to.use, year=2016)
m = merge(data, euref[,c("Constituency.Name","leave")],by="Constituency.Name",all=T)
write.csv(x = m, file = "combined.csv")

library(rjson)
ygov = rjson::fromJSON(file="constituency_detailed_results.json")
yd = data.frame()
for (i in 2:length(ygov)) {
  jitems = ygov[[i]]$d
  for (p in 1:length(jitems)) {
    if (length(jitems[[p]]) == 3) {
      item = data.frame(i=i, party=p)
      item[,c("est","lo","hi")] = jitems[[p]]
      yd = rbind(yd, item)
    }
  }
}

cnames = data.frame(Constituency.Name=readLines(file("names.txt")))
cnames$i = 2:(nrow(cnames)+1)
yd = merge(yd,cnames)
parties = data.frame()
parties = rbind(parties, data.frame(p=1,Party.Name="Con"))
parties = rbind(parties, data.frame(p=2,Party.Name="Lab"))
parties = rbind(parties, data.frame(p=3,Party.Name="LD"))
parties = rbind(parties, data.frame(p=4,Party.Name="UKIP"))
parties = rbind(parties, data.frame(p=5,Party.Name="SNP"))
parties = rbind(parties, data.frame(p=6,Party.Name="Plaid"))
parties = rbind(parties, data.frame(p=7,Party.Name="Grn"))
parties = rbind(parties, data.frame(p=8,Party.Name="Other"))
parties$party = parties$p
yd = merge(yd, parties)

write.csv(x = yd, file = "yougov.csv")

ygest = reshape(yd[,c("Constituency.Name","Party.Name","est")], timevar="Party.Name", idvar="Constituency.Name", direction="wide")
names(ygest) = gsub("est.","pct.", names(ygest))
ygest$year = 2017

for (party in intersect(names(m), parties$Party.Name)) {
  m[,paste0("pct.", party)] = m[,party] / m$Votes * 100
}

library(plyr)

mm = rbind.fill(m, ygest[,intersect(names(m), names(ygest))])
mm = arrange(mm, Constituency.Name, year)
