data = read.csv2("data/valgdata2013.csv", encoding = "UTF-8")
data = data[data$Parti != "Andre2",]

mandater_per_fylke = list(
  "Akershus" = 17,
  "Buskerud" = 9,
  "Hedmark" = 7,
  "Møre og Romsdal" = 9 ,
  "Nord-Trøndelag" = 5,
  "Oslo" = 19,
  "Sogn og Fjordane" = 4,
  "Telemark" = 6,
  "Vest-Agder" = 6,
  "Østfold" = 9,
  "Aust-Agder" = 4,
  "Finnmark" = 5,
  "Hordaland" = 16,
  "Nordland" = 9,
  "Oppland" = 7,
  "Rogaland" = 14,
  "Sør-Trøndelag" = 10,
  "Troms" = 6,
  "Vestfold" = 7
)

stemmedata = aggregate(Stemmer.totalt ~ Parti + Fylkenavn, data, FUN = sum)
stemmedata = subset(stemmedata)

fylker = unique(stemmedata$Fylkenavn)
partier = unique(stemmedata$Parti)

distriktsmandata = data.frame(fylke = c(), parti = c(), mandater = c())

for(fylke in fylker) {
  subdata = subset(stemmedata, Fylkenavn == fylke)
  stemmer = subdata$Stemmer.totalt

  mandater = distriktsmandat(mandater_per_fylke[[fylke]] - 1, stemmer)

  submandata = data.frame(fylke = rep(fylke, length(mandater)), parti = subdata$Parti, mandater = mandater)
  submandata = subset(submandata, mandater > 0)

  distriktsmandata = rbind(distriktsmandata, submandata)
}


distriktsmandat_per_parti = aggregate(mandater ~ parti, distriktsmandata, FUN = sum)

for(parti in partier) {
  if( !(parti %in% distriktsmandat_per_parti$parti) ) {
    distriktsmandat_per_parti = rbind(distriktsmandat_per_parti, c(parti, 0))
  }
}

# order alphabetically
distriktsmandat_per_parti <- distriktsmandat_per_parti[order(distriktsmandat_per_parti$parti), ]
distriktsmandat_per_parti$mandater <- as.numeric(distriktsmandat_per_parti$mandater)

# get country-wide votes
landsstemmer = aggregate(Stemmer.totalt ~ Parti, data, FUN = sum)
landsstemmer$Prosent = landsstemmer$Stemmer.totalt*100/sum(landsstemmer$Stemmer.totalt)



sperregrense = 1.0


# exclude votes for parties below sperregrense
relevante_stemmer = landsstemmer$Stemmer.totalt
relevante_stemmer[landsstemmer$Prosent < sperregrense] = 0


# tildel utjevningsmandater
for(M in 169:0) {
  landsmandater = distriktsmandat(M, relevante_stemmer)
  tildelte_mandater = sum(pmax(landsmandater, distriktsmandat_per_parti$mandater))

  print(M)
  if(tildelte_mandater == 169) {
    mandater_slutt = pmax(landsmandater, distriktsmandat_per_parti$mandater)
    break
  }
}


mandata = data.frame(parti = landsstemmer$Parti, mandater = mandater_slutt)

