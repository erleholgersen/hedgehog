library(reshape2)
library(aplpack)

hordaland = subset(data, Fylkenavn == "Oslo")

hordaland_wide <- dcast(hordaland, Kommunenavn ~ Parti, value.var="ProsentAndel")

faces(hordaland_wide[,-1], labels = hordaland_wide$Kommunenavn, face.type = 2)

x
