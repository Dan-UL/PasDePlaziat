# https://www.fema.gov/openfema-data-page/fima-nfip-redacted-claims-v1


Flood_US <- read.csv("C:/Users/Dean/Desktop/H2023/Apprentissage Statistique/TP_Analyse/FimaNfipClaims.csv")

for (k in 1:length(state.name)){

  df <- subset(Flood_US, Flood_US$state == state.abb[k] )
  
  assign(paste0("Flood_",state.name[k]),df)
  
  # write.csv(df,paste0("C:/Users/Dean/Desktop/H2023/Apprentissage Statistique/TP_Analyse/Flood_",state.name[k]) )
  
}
