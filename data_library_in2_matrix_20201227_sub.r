load.data20201227.2040=function(){
  # Get Files
  library(readxl)
  Data1<-read_excel("a0v20200420.1550_L1700_L2000_combined_drug.info_modi.xlsx",
                  sheet = "Drug_library_info")
}

remove.unnecessary.fields.20201228.0033=function(){
  attach(Data1)
  remove.col=c("Barcode",
               "Product_Name",
               "Plate_Location",
               "Rack_Number",
               "new_rack_number",
               "new_Plate_Location",
               "Information",
               "M.w.",
               "CAS_Number",
               "DMSO_(mg/mL)Max_Solubility",
               "DMSO_(mM)Max_Solubility",
               "water_(mg/mL)Max_Solubility",
               "water_(mM)Max_Solubility",
               "URL",
               "Formula",
               "Form",
               "Synonyms",
               "SMILES",
               "ALogP",
               "HBA_Count",
               "HBD_Count",
               "RotatableBond",
               "middle_rack_number",
               "middle_Plate_Location")
  y1<-setdiff(colnames(Data1),remove.col)
  Data2<-Data1[,y1]
}

head(Data2)

#x=Data2
string.split.20201229.0945=function(x=Data2){
  library(tidyr)
  x=as.data.frame(x,stringsAsFactors=F)
  x1=separate_rows(x,sep = ',',Target)
  for (i in Target){
    strsplit(Target[i], ",")
  }
}

compare.data.20201229.1512=function(){
  n_occur<-data.frame(table(x1$Target))
  n_occur[n_occur$Freq > 1,]
  x1[x1$Target %in% n_occur$Var1[n_occur$Freq > 1],]
}

Target<-sort(unique(x1$Target))
target_catalog<-list()
length(target_catalog)<-length(Target)
names(target_catalog)<-Target
for(i in 1:length(target_catalog)){
  print(names(target_catalog[i]))
  target_catalog[[i]]<-subset(x1, x1$Target==names(target_catalog)[i])$Catalog_Number
}
Data3=Data2
Data3$comb=paste0(Data3$Target,',',Data3$Pathway)
Data4=separate_rows(Data3,sep=',','comb')
Data4$comb2=multi_sub(Data4$comb,
                      patterns = c(" DNA replication inhibitor",
                                   " DNA replication inhibitor (human)",
                                   "11bHSD1","11β-HDS1","11β-HSD1",
                                   "11β-hydroxysteroid dehydrogenase type 1",
                                   "11β-hydroxysteroid dehydrogenase type I ",
                                   "11βHSD1","5-HT Receptor","5-HT3 receptor",
                                   "5-HT3 Receptor","5-lipoxygenase",
                                   "5-alpha Reductase","5-Hydroxytryptamine 2B Receptor",
                                   "acetyl cholinesterase","AChE",
                                   "AChE inhibitor"),
                      replacements = c('DNA_replication_inhibitor',
                                       'DNA_replication_inhibitor',
                                       '11b-HSD1','11b-HSD1','11b-HSD1',
                                       '11b-HSD1',
                                       '11b-HSD1',
                                       '11b-HSD1',"5-HT","5-HT3",
                                       "5-HT3","5-LOX",
                                       "5-α reductase","5-HT2B",
                                       "acetylcholinesterase","acetylcholinesterase",
                                       "acetylcholinesterase"))
head(sort(unique(Data4$comb2)),40)
?separate_rows
library('plyr')

multi_sub=function(input,patterns,replacements){
  for(i in 1:length(patterns)){
    wh=which(input==patterns[i])
    input[wh]=replacements[i]
  }
  return(input)
}
