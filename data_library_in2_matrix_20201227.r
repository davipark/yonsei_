#=============================
# Drug library data matrix
#=============================
setwd('/home/david/RStudioProjects')
source('data_library_in2_matrix_20201227_sub.r')
  #Load Data
   load.data20201227.2040()

   #Parsing Data
Data2<-remove.unnecessary.fields.20201228.0033(x=Data1)

#Split string (Separate a collapsed column into multiple rows)
string.split.20201229.0945
head(x1)

