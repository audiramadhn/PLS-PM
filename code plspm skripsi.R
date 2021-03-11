library(plspm)
library(car)

#inner model (Pengaruh variabel independent - dependent)
CX = c(0,0,0,0,0,0,0,0,0)
CB = c(0,0,0,0,0,0,0,0,0)
RA = c(0,0,0,0,0,0,0,0,0)
PD = c(0,0,0,0,0,0,0,0,0)
PC = c(0,0,0,0,0,0,0,0,0)
A = c(0,0,0,0,0,0,0,0,0)
D = c(0,0,0,0,0,0,0,0,0)
USF = c(1,1,1,1,1,1,1,0,0)
ITU = c(1,1,1,1,1,1,1,1,0)
path = rbind(CX,CB,RA,PD,PC,A,D,USF,ITU)

#masukan indikator variabel
block = list(1:3,4:6,7:11,12:13,14:15,16:18,19:21,22:24,25:27)

#model reflektif (A) & formatif (B)
modes = rep("A",9)

#jenis skala pengukuran (ORD=ordinal, NOM=nominal, NUM=rasio)
scaling = list(c("ORD","ORD","ORD"),c("ORD","ORD","ORD"),c("ORD","ORD","ORD","ORD","ORD"),
               c("ORD","ORD"),c("ORD","ORD"),c("ORD","ORD","ORD"),c("ORD","ORD","ORD"),c("ORD","ORD","ORD")
              ,c("ORD","ORD","ORD"))


#running pls
#scheme= ""path", "factor", "centroid"
pls = plspm(Data_Ordinal_Skripsi, path, block, modes, scaling, scheme = "path", boot.val = FALSE)
plot(pls)


#VIF
model1 = lm(ITU~., data = Total_Test_Skripsi)
model2 = lm(USF~ CX + CB + RA + PD + PC + A + D, data = Total_Test_Skripsi)
