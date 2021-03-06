#��������� ��3-3########################################################################################
getwd()
data <- read.csv(file="https://raw.githubusercontent.com/SSkor/Learning-R/master/Modeling%20prices/hw1.csv",
                         header=TRUE, sep=',') #����� ����� ������������, ��������� ��������
shapiro <- apply(data, 2, shapiro.test)
pvs <- unlist(sapply(shapiro, function(x) x$p.value)) 
pv <- as.vector(pvs)
pvSW <- pv
pvSW[pvSW <= 0.05] <- 0
pvSW[pvSW > 0.05] <- 1
sum(pvSW) #���������� �������� �������� ������������
hist(pvs, seq(from = 0, to = 1, by = 0.05), main = "P-values Shapiro-Wilk") #����������� p-��������

pvk <- sapply(1:nrow(data), 
       function(i) 
         ks.test(as.vector(data[,i]),
                 'pnorm',mean = mean(data[,i]), 
                 sd = sd(data[,i]))$p.value)
hist(pvk, seq(from = 0, to = 1, by = 0.05), main = "P-values Kolmogorov�Smirnov")
pvKS <- pvk
pvKS[pvKS <= 0.05] <- 0
pvKS[pvKS > 0.05] <- 1
sum(pvKS) #���������� �������� �������� ������������

#��������� 29.03.18#####################################################################################
tab <- data.frame(pvKS, pvSW)
tabsum <- tab[,1] + tab[,2]
length(tabsum[tabsum == 1]) #� 163 ������ �� 1000 �������� �� ����������� ���� � ������ (16,3%)
write.csv2(tab, file = "Skorikova_hw1_pv.csv", row.names = FALSE)
########################################################################################################