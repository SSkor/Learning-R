##DATA INPUT----
GDAXI <- read.csv("C:/Users/Svetlana/Desktop/diploma R prog/data/^GDAXI.csv", 
                  header=TRUE, 
                  sep=",", 
                  dec="." )
View(GDAXI)
#S&P 500
GSPC <- read.csv("C:/Users/Svetlana/Desktop/diploma R prog/data/^GSPC.csv", 
                  header=TRUE, 
                  sep=",", 
                  dec="." )
#NASDAQ Composite
IXIC <- read.csv("C:/Users/Svetlana/Desktop/diploma R prog/data/^IXIC.csv", 
                  header=TRUE, 
                  sep=",", 
                  dec="." )
#SSE Composite Index
SSEC <- read.csv("C:/Users/Svetlana/Desktop/diploma R prog/data/^SSEC.csv", 
                  header=TRUE, 
                  sep=",", 
                  dec="." )
##----
#df <- GDAXI[,c("Date", "Close")]
#df <- IXIC[,c("Date", "Close")]
#df <- SSEC[,c("Date", "Close")]
df <- GSPC[,c("Date", "Close")]
traddays <- df[df[, 2] > 0, ]
