if(!file.exists("./data")){dir.create("./data")}
reportsUrl <- "https://github.com/ConnieZ/ufo_app/raw/master/data/ufo_reports.csv"
download.file(reportsUrl, destfile="./data/ufo_reports.csv", mode="wb")
milbaseUrl <- "https://github.com/ConnieZ/ufo_app/raw/master/data/milbases.csv"
download.file(milbaseUrl, destfile="./data/milbases.csv", mode="wb")


