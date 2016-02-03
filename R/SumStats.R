#' @name sumstatswcf
#' @title WC Sumstats HHMM18
#' @description mrm10

sumstats <- function(academicyear, sumstats=TRUE){
    
  #All data
  Fac0405 = read.csv("data/04 05 Faculty.csv")
  Fac0506 = read.csv("data/05 06 Faculty.csv")
  Fac0607 = read.csv("data/06 07 Faculty.csv")
  Fac0708 = read.csv("data/07 08 Faculty.csv")
  Fac0809 = read.csv("data/08 09 Faculty.csv")
  Fac0910 = read.csv("data/09 10 Faculty.csv")
  Fac1011 = read.csv("data/10 11 Faculty.csv")
  Fac1112 = read.csv("data/11 12 Faculty.csv")
  Fac1213 = read.csv("data/1213 Faculty.csv")
  Fac1314 = read.csv("data/1314 Faculty.csv")
  
  #Data Cleaned
  Fac0405r = na.omit(Fac0405)
  Fac0506r = na.omit(Fac0506)
  Fac0607r = na.omit(Fac0607)
  Fac0708r = na.omit(Fac0708)
  Fac0809r = na.omit(Fac0809)
  Fac0910r = na.omit(Fac0910)
  Fac1011r = na.omit(Fac1011)
  Fac1112r = na.omit(Fac1112)
  Fac1213r = na.omit(Fac1213)
  Fac1314r = na.omit(Fac1314)
  
  #All Average Ages
  avgage405 = round(with(Fac0405r,mean(Age)),digits=1)
  avgage506 = round(with(Fac0506r,mean(Age)),digits=1)
  avgage607 = round(with(Fac0607r,mean(Age)),digits=1)
  avgage708 = round(with(Fac0708r,mean(Age)),digits=1)
  avgage809 = round(with(Fac0809r,mean(Age)),digits=1)
  avgage910 = round(with(Fac0910r,mean(Age)),digits=1)
  avgage1011 = round(with(Fac1011r,mean(Age)),digits=1)
  avgage1112 = round(with(Fac1112r,mean(Age)),digits=1)
  avgage1213 = round(with(Fac1213r,mean(Age)),digits=1)
  avgage1314 = round(with(Fac1314r,mean(Age)),digits=1)
  
  
  #All Youngest and Oldest Professors by Year
  minage405 = with(Fac0405r,min(Age))
  maxage405 = with(Fac0405r,max(Age))
  minage506 = with(Fac0506r,min(Age))
  maxage506 = with(Fac0506r,max(Age))
  minage607 = with(Fac0607r,min(Age))
  maxage607 = with(Fac0607r,max(Age))
  minage708 = with(Fac0708r,min(Age))
  maxage708 = with(Fac0708r,max(Age))
  minage809 = with(Fac0809r,min(Age))
  maxage809 = with(Fac0809r,max(Age))
  minage910 = with(Fac0910r,min(Age))
  maxage910 = with(Fac0910r,max(Age))
  minage1011 = with(Fac1011r,min(Age))
  maxage1011 = with(Fac1011r,max(Age))
  minage1112 = with(Fac1112r,min(Age))
  maxage1112 = with(Fac1112r,max(Age))
  minage1213 = with(Fac1213r,min(Age))
  maxage1213 = with(Fac1213r,max(Age))
  minage1314 = with(Fac1314r,min(Age))
  maxage1314 = with(Fac1314r,max(Age))
  
  
  #Average Age by Department
  
  avgbydept405 = tapply(Fac0405r$Age,Fac0405r$Department,mean)
  avgbydept506 = tapply(Fac0506r$Age,Fac0506r$Department,mean)
  avgbydept607 = tapply(Fac0607r$Age,Fac0607r$Department,mean)
  avgbydept708 = tapply(Fac0708r$Age,Fac0708r$Department,mean)
  avgbydept809 = tapply(Fac0809r$Age,Fac0809r$Department,mean)
  avgbydept910 = tapply(Fac0910r$Age,Fac0910r$Department,mean)
  avgbydept1011 = tapply(Fac1011r$Age,Fac1011r$Department,mean)
  avgbydept1112 = tapply(Fac1112r$Age,Fac1112r$Department,mean)
  avgbydept1213 = tapply(Fac1213r$Age,Fac1213r$Department,mean)
  avgbydept1314 = tapply(Fac1314r$Age,Fac1314r$Department,mean)
  
  
  #Average Age by Gender
  avgbygender405 = tapply(Fac0405r$Age,Fac0405r$Gender,mean)
  avgbygender506 = tapply(Fac0506r$Age,Fac0506r$Gender,mean)
  avgbygender607 = tapply(Fac0607r$Age,Fac0607r$Gender,mean)
  avgbygender708 = tapply(Fac0708r$Age,Fac0708r$Gender,mean)
  avgbygender809 = tapply(Fac0809r$Age,Fac0809r$Gender,mean)
  avgbygender910 = tapply(Fac0910r$Age,Fac0910r$Gender,mean)
  avgbygender1011 = tapply(Fac1011r$Age,Fac1011r$Gender,mean)
  avgbygender1112 = tapply(Fac1112r$Age,Fac1112r$Gender,mean)
  avgbygender1213 = tapply(Fac1213r$Age,Fac1213r$Gender,mean)
  avgbygender1314 = tapply(Fac1314r$Age,Fac1314r$Gender,mean)
  
  
  #Average Age by Terminal Degree
  avgagebytermd405 = tapply(Fac0405r$Age,Fac0405r$Year.of.Terminal,mean)
  avgagebytermd506 = tapply(Fac0506r$Age,Fac0506r$Year.of.Terminal,mean)
  avgagebytermd607 = tapply(Fac0607r$Age,Fac0607r$Year.of.Terminal,mean)
  avgagebytermd708 = tapply(Fac0708r$Age,Fac0708r$Year.of.Terminal,mean)
  avgagebytermd809 = tapply(Fac0809r$Age,Fac0809r$Year.of.Terminal,mean)
  avgagebytermd910 = tapply(Fac0910r$Age,Fac0910r$Year.of.Terminal,mean)
  avgagebytermd1011 = tapply(Fac1011r$Age,Fac1011r$Year.of.Terminal,mean)
  avgagebytermd1112 = tapply(Fac1112r$Age,Fac1112r$Year.of.Terminal,mean)
  avgagebytermd1213 = tapply(Fac1213r$Age,Fac1213r$Year.of.Terminal,mean)
  avgagebytermd1314 = tapply(Fac1314r$Age,Fac1314r$Year.of.Terminal,mean)
  
  
  ###Do certain departments require more time to earn a PhD or Terminal Degree in than others?
  ###Time between Bachelors and Terminal Degree by Department
  avgbytermdegdept405 = tapply(Fac0405r$B.A.to.Terminal,Fac0405r$Department,mean)
  avgbytermdegdept506 = tapply(Fac0506r$B.A.to.Terminal,Fac0506r$Department,mean)
  avgbytermdegdept607 = tapply(Fac0607r$B.A.to.Terminal,Fac0607r$Department,mean)
  avgbytermdegdept708 = tapply(Fac0708r$B.A.to.Terminal,Fac0708r$Department,mean)
  avgbytermdegdept809 = tapply(Fac0809r$B.A.to.Terminal,Fac0809r$Department,mean)
  avgbytermdegdept910 = tapply(Fac0910r$B.A.to.Terminal,Fac0910r$Department,mean)
  avgbytermdegdept1011 = tapply(Fac1011r$B.A.to.Terminal,Fac1011r$Department,mean)
  avgbytermdegdept1112 = tapply(Fac1112r$B.A.to.Terminal,Fac1112r$Department,mean)
  avgbytermdegdept1213 = tapply(Fac1213r$B.A.to.Terminal,Fac1213r$Department,mean)
  avgbytermdegdept1314 = tapply(Fac1314r$B.A.to.Terminal,Fac1314r$Department,mean)
  
  #Summary Statistics of Age
  sumage405 = with(Fac0405r,summary(Age))
  sumage506 = with(Fac0506r,summary(Age))
  sumage607 = with(Fac0607r,summary(Age))
  sumage708 = with(Fac0708r,summary(Age))
  sumage809 = with(Fac0809r,summary(Age))
  sumage910 = with(Fac0910r,summary(Age))
  sumage1011 = with(Fac1011r,summary(Age))
  sumage1112 = with(Fac1112r,summary(Age))
  sumage1213 = with(Fac1213r,summary(Age))
  sumage1314 = with(Fac1314r,summary(Age))
  
  #Standard Deviation of Age
  sdage405 = with(Fac0405r,sd(Age))
  sdage506 = with(Fac0506r,sd(Age))
  sdage607 = with(Fac0607r,sd(Age))
  sdage708 = with(Fac0708r,sd(Age))
  sdage809 = with(Fac0809r,sd(Age))
  sdage910 = with(Fac0910r,sd(Age))
  sdage1011 = with(Fac1011r,sd(Age))
  sdage1112 = with(Fac1112r,sd(Age))
  sdage1213 = with(Fac1213r,sd(Age))
  sdage1314 = with(Fac1314r,sd(Age))
  
  #Number of Faculty Over Time
  numberoffaculty405 = 405 
  numberoffaculty506 = 404
  numberoffaculty607 = 417
  numberoffaculty708 = 409
  numberoffaculty809 = 422
  numberoffaculty910 = 374
  numberoffaculty1011 = 378
  numberoffaculty1112 = 383
  numberoffaculty1213 = 401
  numberoffaculty1314 = 418
  

if(sumstats==TRUE){
  result = get(paste0("sumage",academicyear))
}
else{
  result = get(paste0("avgage", academicyear))
}

print(result)
}