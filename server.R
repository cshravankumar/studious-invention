
#SentimentTimeSeries <- function(DB = data.frame(Industry = dataset["Industry"], Category = dataset["Category"], Brand = dataset["Brand"], ReviewTime = dataset["reviewTime"], Sentiment.Score = dataset["Sentiment.Score"], reviewTime = as.Date(0,origin="1970-01-01"), SalesRank = dataset["SalesRank"], Overall = dataset["overall"], Price = dataset["Price"], Quality = dataset["Quality"]))
##############################################







###COMPARISON TIME SERIES FUNCTION
Overall_Comparison <- function(DB1 = temp1, DB2 = temp2, fromdate, todate)
{
  
  division=c(" "," ")
  timevector=as.character(DB1$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB1$year[i]=time[[i]][3]
    DB1$month[i]=time[[i]][1]
    DB1$day[i]=time[[i]][2]
  }
  
  DB1$year=as.numeric(DB1$year)
  DB1$month=as.numeric(DB1$month)
  DB1$day=as.numeric(DB1$day)
  
  
  
  division=c(" "," ")
  timevector=as.character(DB2$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB2$year[i]=time[[i]][3]
    DB2$month[i]=time[[i]][1]
    DB2$day[i]=time[[i]][2]
  }
  
  DB2$year=as.numeric(DB2$year)
  DB2$month=as.numeric(DB2$month)
  DB2$day=as.numeric(DB2$day)
  
  
  
  #assumption for trying
  #fromdate
  #todate
  
  
  SY=2010
  SM=7
  FY=2013
  FM=1
  
  
  
  intlength1=(FY-SY)*12+(FM-SM+1)
  totscore1=c(0,length=intlength1)
  frequency1=c(0,length=intlength1)
  avgsent1=c(0,length=intlength1)
  a1=b1=c1=0
  
  p_totscore1=c(0,length=intlength1)
  p_frequency1=c(0,length=intlength1)
  p_avgsent1=c(0,length=intlength1)
  pa1=pb1=pc1=0
  
  q_totscore1=c(0,length=intlength1)
  q_frequency1=c(0,length=intlength1)
  q_avgsent1=c(0,length=intlength1)
  qa1=qb1=qc1=0
  
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    if((DB1$year[i]<SY) || (DB1$year[i]==SY && DB1$month[i]<SM) || (DB1$year[i]==FY && DB1$month[i]>FM) || (DB1$year[i]>FY))
    {
    }
    else
    {
      totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1+DB1$Sentiment.Score[i]
      a1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=b1+1
      b1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1/b1
      
      p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1+DB1$Price[i]
      pa1=p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pb1+1
      pb1=p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1/pb1
      
      q_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1+DB1$Quality[i]
      qa1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qb1+1
      qb1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1/qb1
      }
  }
  
  j=1
  for (j in 1:intlength1)
  {
    
    if(!is.na(avgsent1[j]) && avgsent1[j] > 1)
    {
      avgsent1[j]=NA
    }
  }
  
  print(avgsent1)
  
  
  intlength2=(FY-SY)*12+(FM-SM+1)
  totscore2=c(0,length=intlength2)
  frequency2=c(0,length=intlength2)
  avgsent2=c(0,length=intlength2)
  a2=b2=c2=0
  
  p_totscore2=c(0,length=intlength2)
  p_frequency2=c(0,length=intlength2)
  p_avgsent2=c(0,length=intlength2)
  pa2=pb2=pc2=0
  
  q_totscore2=c(0,length=intlength2)
  q_frequency2=c(0,length=intlength2)
  q_avgsent2=c(0,length=intlength2)
  qa2=qb2=qc2=0
  
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    if((DB2$year[i]<SY) || (DB2$year[i]==SY && DB2$month[i]<SM) || (DB2$year[i]==FY && DB2$month[i]>FM) || (DB2$year[i]>FY))
    {
    }
    else
    {
      totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2+DB2$Sentiment.Score[i]
      a2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=b2+1
      b2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2/b2
      
      p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2+DB2$Price[i]
      pa2=p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pb2+1
      pb2=p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2/pb2
      
      q_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2+DB2$Quality[i]
      qa2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qb2+1
      qb2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2/qb2
      }
  }
  
i = 1
  j=1
  for (j in 1:intlength2)
  {
    if(!is.na(avgsent2[j]) && avgsent2[j]>i)
    {
      avgsent2[j]=NA
    }
  }
  
  print(avgsent2)
  
  ###graph tot sentiment
  ts.plot(cbind(avgsent1, avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph price  
  #ts.plot(cbind(p_avgsent1, p_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph quality
  #ts.plot(cbind(q_avgsent1, q_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  
}



##############################################

###COMPARISON TIME SERIES FUNCTION
Price_Comparison <- function(DB1 = temp1, DB2 = temp2, fromdate, todate)
{
  
  division=c(" "," ")
  timevector=as.character(DB1$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB1$year[i]=time[[i]][3]
    DB1$month[i]=time[[i]][1]
    DB1$day[i]=time[[i]][2]
  }
  
  DB1$year=as.numeric(DB1$year)
  DB1$month=as.numeric(DB1$month)
  DB1$day=as.numeric(DB1$day)
  
  
  
  division=c(" "," ")
  timevector=as.character(DB2$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB2$year[i]=time[[i]][3]
    DB2$month[i]=time[[i]][1]
    DB2$day[i]=time[[i]][2]
  }
  
  DB2$year=as.numeric(DB2$year)
  DB2$month=as.numeric(DB2$month)
  DB2$day=as.numeric(DB2$day)
  
  
  
  #assumption for trying
  #fromdate
  #todate
  
  
  SY=2009
  SM=4
  FY=2013
  FM=1
  
  
  
  intlength1=(FY-SY)*12+(FM-SM+1)
  totscore1=c(0,length=intlength1)
  frequency1=c(0,length=intlength1)
  avgsent1=c(0,length=intlength1)
  a1=b1=c1=0
  
  p_totscore1=c(0,length=intlength1)
  p_frequency1=c(0,length=intlength1)
  p_avgsent1=c(0,length=intlength1)
  pa1=pb1=pc1=0
  
  q_totscore1=c(0,length=intlength1)
  q_frequency1=c(0,length=intlength1)
  q_avgsent1=c(0,length=intlength1)
  qa1=qb1=qc1=0
  
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    if((DB1$year[i]<SY) || (DB1$year[i]==SY && DB1$month[i]<SM) || (DB1$year[i]==FY && DB1$month[i]>FM) || (DB1$year[i]>FY))
    {
    }
    else
    {
      totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1+DB1$Sentiment.Score[i]
      a1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=b1+1
      b1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1/b1
      
      p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1+DB1$Price[i]
      pa1=p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pb1+1
      pb1=p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1/pb1
      
      q_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1+DB1$Quality[i]
      qa1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qb1+1
      qb1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1/qb1
    }
  }
  
  
  
  intlength2=(FY-SY)*12+(FM-SM+1)
  totscore2=c(0,length=intlength2)
  frequency2=c(0,length=intlength2)
  avgsent2=c(0,length=intlength2)
  a2=b2=c2=0
  
  p_totscore2=c(0,length=intlength2)
  p_frequency2=c(0,length=intlength2)
  p_avgsent2=c(0,length=intlength2)
  pa2=pb2=pc2=0
  
  q_totscore2=c(0,length=intlength2)
  q_frequency2=c(0,length=intlength2)
  q_avgsent2=c(0,length=intlength2)
  qa2=qb2=qc2=0
  
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    if((DB2$year[i]<SY) || (DB2$year[i]==SY && DB2$month[i]<SM) || (DB2$year[i]==FY && DB2$month[i]>FM) || (DB2$year[i]>FY))
    {
    }
    else
    {
      totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2+DB2$Sentiment.Score[i]
      a2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=b2+1
      b2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2/b2
      
      p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2+DB2$Price[i]
      pa2=p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pb2+1
      pb2=p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2/pb2
      
      q_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2+DB2$Quality[i]
      qa2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qb2+1
      qb2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2/qb2
    }
  }
  
  ###graph tot sentiment
  #ts.plot(cbind(avgsent1, avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph price  
  ts.plot(cbind(p_avgsent1, p_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph quality
  #ts.plot(cbind(q_avgsent1, q_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  
}


##############################################

###COMPARISON TIME SERIES FUNCTION
Quality_Comparison<- function(DB1 = temp1, DB2 = temp2, fromdate, todate)
{
  
  division=c(" "," ")
  timevector=as.character(DB1$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB1$year[i]=time[[i]][3]
    DB1$month[i]=time[[i]][1]
    DB1$day[i]=time[[i]][2]
  }
  
  DB1$year=as.numeric(DB1$year)
  DB1$month=as.numeric(DB1$month)
  DB1$day=as.numeric(DB1$day)
  
  
  
  division=c(" "," ")
  timevector=as.character(DB2$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB2$year[i]=time[[i]][3]
    DB2$month[i]=time[[i]][1]
    DB2$day[i]=time[[i]][2]
  }
  
  DB2$year=as.numeric(DB2$year)
  DB2$month=as.numeric(DB2$month)
  DB2$day=as.numeric(DB2$day)
  
  
  
  #assumption for trying
  #fromdate
  #todate
  
  
  SY=2009
  SM=4
  FY=2013
  FM=1
  
  
  
  intlength1=(FY-SY)*12+(FM-SM+1)
  totscore1=c(0,length=intlength1)
  frequency1=c(0,length=intlength1)
  avgsent1=c(0,length=intlength1)
  a1=b1=c1=0
  
  p_totscore1=c(0,length=intlength1)
  p_frequency1=c(0,length=intlength1)
  p_avgsent1=c(0,length=intlength1)
  pa1=pb1=pc1=0
  
  q_totscore1=c(0,length=intlength1)
  q_frequency1=c(0,length=intlength1)
  q_avgsent1=c(0,length=intlength1)
  qa1=qb1=qc1=0
  
  i=1
  for (i in 1:length(DB1$reviewTime))
  {
    if((DB1$year[i]<SY) || (DB1$year[i]==SY && DB1$month[i]<SM) || (DB1$year[i]==FY && DB1$month[i]>FM) || (DB1$year[i]>FY))
    {
    }
    else
    {
      totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1+DB1$Sentiment.Score[i]
      a1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=b1+1
      b1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=a1/b1
      
      p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1+DB1$Price[i]
      pa1=p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pb1+1
      pb1=p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      p_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1/pb1
      
      q_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1+DB1$Quality[i]
      qa1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qb1+1
      qb1=frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
      q_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1/qb1
    }
  }
  
  
  
  intlength2=(FY-SY)*12+(FM-SM+1)
  totscore2=c(0,length=intlength2)
  frequency2=c(0,length=intlength2)
  avgsent2=c(0,length=intlength2)
  a2=b2=c2=0
  
  p_totscore2=c(0,length=intlength2)
  p_frequency2=c(0,length=intlength2)
  p_avgsent2=c(0,length=intlength2)
  pa2=pb2=pc2=0
  
  q_totscore2=c(0,length=intlength2)
  q_frequency2=c(0,length=intlength2)
  q_avgsent2=c(0,length=intlength2)
  qa2=qb2=qc2=0
  
  i=1
  for (i in 1:length(DB2$reviewTime))
  {
    if((DB2$year[i]<SY) || (DB2$year[i]==SY && DB2$month[i]<SM) || (DB2$year[i]==FY && DB2$month[i]>FM) || (DB2$year[i]>FY))
    {
    }
    else
    {
      totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2+DB2$Sentiment.Score[i]
      a2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=b2+1
      b2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=a2/b2
      
      p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2+DB2$Price[i]
      pa2=p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pb2+1
      pb2=p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      p_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2/pb2
      
      q_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2+DB2$Quality[i]
      qa2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qb2+1
      qb2=frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
      q_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2/qb2
    }
  }
  
  ###graph tot sentiment
  #ts.plot(cbind(avgsent1, avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph price  
  #ts.plot(cbind(p_avgsent1, p_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  ###graph quality
  ts.plot(cbind(q_avgsent1, q_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
  
  
}



















##############################################

###OVERALL TIME SERIES FUNCTION
Overall_Series <- function(DB = temp, fromdate, todate)
{
  
  division=c(" "," ")
  timevector=as.character(DB$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB$year[i]=time[[i]][3]
    DB$month[i]=time[[i]][1]
    DB$day[i]=time[[i]][2]
  }
  
  DB$year=as.numeric(DB$year)
  DB$month=as.numeric(DB$month)
  DB$day=as.numeric(DB$day)
  
  
  
  #SM=month(fromdate)
  #FY=year(todate)
  #FM=month(todate)
  
  SY=2009
  SM=4
  FY=2013
  FM=1
  
  
  
  intlength=(FY-SY)*12+(FM-SM+1)
  totscore=c(0,length=intlength)
  frequency=c(0,length=intlength)
  avgsent=c(0,length=intlength)
  a=b=c=0
  
  p_totscore=c(0,length=intlength)
  p_frequency=c(0,length=intlength)
  p_avgsent=c(0,length=intlength)
  pa=pb=pc=0
  
  q_totscore=c(0,length=intlength)
  q_frequency=c(0,length=intlength)
  q_avgsent=c(0,length=intlength)
  qa=qb=qc=0
  
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    if((DB$year[i]<SY) || (DB$year[i]==SY && DB$month[i]<SM) || (DB$year[i]==FY && DB$month[i]>FM) || (DB$year[i]>FY))
    {
    }
    else
    {
      totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a+DB$Sentiment.Score[i]
      a=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=b+1
      b=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a/b
      
      p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa+DB$Price[i]
      pa=p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pb+1
      pb=p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa/pb
      
      q_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa+DB$Quality[i]
      qa=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qb+1
      qb=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa/qb
      }
  }
  
  
  ###graph tot sentiment
  total_sentiment=ts(avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(total_sentiment)
  
  ###graph price
  #price_sentiment=ts(p_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(price_sentiment)
  
  ###graph quality
  #quality_sentiment=ts(q_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  
  
  return(total_sentiment)
  #plot(quality_sentiment)
  
}
#################################
#END OF FUNCTION



##############################################
###PRICE TIME SERIES

Price_Series <- function(DB = temp, fromdate, todate)
{

  
  division=c(" "," ")
  timevector=as.character(DB$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB$year[i]=time[[i]][3]
    DB$month[i]=time[[i]][1]
    DB$day[i]=time[[i]][2]
  }
  
  DB$year=as.numeric(DB$year)
  DB$month=as.numeric(DB$month)
  DB$day=as.numeric(DB$day)
  
  
  
  SY=2009
  SM=4
  FY=2013
  FM=1
  
  
  
  intlength=(FY-SY)*12+(FM-SM+1)
  totscore=c(0,length=intlength)
  frequency=c(0,length=intlength)
  avgsent=c(0,length=intlength)
  a=b=c=0
  
  p_totscore=c(0,length=intlength)
  p_frequency=c(0,length=intlength)
  p_avgsent=c(0,length=intlength)
  pa=pb=pc=0
  
  q_totscore=c(0,length=intlength)
  q_frequency=c(0,length=intlength)
  q_avgsent=c(0,length=intlength)
  qa=qb=qc=0
  
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    if((DB$year[i]<SY) || (DB$year[i]==SY && DB$month[i]<SM) || (DB$year[i]==FY && DB$month[i]>FM) || (DB$year[i]>FY))
    {
    }
    else
    {
      totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a+DB$Sentiment.Score[i]
      a=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=b+1
      b=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a/b
      
      p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa+DB$Price[i]
      pa=p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pb+1
      pb=p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa/pb
      
      q_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa+DB$Quality[i]
      qa=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qb+1
      qb=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa/qb
      }
  }
  
  
  ###graph tot sentiment
  #total_sentiment=ts(avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(total_sentiment)
  
  ###graph price
  price_sentiment=ts(p_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(price_sentiment)
  
  ###graph quality
  #quality_sentiment=ts(q_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  
  
  return(price_sentiment)
  #plot(quality_sentiment)
  
  
  
}
#################################
#END OF FUNCTION



##############################################
###Quality TIME SERIES

Quality_Series <- function(DB = temp, fromdate, todate)
{
 
  division=c(" "," ")
  timevector=as.character(DB$reviewTime)
  time=strsplit(timevector,division,fixed=TRUE)
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    time[[i]][2]=gsub(",","",time[[i]][2])
    DB$year[i]=time[[i]][3]
    DB$month[i]=time[[i]][1]
    DB$day[i]=time[[i]][2]
  }
  
  DB$year=as.numeric(DB$year)
  DB$month=as.numeric(DB$month)
  DB$day=as.numeric(DB$day)
  
  
  ####################
  #TIME SERIES CODE
  ####################
  
  #assumption for trying
  #fromdate
  #todate
  
  
  
  
  SY=2009
  SM=4
  FY=2013
  FM=1
  
  
  
  intlength=(FY-SY)*12+(FM-SM+1)
  totscore=c(0,length=intlength)
  frequency=c(0,length=intlength)
  avgsent=c(0,length=intlength)
  a=b=c=0
  
  p_totscore=c(0,length=intlength)
  p_frequency=c(0,length=intlength)
  p_avgsent=c(0,length=intlength)
  pa=pb=pc=0
  
  q_totscore=c(0,length=intlength)
  q_frequency=c(0,length=intlength)
  q_avgsent=c(0,length=intlength)
  qa=qb=qc=0
  
  i=1
  for (i in 1:length(DB$reviewTime))
  {
    if((DB$year[i]<SY) || (DB$year[i]==SY && DB$month[i]<SM) || (DB$year[i]==FY && DB$month[i]>FM) || (DB$year[i]>FY))
    {
    }
    else
    {
      totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a+DB$Sentiment.Score[i]
      a=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=b+1
      b=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=a/b
      
      p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa+DB$Price[i]
      pa=p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pb+1
      pb=p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      p_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa/pb
      
      q_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa+DB$Quality[i]
      qa=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qb+1
      qb=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
      q_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa/qb
      }
  }
  
  
  ###graph tot sentiment
  #total_sentiment=ts(avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(total_sentiment)
  
  ###graph price
  #price_sentiment=ts(p_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  #plot(price_sentiment)
  
  ###graph quality
  quality_sentiment=ts(q_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
  
  
  return(quality_sentiment)
  #plot(quality_sentiment)
  
  
  
}
#################################
#END OF FUNCTION
























################################
###############################
source("HMWK#1.R")
library(shiny)

# Define server logic
shinyServer(function(input, output) {
  
  
##############################################################################  
  #BEGIN
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
##############################################################################
  
  output$Sentiplot <- renderPlot({
  
  # temp <- read.csv("Apparel_filtered_data.csv")
  
   #dataset <- read.csv("output-2.csv")
     
   #SentiCols3 = c("Industry", "Category", "brand","reviewTime","Sentiment.Score", "salesRank", "overall", "Price", "Quality")
   
   #Data_Sentiments3 = data.frame(Industry = dataset["Industry"], Category = dataset["Category"], Brand = dataset["brand"], reviewTime = dataset["reviewTime"], SentimentScore = dataset["Sentiment.Score"], SalesRank = dataset["salesRank"], Overall = dataset["overall"], Price = dataset["Price"], Quality = dataset["Quality"])
   #colnames(Data_Sentiments3) <- SentiCols3
   

   #s <- ggplot(aes(x = Data_Sentiments$Time, y = Data_Sentiments$SentimentScore), data = Data_Sentiments) + geom_line()
    
   #print(s)
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)

      c1_inds_op1 <- reactive(input$c1_inds_op1)
      c1_inds_op2 <- reactive(input$c2_inds_op1)
      
      
     data_c1_inds_op1_sub = reactive({
                          a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
                          a <- droplevels(a)
                          return(a)
                         })
     data_c1_inds_op1 <- data_c1_inds_op1_sub()
         
    
     data_c1_inds_op2_sub = reactive({
                          a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
                          a <- droplevels(a)
                          return(a)
                                    })
     data_c1_inds_op2 <- data_c1_inds_op2_sub()
      
     
    #s <- ggplot(aes(x = data_c1_inds_op2$Time, y = data_c1_inds_op2$SentimentScore), data = data_c1_inds_op2) + geom_line()  
     
    #print(s)
     #plot(data_c1_inds_op2$SentimentScore)
     #q <- SentimentTimeSeries(data_c1_inds_op2, fromdate, todate)
     
     #plot(q)
     
     Overall_Comparison(data_c1_inds_op1, data_c1_inds_op2, fromdate, todate)
     
     
    }
    
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(data_c2_inds_op1, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(data_c2_inds_op1, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
    
      Overall_Comparison(data_c2_inds_op1, data_c2_inds_op2, fromdate, todate)
      

    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      Overall_Comparison(data_c2_s2_cat_op1, data_c2_s2_cat_op2, fromdate, todate)
      
    }
    
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      q <- Overall_Series(data_c2_s3_cat, fromdate, todate)
      
      plot(q)
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(data_c2_inds_op1, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      q <- Overall_Series(data_c2_s3_brand, fromdate, todate)
      
      plot(q)
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(data_c2_s3_cat, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
      q <- Overall_Series(data_c2_s3_brand, fromdate, todate)
      
      plot(q)
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      Overall_Comparison(data_c3_brand_op1,data_c3_brand_op2, fromdate, todate)
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(data_c4_brand, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(data_c4_brand, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
      
      Overall_Comparison(data_c4_s1_cat_op1, data_c4_s1_cat_op2, fromdate, todate)
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(data_c4_brand, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
      q <- Overall_Series(data_c4_s3_cat, fromdate, todate)
      
      plot(q)
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      q <- Overall_Series(data_c4_brand, fromdate, todate)
      
      plot(q)
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
})
  
  
  ##############################################################################  
  #END
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  
  ##############################################################################  
  #BEGIN - DIMENSION ANALYSIS
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$PricePlot <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c1_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op1 <- data_c1_inds_op1_sub()
      
      
      data_c1_inds_op2_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op2 <- data_c1_inds_op2_sub()
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      q <- PriceTimeSeries(data_c2_s3_cat, fromdate, todate)
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
  })
  
  
  
  
  output$QualityPlot <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c1_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op1 <- data_c1_inds_op1_sub()
      
      
      data_c1_inds_op2_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op2 <- data_c1_inds_op2_sub()
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(data_c2_inds_op1, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      q <- PriceTimeSeries(data_c2_s3_cat, fromdate, todate)
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
  })
  
  
  
  
  
  
  
  ##############################################################################  
  #END - DIMENSION ANALYSIS
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  ##############################################################################  
  #BEGIN - WORD CLOUD
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$WordCloud <- renderPlot({
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c1_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op1 <- data_c1_inds_op1_sub()
      
      
      data_c1_inds_op2_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op2 <- data_c1_inds_op2_sub()
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
  })
  
  ##############################################################################  
  #END - WORD CLOUD
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
    
  ##############################################################################  
  #BEGIN - SALES RANK
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  output$SalesRank <- renderPlot({
  
    x <- c(1:10)
    plot(x)
    
    #################    
    #CHOICE ONE
    #################           
    if(input$main_radioselection == 'Comparison of Two Industries'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c1_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op1 <- data_c1_inds_op1_sub()
      
      
      data_c1_inds_op2_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c1_inds_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c1_inds_op2 <- data_c1_inds_op2_sub()
      
      
    }
    #################    
    #CHOICE TWO
    #################    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two brands'){
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s1_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s1_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s1_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s1_brand_op2 <- data_c2_s1_brand_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Comparison' & input$c2_radioselection_s1 == 'Comparison between two categories'){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s2_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op1 <- data_c2_s2_cat_op1_sub()
      
      
      data_c2_s2_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s2_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s2_cat_op2 <- data_c2_s2_cat_op2_sub()
      
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of an Industry' & input$c2_radioselection == 'Analysis' & input$c2_checkbox2_cat == TRUE & input$c2_checkbox2_brand == TRUE) {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c2_inds_op1_sub = reactive({
        a <- subset(Data_Sentiments, Industry %in% input$c2_inds_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c2_inds_op1 <- data_c2_inds_op1_sub()
      
      
      data_c2_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c2_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_cat <- data_c2_s3_cat_sub()
      
      
      
      data_c2_s3_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c2_s3_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c2_s3_brand <- data_c2_s3_brand_sub()
      
      
    }
    
    #################    
    #CHOICE THREE
    #################    
    else if (input$main_radioselection == 'Comparison of Two Brands') {
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c3_brand_op1_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op1 <- data_c3_brand_op1_sub()
      
      
      data_c3_brand_op2_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c3_brand_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c3_brand_op2 <- data_c3_brand_op2_sub()
      
      
      
      
    }
    
    #################    
    #CHOICE FOUR
    #################    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Comparison') {
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s1_cat_op1_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op1)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op1 <- data_c4_s1_cat_op1_sub()
      
      
      data_c4_s1_cat_op2_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s1_cat_op2)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s1_cat_op2 <- data_c4_s1_cat_op2_sub()
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == TRUE){
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
      data_c4_s3_cat_sub = reactive({
        a <- subset(Data_Sentiments, Category %in% input$c4_s3_cat)
        a <- droplevels(a)
        return(a)
      })
      data_c4_s3_cat <- data_c4_s3_cat_sub()
      
      
    }
    
    else if (input$main_radioselection == 'Analysis of a Brand' & input$c4_radioselection == 'Analysis' & input$c4_s2_cat == FALSE){
      
      
      fromdate <- reactive(input$datefrom)
      todate <- reactive(input$dateto)
      
      data_c4_brand_sub = reactive({
        a <- subset(Data_Sentiments, Brand %in% input$c4_brand)
        a <- droplevels(a)
        return(a)
      })
      data_c4_brand <- data_c4_brand_sub()
      
      
    }
    
    #################    
    #CATCH ERROR
    #################    
    else {}
    
    
    #END OF RENDER PLOT
    
      
  })
  
  ##############################################################################  
  #END - SALES RANK
  # Output for the main panel (and the side panel for the confidence interval)
  # Plot output
  ##############################################################################
  
  
    
})
