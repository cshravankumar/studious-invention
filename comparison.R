####################
#TIME SERIES FOR COMPARISON
####################



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
    avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]/frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    
    p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pa1+DB1$Price[i]
    pa1=p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=pb1+1
    pb1=p_frequency1[(DB$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    p_avgsent1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=p_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]/p_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    
    q_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qa1+DB1$Quality[i]
    qa1=totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    q_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]=qb1+1
    qb1=frequency1[(DB$year[i]-SY)*12+(DB1$month[i]-SM+1)]
    q_avgsent1[(DB$year[i]-SY)*12+(DB1$month[i]-SM+1)]=q_totscore1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]/q_frequency1[(DB1$year[i]-SY)*12+(DB1$month[i]-SM+1)]
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
    avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]/frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    
    p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pa2+DB2$Price[i]
    pa2=p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=pb2+1
    pb2=p_frequency2[(DB$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    p_avgsent2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=p_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]/p_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    
    q_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qa2+DB2$Quality[i]
    qa2=totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    q_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]=qb2+1
    qb2=frequency2[(DB$year[i]-SY)*12+(DB2$month[i]-SM+1)]
    q_avgsent2[(DB$year[i]-SY)*12+(DB2$month[i]-SM+1)]=q_totscore2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]/q_frequency2[(DB2$year[i]-SY)*12+(DB2$month[i]-SM+1)]
  }
}

###graph tot sentiment
ts.plot(cbind(avgsent1, avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))

###graph price  
ts.plot(cbind(p_avgsent1, p_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))

###graph quality
ts.plot(cbind(q_avgsent1, q_avgsent2), gpars=list(xlab="time", ylab="total_sentiment", lty=c(1:5)))
