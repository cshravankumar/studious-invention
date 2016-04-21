
SentimentTimeSeries <- function(DB = data.frame(Industry = dataset["Industry"], Category = dataset["Category"], Brand = dataset["Brand"], ReviewTime = dataset["reviewTime"], Sentiment.Score = dataset["Sentiment.Score"], reviewTime = as.Date(0,origin="1970-01-01"), SalesRank = dataset["SalesRank"], Overall = dataset["overall"], Price = dataset["Price"], Quality = dataset["Quality"]))
{
####################
#TIME SERIES INTRO
####################


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
fromdate
todate




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
    avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]/frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    
    p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pa+DB$Price[i]
    pa=p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=pb+1
    pb=p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    p_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=p_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]/p_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    
    q_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qa+DB$Quality[i]
    qa=totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    q_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=qb+1
    qb=frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
    q_avgsent[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]=q_totscore[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]/q_frequency[(DB$year[i]-SY)*12+(DB$month[i]-SM+1)]
  }
}


###graph tot sentiment
total_sentiment=ts(avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
plot(total_sentiment)

###graph price
price_sentiment=ts(p_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)
plot(price_sentiment)

###graph quality
quality_sentiment=ts(q_avgsent, start=c(SY, SM), end=c(FY, FM), frequency=12)


return(quality_sentiment)
#plot(quality_sentiment)



}