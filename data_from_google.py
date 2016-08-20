
import urllib, time, os, re, csv
 
def fetchGF(googleticker):
    url="http://www.google.com/finance?&q="
    txt=urllib.urlopen(url+googleticker).read()
    #print(txt)
    k=re.search('id="ref_(.*?)">(.*?)<',txt)
    if k:
        tmp=k.group(2)
        q=tmp.replace(',','')
    else:
        q="Nothing found for: "+googleticker
    return q

print(time.ctime())
print
 
# Set local time zone to NYC
os.environ['TZ']='America/New_York'
time.tzset()
t=time.localtime() # string
print(time.ctime())
print


def combine(ticker):
    quote=fetchGF(ticker) # use the core-engine function
    t=time.localtime()    # grasp the moment of time
    output=[t.tm_year,t.tm_mon,t.tm_mday,t.tm_hour,  # build a list
            t.tm_min,t.tm_sec,ticker,quote]
    return output


ticker="INDEXSP%3A.INX" # You should choose the name of stock/index given in google finance


fname="new_d_i"
# remove a file, if exist
os.path.exists(fname) and os.remove(fname)


freq=1 # This the frequency of data flow 


with open(fname,'a') as f:
    writer=csv.writer(f,dialect="excel") #,delimiter=" ")
    while(t.tm_hour<=16):
        if(t.tm_hour==16):
            while(t.tm_min<01):
                #for ticker in tickers:
                    data=combine(ticker)
                    print(data)
                    writer.writerow(data)
                    time.sleep(freq)
            else:
                break
        else:
            data=combine(ticker)
            print(data)
            writer.writerow(data)
            time.sleep(freq)
 
f.close()

