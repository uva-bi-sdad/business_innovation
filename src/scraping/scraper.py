# -*- coding: utf-8 -*-
"""
Created on Tue May 28 15:36:13 2019
Basic Scraper Utility
@author: Neil K

"""

from bs4 import BeautifulSoup
import requests
import time

url="https://www.drugs.com"

result=requests.get("https://www.drugs.com/newdrugs.html")

soup = BeautifulSoup(result.content,'html.parser')

linkdiv=soup.find("div",class_="newsArchiveLinks")

links=linkdiv.find_all("a")

fdalinks=[]

dates=["2013","2014","2015","2016"]

for address in links:
    testval=address['href'].split("/")[-1]

    
    if any(x in testval for x in dates):
        #print(testval)
        drugdata={"name":testval,"url":url+address['href']}
        fdalinks.append(drugdata)

#print(fdalinks)
finaldata={}
for i in fdalinks:
    time.sleep(5)
    print("Now in "+i["name"])
    soup=BeautifulSoup(requests.get(i["url"]).content,'html.parser')
    datadivs=soup.find_all("div",class_="newsItem")
    print("length is "+str(len(datadivs)))
    for each in datadivs:
        drugtext=each.find("h3").text
        print(drugtext)
        drugname=drugtext.split("(")[0]
        formula=drugtext.split("(")[1].split(")")[0]
        if(drugname not in finaldata.keys()):
            finaldata[drugname]=formula
        
print(finaldata)        
        
    

    
