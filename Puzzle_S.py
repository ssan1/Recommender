#!/usr/bin/env python

def swapfunction (afterfirstswap,x,y):
    splitstr=afterfirstswap.split()
    newstr=""
    temp1=""
    temp2=""
    if(x<y):
        for i in range (0,len(afterfirstswap)):
               
                if(i==x):
                    temp1=splitstr[0][i]
                    newstr=newstr+"_"
                elif(i==x+1):
                   
                    temp2=splitstr[0][i]
                    newstr=newstr+"_"
                elif(i==y):
                   
                    newstr=newstr+temp1
                elif(i==y+1):
                    newstr=newstr+temp2
                else:
                    newstr=newstr+splitstr[0][i]
    else:
        for i in range (0,len(afterfirstswap)):
               
                temp1=splitstr[0][x]
                temp2=splitstr[0][x+1]
                if(i==x):
                    newstr=newstr+"_"
                elif(i==x+1):
                    newstr=newstr+"_"
                elif(i==y):
                    newstr=newstr+temp1
                elif(i==y+1):
                    newstr=newstr+temp2
                else:
                    newstr=newstr+splitstr[0][i]
           
    return newstr
          
   

def main():
    original = "HHHHHTTTTT"
    position = "0123456789"
    print "string:  "+original
    print "position:"+position
    x=input("Enter position number of left hand side of pair:") #first time only this should prompt
   
    afterfirstswap = original[:x] + "__" + original[x+2:]+original[x:x+2]
    print afterfirstswap

    for attempt in range(0,5):
        x=input("Enter position number of left hand side of pair:") #first time only this should prompt
        y=input("Enter position number of left hand side of destination position:")
        afterfirstswap=swapfunction(afterfirstswap,x,y)
        print afterfirstswap
        if(afterfirstswap=="HTHTHTHTHT__" or afterfirstswap=="THTHTHTHTH__" or afterfirstswap=="__HTHTHTHTHT"):
            print "you won!!"
main()
