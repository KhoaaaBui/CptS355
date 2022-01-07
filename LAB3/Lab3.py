# CptS 355 - Fall 2021 - Lab 3
# Khoa Bui - WSU ID: 11685409

debugging = False
def debug(*s): 
     if debugging: 
          print(*s)

## problem 1 getNumCases 
def getNumCases(data, counties, months):
     totalCases = 0
     for county, log in data.items():
          # county exist in given counties list
          if county in counties:
               for month, cases in log.items():
                    if month in months:
                         totalCases += cases
     return totalCases


## problem 2 getMonthlyCases
def getMonthlyCases(data):
     dictionary = {}
     for county, log in data.items():
          for month, cases in log.items():
               # if month already in dictionary, add county:cases to dictionary[month]
               if month in dictionary:
                    dictionary[month][county] = cases
               # else add month with and empty dictionary inside
               else:
                    dictionary[month] = {}
                    dictionary[month][county] = cases
     return dictionary


from functools import reduce
## problem 3 mostCases 
def mostCases(data):
     # reformat data
     monthlyCases = getMonthlyCases(data)

     # get total cases per month
     def getTotal(val):
          return reduce ((lambda x, y : x + y ), val.values())
          #                           month,         log
     totalMonthly = map ((lambda x : (x[0], getTotal(x[1]))), monthlyCases.items())

     # get month that has maximum number of cases
     #                                        get the                   cast to list
     maxMonthly = reduce ((lambda x, y : x if x[1] > y[1] else y), list(totalMonthly))
     return maxMonthly


## problem 4a) searchDicts(L,k)
def searchDicts(L, k):
     # start from the end of the list
     for d in reversed(L):
          if k in d:
               return d[k]
     return None


## problem 4b) searchDicts2(L,k)
def searchDictsHelper(tL, k, i):
     # search the dictionary at the index i
     if k in tL[i][1]:
          return tL[i][1][k]
     # reach the start of the list
     elif i == 0:
          return None
     else:
          # recursively call the next dictionary
          return searchDictsHelper(tL, k, tL[i][0])

def searchDicts2(tL, k):
     return searchDictsHelper(tL, k, len(tL)-1)


## problem 5 - getLongest
def getLongest(L):
     return (getLongestHelper(L, longest = ""))
def getLongestHelper(L, longest):
     for ele in L:
          # if element is not a list, compare length
          if type(ele) != list:
               if len(longest) < len(ele):
                    longest = ele
          # if element is a list, call recursive function
          else:
               longest = getLongestHelper(ele, longest)
     return longest
## problem 6 - apply2nextN 