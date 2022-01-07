# CptS 355 - Fall 2021 - Assignment 3
# Please include your name and the names of the students with whom you discussed any of the problems in this homework
# Khoa Bui - 11685409

debugging = True
def debug(*s): 
     if debugging: 
          print(*s)

my_cats_log =  {(2,2019):{"Oceanfish":7, "Tuna":1, "Whitefish":3, "Chicken":4, "Beef":2},
                (5,2019):{"Oceanfish":6, "Tuna":2, "Whitefish":1, "Salmon":3, "Chicken":6},
                (9,2019):{"Tuna":3, "Whitefish":3, "Salmon":2, "Chicken":5, "Beef":2, "Turkey":1, "Sardines":1},
                (5,2020):{"Whitefish":5, "Sardines":3, "Chicken":7, "Beef":3},
                (8,2020):{"Oceanfish":3, "Tuna":2, "Whitefish":2, "Salmon":2, "Chicken":4, "Beef":2, "Turkey":1},
                (10,2020):{"Tuna":2, "Whitefish":2, "Salmon":2, "Chicken":4, "Beef":2, "Turkey":4, "Sardines":1},
                (12,2020):{"Chicken":7,"Beef":3, "Turkey":4, "Whitefish":1, "Sardines":2},
                (4,2021):{"Salmon":2,"Whitefish":4, "Turkey":2, "Beef":4, "Tuna":3, "MixedGrill": 2}, 
                (5,2021):{"Tuna":5,"Beef":4, "Scallop":4, "Chicken":3}, 
                (6,2021):{"Turkey":2,"Salmon":2, "Scallop":5, "Oceanfish":5, "Sardines":3}, 
                (9,2021):{"Chicken":8,"Beef":6},                 
                (10,2021):{ "Sardines":1, "Tuna":2, "Whitefish":2, "Salmon":2, "Chicken":4, "Beef":2, "Turkey":4} }

p1a_output = {2019: {'Oceanfish': 13, 'Tuna': 6, 'Whitefish': 7, 'Chicken': 15, 'Beef': 4, 'Salmon': 5, 'Turkey': 1, 'Sardines': 1}, 
              2020: {'Whitefish': 10, 'Sardines': 6, 'Chicken': 22, 'Beef': 10, 'Oceanfish': 3, 'Tuna': 4, 'Salmon': 4, 'Turkey': 9}, 
              2021: {'Salmon': 6, 'Whitefish': 6, 'Turkey': 8, 'Beef': 16, 'Tuna': 10, 'MixedGrill': 2, 'Scallop': 9, 'Chicken': 15, 'Oceanfish': 5, 'Sardines': 4}}


## problem 1(a) merge_by_year - 10%
def merge_by_year(feeding_log):
     if feeding_log == {}:
          return {}

     dictionary = {}
     
     # time stamp : {key:value}
     for time, log in feeding_log.items():
          # if year is in dictionary, combine all the logs of same year
          if time[1] in dictionary:
               for key in log:
                    # if key in the log is found in dictionary, add the can number
                    if key in dictionary[time[1]]:
                         dictionary[time[1]][key] += log[key]
                    else:
                         dictionary[time[1]][key] = log[key]
          # if year is not in dictionary, add 
          else:
               dictionary[time[1]] = log
     return dictionary

#debug(merge_by_year(my_cats_log))

from functools import reduce

## problem 1(b) merge_year - 15%
def combine_dicts(d1, d2):
          common_elements = map(lambda x : (x[0], x[1] + d2.get(x[0],0)),d1.items())
          other_elements = filter(lambda y : y[0] not in d1, d2.items())
          return dict(sorted(list(other_elements) + list(common_elements)))

def merge_year(feeding_log, year):
     if feeding_log == {}:
          return {}
     # filter out logs that has the needed year
     def getYear(data):
          yearLog = filter ((lambda x : True if x[0][1] == year else False), data.items())
          yearList = dict(yearLog).values()
          return yearList
     yearMix = reduce((lambda x, y :  combine_dicts(x, y)), getYear(feeding_log))
     return yearMix
#debug(merge_year(my_cats_log, 2021))
## problem 1(c) getmax_of_flavor - 15%
def getmax_of_flavor(feeding_log, flavor):
     if feeding_log == {}:
          return ((0,0),0)
     # log that has the flavor
     flavorLogs = filter((lambda x : flavor in x[1]), feeding_log.items())
     # get the log with the max number of flavor can
     maxFlavor = list(reduce((lambda x, y : x if x[1][flavor] >= y[1][flavor] else y), list(flavorLogs)))
     return (maxFlavor[0],maxFlavor[1][flavor])

#debug(getmax_of_flavor(my_cats_log, "Beef"))
graph = {'A':{'B','C','D'},'B':{'C'},'C':{'B','E','F','G'},'D':{'A','E','F'},'E':{'F'}, 'F':{'E', 'G'},'G':{}, 'H':{'F','G'}}
## problem 2(a) follow_the_follower - 10%
def follow_the_follower(graph):
     result = []
     # if graph is empty
     if graph == {}:
          return []
     for start, ends in graph.items():
          for node in ends:
               if start in graph[node]:
                    result.append((start, node))
     return result
#debug(follow_the_follower(graph))
## problem 2(b) follow_the_follower2 - 6%
def follow_the_follower2(graph):
     result = []
     if graph == {}:
          return []
     for start, ends in graph.items():
          newList = [(start, node) for node in ends if start in graph[node]]
          result += newList
     return result
# debug(follow_the_follower2(graph))
## problem 3 - connected - 15%
def connected(graph, node1, node2):
     if graph == {}:
          return False
     # if both nodes are connected with each other, return True
     if (node1, node2) in follow_the_follower(graph):
          return True
     else:
          return connectedHelper(graph, node1, node2)

def connectedHelper(graph, node1, node2):
     for node in graph[node1]:
          # if node2 is found, return True
          if node == node2:
               return True
          else:
               # if node1 and node is not a loop
               if (node1, node) not in follow_the_follower(graph):
                    return connectedHelper(graph, node, node2)
     return False
debug(connected(graph, 'A', 'F'))
debug(connected(graph, 'E', 'A'))
debug(connected(graph, 'A', 'H'))
debug(connected(graph, 'H', 'E'))
## problem 4(a) - lazy_word_reader - 20%
class lazy_word_reader():
     # constructor that takes a file name
     def __init__(self, name):
          self.file = open(name)
          # store read line
          self.line = []
     def __iter__(self):
          return self
     def __next__(self):
          # if line is empty, get new line
          if self.line == []:
               word = self.file.readline()
               # while not end of file
               while word != "":
                    # if empty line, read the next line
                    if word == '\n':
                         word = self.file.readline()
                    # split string to list and return a word
                    else:
                         self.line = word.split()
                         return self.line.pop(0)
               raise StopIteration
          return self.line.pop(0)
          
               

#mywords = lazy_word_reader("testfile.txt")
#print(mywords.__next__())
#print(mywords.__next__())
#print(mywords.__next__())
#for word in mywords:
 #    print(word)
         
## problem 4(b) - word_histogram - 3%
def word_histogram(words):
     d = {}
     for word in words:
          if word in d.keys():
               d[word] += 1
          else:
               d[word] = 1
     return sorted(sorted(list(d.items())), key = lambda item: item[1], reverse = True)

#debug(word_histogram(lazy_word_reader("testfile.txt")))