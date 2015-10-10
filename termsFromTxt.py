#!/usr/bin/python

# Nota: to source this file from python interpreter:
# from termsFromTxt import *
# #  and to reload:
# reload(termsFromTxt) ; from termsFromTxt import *
# need pyGTrends.py in same directory to run
# if using python, will not work on versiosn higher than 2

import sys
sys.path.append("google-trend-api")
from pyGTrends import pyGTrends
import csv, datetime, time, getpass, sys
import re
import os,glob # for files managements

# parameters
requestsFolder = "requestsListings" # where to get the requests to make
outFolder = "requestsResults" # where to put the results from google trends
prefixInput = "request" # additional prefix of the requests files

# get google id
google_username = raw_input("Google username: ")
google_password = getpass.getpass("Google password: ")

# get timestamp
ts = time.time()
timeStamp = datetime.datetime.fromtimestamp(ts).strftime('%Y-%m-%d_%H-%M-%S')

def read_csv_data( data ):
    """
        Reads CSV from given path and Return list of dict with Mapping
    """
    csv_reader = csv.reader( data )
    # Read the column names from the first line of the file
    fields = csv_reader.next()
    data_lines = []
    for row in csv_reader:
        items = dict(zip(fields, row))
        data_lines.append(items)
    return data_lines

def progressbar(it, prefix = "", size = 60):
    count = len(it)
    def _show(_i):
        x = int(size*_i/count)
        sys.stdout.write("%s[%s%s] %i/%i\r" % (prefix, "#"*x, "."*(size-x), _i, count))
        sys.stdout.flush()
    
    _show(0)
    for i, item in enumerate(it):
        yield item
        _show(i+1)
    sys.stdout.write("\n")
    sys.stdout.flush()


def getGTData( search_query = "debt", date="all", geo="all", scale="1", position = "end" ) :

  connector = pyGTrends( google_username, google_password )
  connector.download_report( ( search_query ) 
      		   , date = date
                         , geo = geo
                         , scale = scale )
  connector.writer()
  # print connector.getData()
  # d = DictReader( connector.getData().split('\n'))
  # print  csv.DictReader( connector.csv().split('\n'))
  # print  connector.csv( section='Main', as_list=True )
  # data = connector.csv( section='Main' ).split('\n')
  # csv_reader = csv.reader( data )
  return connector.getData()

  # example using City to cache search terms
  #csv_reader = csv.reader( connector.csv( section='City' ).split('\n') )

def getGTDataWrite(search_query = "debt", date="all", geo="all", position = "end", scale = "1" ) :

    outTrends = getGTData(search_query = search_query, date = date, geo = geo, scale = scale )

    # remove all whitespaces in query term
    search_query = search_query.strip() 
    search_query = " ".join( search_query.split() )
    search_query = search_query.replace(" ", "") 
        
    # save in csv file named after the query
    fileName = search_query + '_google_report.csv'
    f = open( fileName, 'w')
    f.write(outTrends)
    print "File saved: %s " % ( fileName )
        
def getGoogleTrendData( search_queries = ["debt"], date="all", geo="all", scale="1" ) :
  for search_term in progressbar( search_queries, "Downloading: ", 40 ):
      getGTDataWrite(search_query = search_term, date = date, geo = geo, scale = scale )
      time.sleep(2)  # Delay for x seconds    
  return True


if __name__=="__main__":
  
  # get the request files list
  requestFiles = glob.glob(requestsFolder + "/"+ prefixInput + "*.txt")
  outFolder = outFolder + "_" + timeStamp

  # check the outFolder is present, if not create it
  try:
    os.stat(outFolder)
  except:
    os.mkdir(outFolder)       
  
  for inputFile in progressbar(requestFiles,"Downloading:",40) :
    ### get the queries
    f = open(inputFile)
    list_of_queries = f.read().rstrip().split("\n")
    f.close()
  
    ### look at queries separatly
    # # Remove duplicate entries in the list if there are any...
    # list_of_queries = list( set( list_of_queries ) )
    # if getGoogleTrendData( search_queries = list_of_queries, date="all", geo="US", scale="1" ) :
    #     print "Google Trend Data aquired."
    
    ### look at queries simultanuously
    outTrends = getGTData(search_query = list_of_queries, date = "all", geo = "US", scale = "1" )
  
    ### save the result
    # make name
    inName = inputFile.rsplit(".txt")[0].rsplit(prefixInput)[2]
    fileName = outFolder + "/" + inName + '_google_report.csv'
  
    # check outFolder exists
    f = open( fileName, 'w')
    f.write(outTrends)
    print "File saved: %s " % ( fileName )
    time.sleep(2)  # Delay for x seconds
