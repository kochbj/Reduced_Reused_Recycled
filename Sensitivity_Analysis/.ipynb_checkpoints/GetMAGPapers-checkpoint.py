import os
import pandas as pd
import numpy as np
"""
Test Module
"""
import string
import os
import sys
import json
import requests
import pandas as pd
from os.path import join, dirname
from random import randint
from queue import Queue
from threading import Thread
from time import sleep
from dotenv import load_dotenv
from optparse import IndentedHelpFormatter, OptionGroup, OptionParser
from copy import deepcopy
dotenv_path = join(dirname(__file__), 'env.data')
load_dotenv(dotenv_path)
try:
    import maka
except ImportError:
    import inspect
    CURRENT_DIR = os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe())))
    PARENT_DIR = os.path.dirname(CURRENT_DIR)
    os.sys.path.insert(0, PARENT_DIR)
    import maka

RESULTS={}
H_INDICES={}
DELAY = 1
NUM_QUERIER_THREADS = 50
THE_QUEUE = Queue()
COMPLETED={}
removed_papers=[]

import pandas as pd
import json


#text_data=pd.read_csv('/home/bkoch/Projects/DataProject/analyses/04-PWC/PWC_2021_06_16/mag_failed_link.txt',sep='\t')
pwc_Titles=pd.read_csv('/home/bkoch/Projects/DataProject/analyses/04-PWC/MAGCode/ForEmilyV2/face_recognition_MAGIDs.txt',sep='\t')

def querier_enclosure(i, q):
    """
    Wrapper for the query procedure in order to be used in a Worker
    """
    while True:
        print('Worker {}: Looking for the next query'.format(i))
        args = q.get()
        query = maka.AcademicQuerier(args['query_type'], args['payload'])
        completed_papers=os.listdir('/home/bkoch/Projects/DataProject/analyses/04-PWC/MAGCode/ForEmilyV2/Results/')
        output_name=args['data']['PWC_Clean_Title'].strip('\'').replace(' ','_').replace('/','_')+'.'+str(args['data']['PaperId'])+'.json'
        if output_name in completed_papers:
            q.task_done()
            continue
        print(query)
        if query is not None:
            try:
                results = query.post()
            except:
                q.task_done()
                sleep(DELAY)
                continue
            #print("RESULTS",results)
            if len(results)==0:
                q.task_done()
                sleep(DELAY)
                continue
            with open('/home/bkoch/Projects/DataProject/analyses/04-PWC/MAGCode/ForEmilyV2/Results/{}'.format(output_name),'w') as f:
                dump_file=query.raw
                dump_file['title']=args['data']['PWC_Title']
                dump_file['clean_title']=args['data']['PWC_Clean_Title']
                json.dump(dump_file,f,indent=4)
                q.task_done()
                sleep(DELAY)
                continue
        q.task_done()
        sleep(DELAY)

def main():
    
    for i in range(NUM_QUERIER_THREADS):
        worker = Thread(target=querier_enclosure, args=(i, THE_QUEUE))
        worker.setDaemon(True)
        worker.start()
    completed_papers=os.listdir('/home/bkoch/Projects/DataProject/analyses/04-PWC/MAGCode/ForEmilyV2/Results/')
    
    for i,row in pwc_Titles.iterrows():
        #print(row['PWC_Clean_Title'],row['PWC_Title'])
        output_name=row['PWC_Clean_Title'].strip('\'').replace(' ','_').replace('/','_')+'.json'
        if output_name in completed_papers:
            print("SKIPPING")
            continue
        THE_QUEUE.put({
            'query_type': maka.AcademicQueryType.EVALUATE,
            'payload': {
            'expr':"And(Id={0})".format(row['PaperId']),
        'attributes':'Y,AA.DAuN,CC,ECC,C.CN,C.CId,Id,Ti,J.JN,J.JId,AA.AuN,AA.AuId,DOI,AA.S,AA.AfId,AA.AfN,S,F.FN,F.FId,F.DFN,FamId,RId',
            'count':2,
            'timeout': 1000,       
            },
            'data': row
    })
    print('*** Main thread waiting')
    THE_QUEUE.join()
    print('*** Done')

if __name__ == "__main__":
    sys.exit(main())
