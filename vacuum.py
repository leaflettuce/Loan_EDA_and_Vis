# -*- coding: utf-8 -*-
"""
Spyder Editor

"""
import csv, pprint
import pandas as pd
import os

os.chdir('d:/projects/SNHU/loans/')



#manually check numbers and set categories
with open('for_python.csv') as csvfile:
    df = csv.reader(csvfile, delimiter=',')
    next(df)
    
    job_collection = {}
    categories = {'sales': ['sale', 'retail', 'target'],
                  'health care': ['med', 'x-ray', 'health', 'care',],
                  'management': ['manage'],
                  'bus and finance': ['business', 'finance', 'account', 
                                      'analyst', 'executive', 'bank', ],
                  'comp and tech': ['tech', 'it ', 'informat', 'data',
                                    'program', 'dev', 'netwo'],
                  'engineering': ['archite', 'engineer', 'scien'],
                  'education': ['teach', 'prof', 'educat'],
                  'entertainment and arts': ['entert', 'music', 'art', 'danc'],
                  'service inductry': ['service', 'trans', 'drive', 'styl',
                                       'customer', 'rep', 'starb', 'coffee', 'bari',
                                       'resta'],
                  'government': ['gov', 'public', 'polic', 'fire', 'emergen', 'dept',
                                 'rev', 'inter', 'bur'],
                  'legal': ['legal', 'law', 'para']}
    row_count = 0
    
    for i in df:
        row_count += 1
        job = i[9]
        job = job.lower()
        skip = False
        
        #Do cleaning here!
        for j in categories.keys():
            for i in categories[j]:
                if job in i:
                    job = j
                    skip = True
                
        if skip == False:        
            job = 'other'
        #End cleaning here!
        
        if job in job_collection:
            job_collection[job] += 1
        else:
            job_collection[job] = 1
        
    pprint.pprint(job_collection)
    print 'Total job types:',len(job_collection)


### CLEAN REASON FOR LOAN
df = pd.read_csv('for_python.csv')

len(pd.unique(df['title']))

count = 0
for i in df['title']:
    i = str(i).lower()
    
    if 'wedd' in i:
        df.set_value(count, 'title', 'wedding')
    elif 'med' in i or 'surg' in i:
        df.set_value(count, 'title', 'medical')
    elif 'car' in i or 'motor' in i or 'vehic' in i:
        df.set_value(count, 'title', 'vehicle')
    elif 'bike' in i or 'comp' in i or 'personal' in i or 'priv' in i or 'purch' in i:
        df.set_value(count, 'title', 'personal')
    elif 'home' in i or 'mor' in i or 'movi' in i or 'furn' in i or 'impro' in i:
        df.set_value(count, 'title', 'home_investment')
    elif 'bus' in i:
        df.set_value(count, 'title', 'business')
    elif 'bill' in i:
        df.set_value(count, 'title', 'bills')
    elif 'debt' in i or 'credit' in i or 'conso' in i or 'card' in i or 'loan' in i or'refi' in i or 'free' in i:
        df.set_value(count, 'title', 'debt_consolidation')
    else:
        df.set_value(count, 'title', 'other')
        
    if count%100 == 0:
        print(float(count)/len(df))
    count += 1

len(pd.unique(df['title']))

df.to_csv('loans_cleaned.csv')