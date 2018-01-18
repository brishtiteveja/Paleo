#!/usr/bin/python
import sqlite3
import pprint as pp
import numpy as np
import matplotlib.pyplot as plt

db_file='/Users/andy/Documents/TSCreator/Data Mining/databases/Past_10000_years_Jul24_2017.sqlite'

conn = sqlite3.connect(db_file)
c = conn.cursor()

col='type'

c.execute('select min({ba}) from {tn}'.format(ba='begin_age', tn='data'))
min_age = c.fetchall()[0][0]

c.execute('select max({ea}) from {tn}'.format(ea='end_age', tn='data'))
max_age = c.fetchall()[0][0]
max_age = 1000

events = []
event_count = []

prev_a = min_age - 0.5 # to add 0.0 at the beginning 
step = 0.2 # every 200 years as the age is in Ka
a = min_age + step 
tbl1_name = 'data'

while a <= max_age:
    # Get block data
    tbl2_name = 'block_data'
    c.execute('select * from {tn1} as data join {tn2} as blockdata on (blockdata.id = data.id) where data.begin_age > {bna} and data.end_age > {bna} and data.end_age <= {ena}'
        .format(tn1=tbl1_name,
                tn2=tbl2_name, 
                bna=prev_a, 
                ena=a))

    all_rows = c.fetchall()

    # Get event data
    tbl2_name = 'event_data'
    c.execute('select * from {tn1} as data join {tn2} as eventdata on (eventdata.id = data.id) where data.begin_age > {bna} and data.end_age > {bna} and data.end_age <= {ena}'
        .format(tn1=tbl1_name,
                tn2=tbl2_name, 
                bna=prev_a, 
                ena=a))

    all_rows += c.fetchall()

    es = []
    for r in all_rows:
        r_begin_age = r[1]  # In some data point, begin age is greater than end_age (TOP points, derived from last end age) and vice versa
        r_end_age = r[2]

        if r_begin_age > r_end_age:
            continue
        
        if r_begin_age > prev_a and r_end_age <= a :
            es.append(r)

    events.append((a, es))
    event_count.append((a, len(es)))

    #(a, ess) = events[0]
    #print ess
    #print len(ess)
    #print a

    prev_a = a
    a += step

#print event_count

conn.close()

e = np.array(event_count)

mn = 0
mx = len(event_count) 
ages = e[:,0][mn:mx]
print len(ages)
counts = e[:,1][mn:mx]
print len(counts)

fig = plt.figure()
bar_width = 0.05
opacity=0.4
plt.bar(ages, counts, bar_width, color='g', alpha=opacity)
plt.title('Number of events per thousand years')
plt.xlabel('Ka') 
plt.ylabel('Number of events')
plt.show()
plt.close()
