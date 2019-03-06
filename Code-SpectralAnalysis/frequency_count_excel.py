#! /usr/bin/python
import csv
import matplotlib.pyplot as plt
import matplotlib.patches as patches
import numpy as np

from pprint import pprint as pp

# Directory to get the TSCreator datapack
prev_dir= '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/projects/'
datapack_parent_dir = prev_dir +'ML-Data Mining/datapacks/' 

# open the datapack file
dp_file = datapack_parent_dir + 'HumanCulture MidEastIntervals wGreenland-Ice cleaned 19Jan2016.txt'
f = open(dp_file, 'rU')

# the size of the bin for which the number of events will be counted
yr_bin_size = float(raw_input("Please enter the time window (bin) in year = ")) 
unit = 1000 # 1K

# how much to slide over the time (rolling/sliding window)
sliding_window = float(raw_input("Please enter the sliding window  = ")) 

# Minimum and maximum age to explore
min_age = float(raw_input("Minimum age to be explored in thousand years = "))
max_age = float(raw_input("Maximum age to be explored in thousand years = "))

# File for listing the events in yeach bin
f_n = prev_dir + 'ML-Data Mining/programming/output/HumanCulture_MidEastIntervals_' + str(yr_bin_size) +  'yr_bin_' + str(sliding_window) +'yr_slide_window.txt'
f_w = open(f_n, 'wb');
f_w.write('begin_age\tend_age\tevents\n')

# Datapack file for creating curve column in tscreator after counting the events from the datapack
f_d_n = 'HumanCulture_MidEastIntervals_' + str(yr_bin_size) + '_yr_bin' + str(sliding_window) + 'yr_event_frequency_curve'
f_d = prev_dir + 'ML-Data Mining/programming/output/' + f_d_n + '_datapack.txt'
f_d_w = open(f_d, 'wb');

# Datapack header
f_d_w.write('format version:' + '\t1.2\n')
f_d_w.write('date:' + '\t16/08/2017\n')
f_d_w.write('age_units:' + '\tka (before AD2000)\n\n')

f_d_w.write(f_d_n + '\tpoint\t250\n')
f_d_w.write('nopoints\tline\t0\t500\n')

reader = csv.reader(f, delimiter='\t')

c = 0
ages = []
s_events = []

# Columns to be skipped for event counting
skip_column_list = ['Greenland', 'HumanCulture_MidEastIntervals']
for row in reader:
    #skip columns
    if len(row) >=1: 
        q = 0
        for a in skip_column_list: 
            if a in row[0]:
                q = 1
        if q == 1:
            break
        
    # skip the row with column name
    if len(row) >= 1 and row[0] != "":
        pass

    # Extract events and ages
    if len(row) >= 3 and row[0] == "":
        #print row
        if row[1] != "" and row[2] != "":
            s_events.append(str(row[1]))
            ages.append(float(row[2]))
    c += 1

gr_ages = []
gr_oxy = []
#extract greenland oxy-18 curve datapoints (temperature related data)
if len(row) >=1 and skip_column_list[0] in row[0]:
    for row in reader:
        if len(row) != 3 or len(row) > 3:
           continue 
        #print row
        gr_ages.append(float(row[1]))        
        gr_oxy.append(float(row[2]))
        
gr_ages_b = [a for a in gr_ages if a <= max_age]
#idx_max = gr_ages.index(max(gr_ages_b))
gr_oxy_b = gr_oxy[0:len(gr_ages_b)]


#sorting ages and events for counting per year bin
sorted_indices=sorted(range(len(ages)), key=lambda x:ages[x])
sorted_ages = sorted(ages)
sorted_events = [s_events[i] for i in sorted_indices]

min_global_age = max(min(sorted_ages), min_age)
max_global_age = min(max(sorted_ages), max_age)
#max_age = 10 # Only first 10K years

# initializing the range of each bin
l_range = min_global_age 
u_range = l_range + yr_bin_size/unit 

print l_range
print u_range

age = []
event = [] # list of list of events per yr bin
freq = []
cnt = 1
while u_range <= max_age:
    c = 0
    events = []
    for i, a in enumerate(sorted_ages):
        if a >= l_range and a < u_range:
            c += 1
	    events.append(sorted_events[i])

    event.append(events)
    #print cnt, '. ', '(', str(l_range), ', ', str(u_range), 'Ka): ', str(c)
    #print events
    #print ""
    #print ""
    f_w.write(str(l_range) + '\t' + str(u_range) + '\t' + str(events) + '\n')
    cnt += 1

    age.append(l_range)
    freq.append(c)
    # updating the range of the bin
    l_range += (sliding_window/unit)
    u_range = l_range + (yr_bin_size/unit) 

mn = int((min_age * unit) / yr_bin_size) 
mx = int((max_age * unit) / yr_bin_size * (yr_bin_size / sliding_window))

print len(freq) 
print mx
ages = age[mn:mx]

# take the center of each sliding window 
ages_centered = []
gr_ages_moving_avg = []
gr_oxy_moving_avg = []
j = 0
for i in xrange(mn, mx):
    if i == len(ages):
        break

    a_lower = ages[i]
    a_upper = ages[i] + yr_bin_size/unit
    a = float(a_lower + a_upper) / 2.0
    nage = round(a,2)
    ages_centered.append(nage)

    cnt_gr = 0
    oxy_sum = 0
    while gr_ages_b[j] >= a_lower and gr_ages_b[j] < a_upper: 
        oxy_sum += gr_oxy_b[j]
        j += 1
        cnt_gr += 1

    if cnt_gr != 0:
        oxy_avg = oxy_sum / cnt_gr
    else:
        oxy_avg = oxy_sum + 0.5

    gr_ages_moving_avg.append(nage)
    gr_oxy_moving_avg.append(oxy_avg)


counts = freq[mn:mx]

for i in range(len(ages_centered)):
    f_d_w.write('\t' + str(ages_centered[i]) + '\t' + str(counts[i]) + '\n')
    #print(str(ages[i]) + '\t' + str(counts[i]) + '\n')

f_d_w.close()

fig, ax = plt.subplots()

# Twin the x-axis twice to make independent y-axes.
axes = [ax, ax.twinx()]

# Make some space on the right side for the extra y-axis.
fig.subplots_adjust(right=0.75)

# Move the last y-axis spine over to the right by 20% of the width of the axes
#axes[-1].spines['right'].set_position(('axes', 1.2))

# To make the border of the right-most axis visible, we need to turn the frame
# on. This hides the other plots, however, so we need to turn its fill off.
axes[-1].set_frame_on(True)
axes[-1].patch.set_visible(False)

# And finally we get to plot things...
colors = ('Green', 'Red')

i = 0
for ax, color in zip(axes, colors):
    data = np.random.random(1) * np.random.random(10)
    if i == 0:
	ax.plot(ages_centered, counts, ms=2, c=color, ls='-') #, alpha=opacity), marker='.'
    	ax.set_ylabel('Frequency of Events')
        i += 1
    else:
	ax.plot(gr_ages_moving_avg, gr_oxy_moving_avg, ms=2, c=color, ls='-') #, alpha=opacity), marker='.'
    	ax.set_ylabel('GRIP Oxy-18')
    #ax.plot(data, marker='o', linestyle='none', color=color)
    ax.tick_params(axis='y', colors=color)

axes[0].set_xlabel('Ka')
plt.title('Event Frequency plot')

plt.show()
plt.close()


