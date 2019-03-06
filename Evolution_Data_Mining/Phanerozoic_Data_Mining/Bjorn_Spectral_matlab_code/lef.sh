#!/bin/bash
# ############################################################################ #
#                                                                              #
# lef.sh  is a routine that downloads CO2 fluxes from the University of        #
# Wisconsin-Madison's near real-time flux tower network in northern Wisconsin. #
# The data are from the LEF 396 meter tall tower, and its fluxes are           #
# representative of a North American mixed deciduous/confier ecotone.          #
# These CO2 flux measurements of net ecosystem exchange contain gaps due to    #
# instrument failures, storms, hardware problems, etc. These data are used by  #
# the matlab routine 'lsps.m' to illustrate how to do power spectrum analysis  #
# on data that are not uniformly sampled.                                      #
#                                                                              #
# Requires:                                                                    #
# awk, Octave (or Matlab), wget                                                #
#                                                                              #
# ############################################################################ #

### Check programs
for PROG in awk octave wget; do
   if [ `which ${PROG} > /dev/null 2>&1 ; echo $?` -ne 0 ]; then
      echo ${PROG} is required by this script.
      exit
   fi
done

### Dowload lef data for specified years
for (( yr=2008;yr<=2009;yr++ )); do
   wget -O lef_${yr}.txt http://flux.aos.wisc.edu/data/wlef/flux/${yr}/prefnee_${yr}.txt
done

### Parse lef data for specified years, printing only the non-null values
for (( yr=2008;yr<=2009;yr++ )); do
   awk -v YR=${yr} '{if ($7 != -999.0)
	printf "%0.5f  %s\n", YR+$6/365, $7;
   }' lef_${yr}.txt | tail -n+2 > lef_${yr}.dat
done
cat lef_2008.dat lef_2009.dat > lef_2008_2009.dat

octave -q lsps.m >& lsps.logfile

### after transforming lef data with lomb-scargle use this to pull the
### power spectrum off the x-axis so it looks nice in the plot.
awk '{if ($2 < 2) print $1, 2; else print $1, $2}' lef_2008_2009_lsps.dat > lef_2008_2009_lsps.tmp; mv lef_2008_2009_lsps.tmp lef_2008_2009_lsps.dat
rm -f lef_200[8-9].txt lef_200[8-9].dat lef_200[8-9]_[0-9].dat lef_200[8-9]_[0-9][0-9].dat
