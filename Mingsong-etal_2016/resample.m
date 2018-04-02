% Output evenly spaced data

function [data]=resample(data,step)
  datax=data(:,1);
  datay=data(:,2);
  npts=length(datax);      
    dataxeven=datax(1):step:datax(npts);
    dataxeven=dataxeven';
    datayeven=interp1(datax,datay,dataxeven,'cubic');
    data=[dataxeven,datayeven];