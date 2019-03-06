<<<<<<< HEAD
% Output evenly spaced data

function [data]=resample(data,step)
  datax=data(:,1);
  datay=data(:,2);
  npts=length(datax);      
    dataxeven=datax(1):step:datax(npts);
    dataxeven=dataxeven';
    datayeven=interp1(datax,datay,dataxeven,'cubic');
=======
% Output evenly spaced data

function [data]=resample(data,step)
  datax=data(:,1);
  datay=data(:,2);
  npts=length(datax);      
    dataxeven=datax(1):step:datax(npts);
    dataxeven=dataxeven';
    datayeven=interp1(datax,datay,dataxeven,'cubic');
>>>>>>> 53705711ba35f3cab7f210862f89039ae612b64f
    data=[dataxeven,datayeven];