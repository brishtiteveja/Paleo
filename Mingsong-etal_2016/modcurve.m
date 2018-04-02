<<<<<<< HEAD
function [modelcurve]=modcurve(data,harmpow,t1,t2)
% This script construct modeled curve from t1 to t2 using 'harmpow' result
% OUTPUT
%   modelcurve :    2-column datasets with time and value

period=harmpow(:,1);
amp=harmpow(:,2);
phase=harmpow(:,3);

[nrow ncol] = size (harmpow);
y=0.;
step = data(2,1)-data(1,1);

t=t1:step:t2;

for n=1:nrow
    y=y+amp(n)*sin(2*pi/period(n)*t+phase(n));
end

=======
function [modelcurve]=modcurve(data,harmpow,t1,t2)
% This script construct modeled curve from t1 to t2 using 'harmpow' result
% OUTPUT
%   modelcurve :    2-column datasets with time and value

period=harmpow(:,1);
amp=harmpow(:,2);
phase=harmpow(:,3);

[nrow ncol] = size (harmpow);
y=0.;
step = data(2,1)-data(1,1);

t=t1:step:t2;

for n=1:nrow
    y=y+amp(n)*sin(2*pi/period(n)*t+phase(n));
end

>>>>>>> 53705711ba35f3cab7f210862f89039ae612b64f
modelcurve=[t',y'];