<<<<<<< HEAD
% [harmp] = harmpow(data,period1,period2,step), power from period 1-2
% using step
%   OUTPUT
%       harmp, 6-column data of period, freq, amplitude, phase, A, B
function [harmp] = harmpow(data,period1,period2,step)
% freqt=62.79:.00001:62.81;
period = period1:step:period2;
nperiod=length(period);   % length of frequency

harmp=zeros(nperiod,3);   % nfreqt rows, 6 columns zero matrix
for n=1:nperiod
    [deharm_result]=deharm(data,period(n));
    harmp(n,:)=deharm_result;
end
=======
% [harmp] = harmpow(data,period1,period2,step), power from period 1-2
% using step
%   OUTPUT
%       harmp, 6-column data of period, freq, amplitude, phase, A, B
function [harmp] = harmpow(data,period1,period2,step)
% freqt=62.79:.00001:62.81;
period = period1:step:period2;
nperiod=length(period);   % length of frequency

harmp=zeros(nperiod,3);   % nfreqt rows, 6 columns zero matrix
for n=1:nperiod
    [deharm_result]=deharm(data,period(n));
    harmp(n,:)=deharm_result;
end
>>>>>>> 53705711ba35f3cab7f210862f89039ae612b64f
