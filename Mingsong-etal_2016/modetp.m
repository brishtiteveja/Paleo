% [modetp,data] = modetp(Laskar,t1,t2,powE,powO,powP)
% 
% INPUT
%   Laskar, astronomical solution with 4 columns, i.e., time, ecc, obl, pre
%   t1, modeled start time
%   t2, modeled end time
%   powE, powO, powP are based on relative variance (power)
%       in the eccentricity, obliquity and precession bands
%       of the tuned 'real' series
% OUTPUT
%   modetp  :    2-column modeled ETP series contains time, modeled ETP
%       with ratio of powE-powO-powP, e.g., 10E3T2P
%   data	:    E, T, P series of selected interval

% Mingsong Li (China Univ Geosci & Johns Hopkins Univ), Mar 12, 2016

function [modetp,data] = modetp(Laskar,t1,t2,powE,powO,powP)

t1=min(t1,t2);      % sort t1,t2; t1 < t2
t2=max(t1,t2);      % sort t1,t2; t2 > t1

disp(['>>      Calculate ETP from ',num2str(t1),' to ',num2str(t2), ' kyr']);
disp(['>>      Ratio of relative power of Ecc, Obliq and Prec is ',...
    num2str(powE),' : ',num2str(powO), ' : ',num2str(powP)]);

nrow=t2-t1+1;       % row of output
data=zeros(nrow,4); % output data of modeled ETP
% Save selected time interval of Laskar solution to data
for i= t1:t2
 data(i-t1+1,:)=Laskar(i,:);
end
modetp(:,1)=data(:,1);
% modeled ETP using input powE, powO, powP (Ratio of relative power of Ecc, Obliq and Prec)
modetp(:,2)=powE*zscore(data(:,2))+powO*zscore(data(:,3))-powP*zscore(data(:,4));

