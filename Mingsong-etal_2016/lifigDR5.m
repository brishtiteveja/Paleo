<<<<<<< HEAD
% Figure DR5 in Li, Huang, Hinnov et al., 2016 Geology
%
% INPUT
%       data  :  Modified from Laskar 2004 or Laskar 2010d solution.
%                The data must be a 249001 x 4 matrix, which contains 
%                   t, eccentricity, obliquity and precession, 
%                   where t is the time from J2000 (in kyr).
%                   The step time is +1 kyr. 
%                   The first 3 rows may be as follows:
%                       0. 0.016702 0.409093 1.796257
%                       1. 0.017161 0.411353 1.497928
%                       2. 0.017497 0.413555 1.200024
%                 The Laskar 2004 solution may be downloaded at
%                 http://vo.imcce.fr/insola/earth/online/earth/La2004/INSOLN.LA2004.BTL.250.ASC
% OUTPUT
%       modetpla	:	ETP series using 10E3T2P (i.e., 10 x eccentricity + 2 x obliquity + 2 x precession)
%       interv      :	ETP series using  1E1T1P
%       powratio	:	2-column series of Obliquity power / total power (O/T)
%       powratioresample :  Interpolated powerratio; to reduce times of calculation
%       harmpow     :	2-row period-amplitude-phase dataset with top-2 amplitude values
%       modelcurve	:	Modeled O/T curves
%   Calls for
%       deharm.m
%       getpks.m
%       harmpow.m
%       harmpowtry.m
%       modcurve.m
%       modetp.m
%       pda.m
%       resample.m

clear;clc;

% calculate modeled ETP using Laskar solution from 230 Ma to 249 Ma
% ratio of relative power of ecc, obliquity, precession is set to 10:3:2
% See Supplementary information '2.2 Obliquity power/total power (O/T)' for details 
%   in Li, Huang, Hinnov et al., 2016 Geology
[modetpla,interv] = modetp(data,230001,249001,10,3,2);
disp('>>      STEP 1/6: Modeled ETP: DONE');
disp('>>      STEP 2/6: Power ratio: working ...');

% calculate power ratio of obliquity power/total power for modetpla
% fmin is 1/45 kyr-1; fmax is 1/24 kyr-1; window is 500 kyr; and number of tapers is 2 pi. 
[powratio,m]=pda(modetpla,1/45,1/24,500,2);
disp('>>      STEP 2/6: Power ratio: DONE');

% Interpolate powratio data
step = 10;  % interpolated sample rate; original rate is 1 kyr, now set to 10 kyr.
[powratioresample]=resample(powratio(:,1:2),step);
disp('>>      STEP 3/6: Interpolate: DONE');

% calculate deharm results
% Get 2-row period-amplitude-phase datasets with top-2 amplitude values
[harmpow] = harmpowtry(powratioresample);
disp('>>      STEP 4/6: Find amplitude peaks: DONE');

% Construction modeled long-term modulation cycles of obliquity power/total power
% from 230 Ma to 250 Ma
% In figure DR5, modeled curve is from 230 Ma to 254 Ma
[modelcurve] = modcurve(powratioresample,harmpow,230000,250000);

disp('>>      STEP 5/6: Modeled curves: DONE');
disp('>>        Period  Amplitude  Phase  are : ');
disp(num2str(harmpow))

figure; 
subplot(3,1,1),plot(modelcurve(:,1),modelcurve(:,2));
       title('Fig. DR5 in Li et al., 2016, Geology')
       ylabel('Modeled O/T')
subplot(3,1,2),plot(interv(:,1),interv(:,3));
       ylabel('Obliquity')
subplot(3,1,3),plot(powratio(:,1),powratio(:,2))
       xlabel('Age ( kyr )') 
       ylabel('Modeled O/T')

disp('>>      STEP 6/6: Plot: DONE');
=======
% Figure DR5 in Li, Huang, Hinnov et al., 2016 Geology
%
% INPUT
%       data  :  Modified from Laskar 2004 or Laskar 2010d solution.
%                The data must be a 249001 x 4 matrix, which contains 
%                   t, eccentricity, obliquity and precession, 
%                   where t is the time from J2000 (in kyr).
%                   The step time is +1 kyr. 
%                   The first 3 rows may be as follows:
%                       0. 0.016702 0.409093 1.796257
%                       1. 0.017161 0.411353 1.497928
%                       2. 0.017497 0.413555 1.200024
%                 The Laskar 2004 solution may be downloaded at
%                 http://vo.imcce.fr/insola/earth/online/earth/La2004/INSOLN.LA2004.BTL.250.ASC
% OUTPUT
%       modetpla	:	ETP series using 10E3T2P (i.e., 10 x eccentricity + 2 x obliquity + 2 x precession)
%       interv      :	ETP series using  1E1T1P
%       powratio	:	2-column series of Obliquity power / total power (O/T)
%       powratioresample :  Interpolated powerratio; to reduce times of calculation
%       harmpow     :	2-row period-amplitude-phase dataset with top-2 amplitude values
%       modelcurve	:	Modeled O/T curves
%   Calls for
%       deharm.m
%       getpks.m
%       harmpow.m
%       harmpowtry.m
%       modcurve.m
%       modetp.m
%       pda.m
%       resample.m

clear;clc;

% calculate modeled ETP using Laskar solution from 230 Ma to 249 Ma
% ratio of relative power of ecc, obliquity, precession is set to 10:3:2
% See Supplementary information '2.2 Obliquity power/total power (O/T)' for details 
%   in Li, Huang, Hinnov et al., 2016 Geology
[modetpla,interv] = modetp(data,230001,249001,10,3,2);
disp('>>      STEP 1/6: Modeled ETP: DONE');
disp('>>      STEP 2/6: Power ratio: working ...');

% calculate power ratio of obliquity power/total power for modetpla
% fmin is 1/45 kyr-1; fmax is 1/24 kyr-1; window is 500 kyr; and number of tapers is 2 pi. 
[powratio,m]=pda(modetpla,1/45,1/24,500,2);
disp('>>      STEP 2/6: Power ratio: DONE');

% Interpolate powratio data
step = 10;  % interpolated sample rate; original rate is 1 kyr, now set to 10 kyr.
[powratioresample]=resample(powratio(:,1:2),step);
disp('>>      STEP 3/6: Interpolate: DONE');

% calculate deharm results
% Get 2-row period-amplitude-phase datasets with top-2 amplitude values
[harmpow] = harmpowtry(powratioresample);
disp('>>      STEP 4/6: Find amplitude peaks: DONE');

% Construction modeled long-term modulation cycles of obliquity power/total power
% from 230 Ma to 250 Ma
% In figure DR5, modeled curve is from 230 Ma to 254 Ma
[modelcurve] = modcurve(powratioresample,harmpow,230000,250000);

disp('>>      STEP 5/6: Modeled curves: DONE');
disp('>>        Period  Amplitude  Phase  are : ');
disp(num2str(harmpow))

figure; 
subplot(3,1,1),plot(modelcurve(:,1),modelcurve(:,2));
       title('Fig. DR5 in Li et al., 2016, Geology')
       ylabel('Modeled O/T')
subplot(3,1,2),plot(interv(:,1),interv(:,3));
       ylabel('Obliquity')
subplot(3,1,3),plot(powratio(:,1),powratio(:,2))
       xlabel('Age ( kyr )') 
       ylabel('Modeled O/T')

disp('>>      STEP 6/6: Plot: DONE');
>>>>>>> 53705711ba35f3cab7f210862f89039ae612b64f
