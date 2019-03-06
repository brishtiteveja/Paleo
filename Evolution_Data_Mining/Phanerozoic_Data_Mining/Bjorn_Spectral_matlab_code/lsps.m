%%%
%%% lsps.m is a Matlab/Octave routine for computing the Lomb-Scargle
%%% power spectrum for a non-uniformly sampled series
%%%

close all;clc;clear
ofac=4;        % interpolation (oversampling) parameter (usually >= 4)
fmax=0.11;     % frequencies over which to search (<= 1)
hifac=2*fmax;

fname='lef_2008_2009.dat'; % input file name
data=load(fname);
time=data(:,1);
x=transpose(data(:,2));
t=1:length(x); % Make a vector of time elements

%%% Set limits for the highest frequency of interest with 'fmax' and the 
%%% lowest with 'offset'.
fmax=0.0025;      % 0 < fmax < 0.5. To satisfy the Nyquist sampling theorem
               % I recommend fmax < 0.5.
offset=500;    % Use 'offset' to exclude low-frequency cycles, as they
               % may be too close to the DC to be significant. 'offset' 
               % shifts the index of values in 'xn_lsps' that you are searching 
               % across to find peaks. Larger 'offset' values skip 
               % over lower-frequency cycles. For example 'offset=100' 
               % will shift the search for peaks past the first 100 elements, 
               % which are the lowest frequency cycles.
sd_x=std(x);   % Find standard deviation
xn=x/sd_x;     % and normalize to variance of 1
amp=max(x)*0.5;% Get amplitudes for constructing sine waves later
int=mean(diff(t));
f=((2*int)^(-1))/(length(xn)*ofac):((2*int)^(-1))/(length(xn)*ofac):hifac*(2*int)^(-1);
for k=1:length(f)
   omega=2 * pi * f(k);
   xn_lsps(k)=1 / (2 * var(xn)) * ((sum(xn .* cos(omega * t - ...
   atan2(sum(sin(2 * omega * t)), sum(cos(2 * omega * t))) / 2))) .^ 2) / ...
   (sum((cos(omega * t - atan2(sum(sin(2 * omega * t)), sum(cos(2 * omega * ...
   t))) / 2)) .^ 2)) + ((sum(xn .* sin(omega * t - atan2(sum(sin(2 * omega * ...
   t)), sum(cos(2 * omega * t))) / 2))) .^ 2)/(sum((sin(omega * t - ...
   atan2(sum(sin(2 * omega * t)), sum(cos(2 * omega * t))) / 2)) .^2 ));
end

[re re_idx]=max(xn_lsps(offset:length(f)));  % index values of the largest peak
                                             % for the real & imaginary parts
per=(1/f(offset+re_idx));          % Period of the dominant peak
p_shift=angle(xn_lsps(re_idx));    % Phase shift of the dominant peak
n_t=[2008.43:(1/365/24):2008.46];  % Evenly sampled time vector for sine wave
sw=mean(xn)+(amp*sin(2*(pi/per)*[1:length(n_t)]+p_shift)); % phase-shifted sine
                                                           % wave
%%% Print to screen some basis stats
fprintf('Some stats about the largest peak (the dominant cycle)\n')
fprintf('Peak value is at FT index: %d\n', re_idx )
fprintf('Amplitude of the dominant peak is: %0.4f\n', re )
fprintf('Frequency of the dominant peak is: %0.4f (cycles/month)\n', f(offset-1+re_idx))
fprintf('Period of the dominant peak is: %0.1f months\n', per )
fprintf('The phase shift of the dominant peak: %0.3f (radians)\n\n', p_shift )

%%% Plot data
subplot(2,1,1); plot(f,xn_lsps,'k','LineWidth',4); axis([0 0.1 0 180]); xlabel('Frequency (cycles/hour)'); ylabel('Spectral Power (normalized)'); line([(1/24) (1/24)], [180 150], 'Color', 'm', 'LineWidth',2); text((1/23),170,'24 hour period')
subplot(2,1,2); plot(time,x,'+k',n_t,sw,'g','LineWidth',3'); axis([2008.43 2008.46 -12 18]); xlabel('Date'); ylabel('NEE (um mol m-2 s-1)'); text(2008.435,15,'Irregularly sampled NEE obs and sine wave of 24 cycle')

%%% Write data to output file
fid=fopen('lef_2008_2009_lsps.dat','w');
for k=1:length(xn_lsps)
   fprintf(fid,'%f  %f\n', f(k), xn_lsps(k));
end
fclose(fid);
%fid=fopen('lef_2008_2009_lsps.dat','w');
%fprintf(fid,'%f\n',f);
%fclose(fid);
