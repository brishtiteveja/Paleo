%%%
%%% dftps_mcmc.m is a Matlab/Octave routine for computing the 
%%% discrete-time Fouerier power spectrum for a uniformly sampled time 
%%% series and plotting the phase shifted sine wave of the dominant
%%% cycle over the original series. This routine also computes the 
%%% statistical significance of cycles relative to 1,000 MCMC random
%%% walk trials
%%%

close all;clc;clear

fname='pcm_B05_12_fyr-swc-p-at_0001_monthly.dat';
data=load(fname);
t=data(:,1);
swc=data(:,2);  % fractional volume of root zone water ranging from 0-1
p=data(:,3);    % precipitation (mm)
at=data(:,4);   % air temperature (Kelvin)
x=at;

%%% Fourier transformation
sd_x=std(x);
xn=x/sd_x;             % normalize residuals to variance of 1
lps=2^(16);            % length (num. of elements) for power spectrum
xn_fft=fft(xn,lps);    % Fourier transform interpolated to length lps
xn_ps=xn_fft .* conj(xn_fft)/(lps);  % power spectrum is computed by 
                       % piecewise squaring of complex conjugate 
                       % divided by lps
fmax=0.1;              % highest frequency of interest (freq=1/time).
                       % Annual cycle in monthly data corresonds to 
                       % f = 1/12 = 0.0833, therefore set fmax to 0.1
f=[0:1:round(fmax*lps)]/lps;  % for horizontal axis compute frequencies
                       % from 0 to fmax in increments proprotional
                       % to lps. If interested in higher frequencies
                       % then increase fmax
amp=std(at);           % Get amplitude for constructing sine wave

%%% Monte Carlo random walk trials
tic
step(1:length(x)-1)=x(2:length(x)) .- x(1:length(x)-1);
n_iters=1000;
ck_iter=n_iters/10
for itest = 1:n_iters
    if ck_iter*fix(itest/ck_iter)==itest
        itest
    end

    %%% Permute
    xc(1,1)=x(1);
    rp=randperm(length(x)-1);
    for istep = 1:length(x)-1
        xc(istep+1,1) = xc(istep,1) + step(rp(istep));
    end

    %%% Detrend
    sd_xc=std(xc);
    xc_n=xc/sd_xc;
    xc_fft=fft(xc_n,lps);
    xc_ps=xc_fft .* conj(xc_fft)/(lps);
    mc_test(itest,1:round(fmax*lps)+1)=xc_ps(1:round(fmax*lps)+1);
end
toc

%%% Compute and accumulate the mean, max, stddev of the monte carlo
%%% trials at each frequency
for isa=1:round(fmax*lps)+1
    mc_max(isa)=max(mc_test(:,isa));
    mc_mean(isa)=mean(mc_test(:,isa));
    mc_sd(isa)=std(mc_test(:,isa));
end

%%% Print out some basic stats describing the DFTPS
offset=1000;           % offset the search range to avoid the constant 
                       % component (DC) and edge effects
[re re_idx]=max(xn_ps(offset:length(f)));  % index values of the 
                       % largest peak for the real & imaginary parts
per=(1/f(offset+re_idx));  % Period of the dominant peak
p_shift=angle(xn_ps(re_idx));  % Phase shift of the dominant peak
sw=mean(x)+(amp*sin(2*(pi/per)*[1:length(x)]+p_shift));  % phase-shifted sine wave

%%% Print to screen some basis stats
fprintf('Some stats about the largest peak (the dominant cycle)\n')
fprintf('Peak value is at FT index: %d\n', re_idx )
fprintf('Amplitude of the dominant peak is: %0.4f\n', re )
fprintf('Frequency of the dominant peak is: %0.4f (cycles/month)\n', f(offset-1+re_idx))
fprintf('Period of the dominant peak is: %0.1f months\n', per )
fprintf('The phase shift of the dominant peak: %0.3f (radians)\n\n', p_shift )

%%% Plot data
subplot(2,1,1); plot(f,xn_ps(1:round(fmax*lps)+1),'k','LineWidth',4,f,mc_mean(1:round(fmax*lps)+1),'r','LineWidth',4); axis([0 0.1 0 12]); xlabel('Frequency (cycles/month)'); ylabel('Spectral Power (normalized)'); line([(1/12) (1/12)], [12 10], 'Color', 'm', 'LineWidth',2); text((1/11.9),11,'12 month period')
subplot(2,1,2); plot(t(1:200),x(1:200),'k','LineWidth',4, t(1:200), sw(1:200),'y','LineWidth',3); axis([2000 2017 205 260]); xlabel('Year'); ylabel('Monthly Mean Air Temp. (K)');text(2002,253,...    % Position label apropriately
	['Dominant cycle period: ',num2str(per),' months, Phase shift: ',...
	num2str(p_shift),' (radians)'],'HorizontalAlignment','left')

%%% Write output to file
fid=fopen('dftps_mcmc_f.dat','w');
fprintf(fid,'%f\n',f);
fclose(fid);
fid=fopen('dftps_mcmc_rs.dat','w');
fprintf(fid,'%f\n',mc_mean(1:round(fmax*lps)+1));
fclose(fid);
fid=fopen('dftps_mcmc_sw.dat','w');
fprintf(fid,'%f\n',sw);
fclose(fid);
fid=fopen('dftps_mcmc_ps.dat','w');
fprintf(fid,'%f\n',xn_ps(1:round(fmax*lps)+1));
fclose(fid);
