%%%
%%% csps.m is a Matlab/Octave routine for computing the cross-spectrum 
%%% density of two uniformly sampled series
%%%

close all;clc;clear

fname='pcm_B05_12_fyr-swc-p-at_0001_monthly.dat';
data=load(fname);
t=data(:,1);   % time
x1=data(:,2);  % Soil moisture (volumetric fraction)
x2=data(:,3);  % Precipitation (mm)
x3=data(:,4);  % Temperature (K)

[sd f]=cpsd(x1,x2,48,0,10*length(x1)); % spectral density and associated frequency
sp=sd .* conj(sd);  % Compute cross product for total spectral power

[sd2 f2]=cpsd(x2,x3,48,0,10*length(x2));
sp2=sd2 .* conj(sd2);

marker=max(sp);
marker2=max(sp2);

%%% Plot data
subplot(2,2,1); plot(f,sp,'k','LineWidth',4); xlabel('Frequency (cycles/month)'); ylabel('Cospectral Power (normalized)'); line([(1/12) (1/12)], [marker*1.1 marker], 'Color', 'm', 'LineWidth',2); text((1/11.5),marker*1.04,'12 month period'); text(0.15,1.4,'Precip,Soil Mois Cospectrum')
subplot(2,2,2); plot(f,angle(sd),'k','LineWidth',4); axis([0 max(f) -pi pi]); xlabel('Frequency (cycles/month)'); ylabel('Phase Synchronization (-pi:pi)'); line([(1/12) (1/12)], [pi*0.8 pi], 'Color', 'm', 'LineWidth',2); text((1/11.5),pi*0.9,'12 month period')
subplot(2,2,3); plot(f2,sp2,'k','LineWidth',4); xlabel('Frequency (cycles/month)'); ylabel('Cospectral Power (normalized)'); line([(1/12) (1/12)], [marker2*1.1 marker2], 'Color', 'm', 'LineWidth',2); text((1/11.5),marker2*1.04,'12 month period'); text(0.14,3,'Precip,Temp. Cospectrum')
subplot(2,2,4); plot(f2,angle(sd2),'k','LineWidth',4); axis([0 max(f) -pi pi]);  xlabel('Frequency (cycles/month)'); ylabel('Phase Synchronization (-pi:pi)'); line([(1/12) (1/12)], [pi*0.8 pi], 'Color', 'm', 'LineWidth',2); text((1/11.5),pi*0.9,'12 month period')

%%% Write output to file
fid=fopen('csps_f.dat','w');
fprintf(fid,'%f\n',f);
fclose(fid);
fid=fopen('csps_sp.dat','w');
fprintf(fid,'%f\n',sp);
fclose(fid);
fid=fopen('csps_phase.dat','w');
fprintf(fid,'%f\n',angle(sd));
fclose(fid);
