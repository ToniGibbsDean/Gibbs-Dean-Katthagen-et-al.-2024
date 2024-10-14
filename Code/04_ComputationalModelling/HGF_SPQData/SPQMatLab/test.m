clear all; close all

% load est_withnb_reverse_level3_kax0 % only sub 77
load est_withnb_reverse_level4_kaa0 % only sub 77



for i = 1:length(est)
kappa_x(i) = est(i).c_prc.logkaxsa;
kappa_a(i) = est(i).c_prc.logkaasa;
end

scatter(kappa_x, 1:length(kappa_x))

figure
scatter(kappa_a, 1:length(kappa_a))