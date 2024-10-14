function [logp, yhat, res] = tapas_gaussian_obs_spacetask(r, infStates, ptrans)
% Calculates the log-probability of response y under the Gaussian noise model
%
% --------------------------------------------------------------------------------------------------
% Copyright (C) 2012-2013 Christoph Mathys, TNU, UZH & ETHZ
%
% This file is part of the HGF toolbox, which is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version). For further details, see the file
% COPYING or <http://www.gnu.org/licenses/>.

% Transform zeta to its native space
% if r.c_obs.dyn_noise ==0 
% ze    = exp(ptrans(1));         % ze
% else
% ze    = tapas_sgm(ptrans(1),1);         % ze
% end

% dynamic noise?
% rho = r.c_obs.dyn_noise;
be1      = exp(ptrans(1)); 
zeta_m   = ptrans(2); 
zeta_s   = exp(ptrans(3)); 

% Initialize returned log-probabilities, predictions,exp
% and residuals as NaNs so that NaN is returned for all
% irregualar trials
n = size(infStates,1);
logp = NaN(n,1);
yhat = NaN(n,2);
res  = NaN(n,2);

% Weed irregular trials out from inferred states and responses
x = infStates(:,1,3); % mean belief
x(r.irr) = [];

% nb_raw        = infStates(:,1,11); % noise belief (11 for changed as in plot script)
nb_raw = infStates(:,1,5); 
nb            = tapas_sgm(zscore(nb_raw),1); % noise belief
nb_beta       = be1.*nb; 
nb_beta(r.irr)=[]; 

y_mean = r.y(:,1);          % mean response
y_mean(r.irr) = [];

y_noise = r.y(:,2);  

% Note: 8*atan(1) == 2*pi (this is used to guard against
% errors resulting from having used pi as a variable).
reg = ~ismember(1:n,r.irr);
logp_mean(reg) = -1/2.*log(8*atan(1).*zeta_m) -(y_mean-x).^2./(2.*zeta_m);
yhat(reg,1) = x;
res(reg,1) = y_mean-x;

logp_noise(reg) = -1/2.*log(8*atan(1).*zeta_s) -(y_noise-nb_beta).^2./(2.*zeta_s);
yhat(reg,2) = nb_beta;
res(reg,2) = y_noise-nb_beta; 
 
 
% logp_noise(reg) = 0; % messy workaround to not fit the beamwidth

% logp(reg) = -1/2.*log(8*atan(1).*ze) -(y-x).^2./(2.*ze);
logp(reg) = sum([logp_mean(reg); logp_noise(reg)],1);  % the higher the better 

return;
