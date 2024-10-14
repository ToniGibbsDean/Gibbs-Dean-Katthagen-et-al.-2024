function y = tapas_gaussian_obs_spacetask_sim(r, infStates, p, y)
% Calculates the log-probability of response y under the Gaussian noise model
%
% --------------------------------------------------------------------------------------------------
% Copyright (C) 2012-2013 Christoph Mathys, TNU, UZH & ETHZ
%
% This file is part of the HGF toolbox, which is released under the terms of the GNU General Public
% Licence (GPL), version 3. You can redistribute it and/or modify it under the terms of the GPL
% (either version 3 or, at your option, any later version). For further details, see the file
% COPYING or <http://www.gnu.org/licenses/>.

% read out parameters for response model
be1   = p(1); 
zem   = p(2); 
zes   = p(3); 

n = size(infStates,1);

% Weed irregular trials out from inferred states and responses
mb = infStates(:,1); % mean belief
mb(r.irr) = [];

% nb_raw        = infStates(:,1,11); % noise belief (11 for changed as in plot script)
nb_raw = infStates(:,2); 
nb            = tapas_sgm(zscore(nb_raw),1); % noise belief
nb_beta       = be1.*nb; 
nb_beta(r.irr)=[]; 


% Simulate
y(:,1) = mb       + sqrt(zem)*randn(n, 1);
y(:,2) = nb_beta  + sqrt(zes)*randn(n, 1);

return;
